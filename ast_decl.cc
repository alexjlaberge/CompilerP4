/* File: ast_decl.cc
 * -----------------
 * Implementation of Decl node classes.
 */
#include "ast_decl.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "scope.h"
#include "errors.h"
#include "codegen.h"
  
#include <set>

 class FnDecl;
         
Decl::Decl(Identifier *n) : Node(*n->GetLocation()) {
    Assert(n != NULL);
    (id=n)->SetParent(this); 
}

bool Decl::ConflictsWithPrevious(Decl *prev) {
    if (prev == NULL || prev == this)
	return false;
    ReportError::DeclConflict(this, prev);
    return true;
}

VarDecl::VarDecl(Identifier *n, Type *t) : Decl(n) {
    Assert(n != NULL && t != NULL);
    (type=t)->SetParent(this);
}
  
void VarDecl::Check() {
    type->Check();
    if (type->IsError()) type = Type::errorType;
}
bool VarDecl::IsIvarDecl() { return dynamic_cast<ClassDecl*>(parent) != NULL;}

size_t VarDecl::getSize() {
        return type->getSize();
}

ClassDecl::ClassDecl(Identifier *n, NamedType *ex, List<NamedType*> *imp, List<Decl*> *m) : Decl(n) {
    // extends can be NULL, impl & mem may be empty lists but cannot be NULL
    Assert(n != NULL && imp != NULL && m != NULL);     
    extends = ex;
    if (extends) extends->SetParent(this);
    (implements=imp)->SetParentAll(this);
    (members=m)->SetParentAll(this);
    cType = new NamedType(n);
    cType->SetParent(this);
    cType->SetDeclForType(this);
    convImp = NULL;
}

List<const char*>* ClassDecl::getVTable()
{
    List<const char*> *myVTable = new List<const char*>();
    if(extends != nullptr)
    {
        Decl* parentClass = FindDecl(extends->GetId());
        if(dynamic_cast<ClassDecl*>(parentClass))
        {
            List<const char*> *parentVTable = ((ClassDecl*)parentClass)->getVTable();
            for(int i = 0; i < parentVTable->NumElements(); i++)
            {
                myVTable->Append(parentVTable->Nth(i));
            }
        }
    }

    int curr = myVTable->NumElements();
    curr = curr * 4;

    for(int i = 0; i < members->NumElements(); i++)
    {
        Decl* c = members->Nth(i);
        if(dynamic_cast<FnDecl*>(c))
        {

            ((FnDecl*)members->Nth(i))->offset = curr;
                curr = curr + 4;
                char *a = (char*)malloc(50);

                sprintf(a, "_%s.%s", GetName(), members->Nth(i)->GetName());
                //codegen.GenLabel(a);
                //members->Nth(i)->Emit();
                //printf("%s",a);

                //m->Append(a);

		char *func = strdup(members->Nth(i)->GetName());
		int j = 0;
		for (j = 0; j < myVTable->NumElements(); j++)
		{
			const char *vtable_func = strstr(myVTable->Nth(j), ".") + 1;
			if (strcmp(vtable_func, func) == 0)
			{
				myVTable->RemoveAt(j);
				myVTable->InsertAt(a, j);
				j = myVTable->NumElements() - 1;
				break;
			}
		}
		if (j == myVTable->NumElements())
		{
			myVTable->Append(a);
		}

        }
    }
    vTable = myVTable;
    return vTable;

}

int ClassDecl::getNumVars()
{
    int start = 0;
    if(extends != nullptr)
    {
        Decl* p = FindDecl(extends->GetId());
        if(dynamic_cast<ClassDecl*>(p))
        {
            start = ((ClassDecl*)p)->getNumVars();
        }
    }

    for(int i = 0; i < members->NumElements(); i++)
    {
	    if(dynamic_cast<VarDecl*>(members->Nth(i)))
	    {
		    start++;
	    }
    }
    return start;
}

void ClassDecl::Check() {
    List<const char*>* tmpV = getVTable();
    int curr = 0;
    if(extends != nullptr)
    {
        Decl* p = FindDecl(extends->GetId());
        if(dynamic_cast<ClassDecl*>(p))
        {
            curr = ((ClassDecl*)p)->getNumVars();
        }
    }
    curr++;
    curr = curr * 4;
    for(int i = 0; i < members->NumElements(); i++)
    {
        if(dynamic_cast<VarDecl*>(members->Nth(i)))
        {
            ((VarDecl*)members->Nth(i))->offset = curr;
            curr += 4;
        }
    }

    ClassDecl *ext = extends ? dynamic_cast<ClassDecl*>(parent->FindDecl(extends->GetId())) : NULL; 
    if (extends && !ext) {
        ReportError::IdentifierNotDeclared(extends->GetId(), LookingForClass);
        extends = NULL;
    }
    PrepareScope();
    members->CheckAll();
    for (int i = 0; i < members->NumElements(); ++i) {
	FnDecl *m = dynamic_cast<FnDecl*>(FindDecl(members->Nth(i)->GetId()));
	if (m && m == members->Nth(i)) {
	    for (int j = 0; j < convImp->NumElements(); ++j) {
		FnDecl *p = dynamic_cast<FnDecl*>(convImp->Nth(j)->FindDecl(m->GetId(), kShallow));
		if (p && !m->MatchesPrototype(p)) {
		    ReportError::OverrideMismatch(m);
		    break;
		}
	    }
	}
    }
    for (int i = 0; i < convImp->NumElements(); i++) {
        if (!convImp->Nth(i)->ClassMeetsObligation(this))
            ReportError::InterfaceNotImplemented(this, interfaceDecl_to_namedType->Nth(i));
    }
}

// This is not done very cleanly. I should sit down and sort this out. Right now
// I was using the copy-in strategy from the old compiler, but I think the link to
// parent may be the better way now.
Scope *ClassDecl::PrepareScope()
{
    if (nodeScope) return nodeScope;
    nodeScope = new Scope();  
    if (extends) {
        ClassDecl *ext = dynamic_cast<ClassDecl*>(parent->FindDecl(extends->GetId())); 
        if (ext) nodeScope->CopyFromScope(ext->PrepareScope(), this);
    }
    convImp = new List<InterfaceDecl*>;
    interfaceDecl_to_namedType = new List<NamedType*>;
    std::set<InterfaceDecl*> interfaceSet;
    for (int i = 0; i < implements->NumElements(); i++) {
        InterfaceDecl *in = dynamic_cast<InterfaceDecl*>(parent->FindDecl(implements->Nth(i)->GetId()));
	if (in) {
      if (interfaceSet.find(in) == interfaceSet.end()) {
        interfaceSet.insert(in);
        convImp->Append(in);
        interfaceDecl_to_namedType->Append(implements->Nth(i)); 
      }
      else
        ReportError::RepeatedInterface(this, implements->Nth(i));
  }
	else
            ReportError::IdentifierNotDeclared(implements->Nth(i)->GetId(), LookingForInterface);
    }
    members->DeclareAll(nodeScope);

    for (int i = 0; i < members->NumElements(); i++)
    {
            FnDecl *fn = dynamic_cast<FnDecl*>(members->Nth(i));
            if (fn)
            {
                    fn->PrepareScope();
            }
    }

    return nodeScope;
}


bool ClassDecl::IsCompatibleWith(Type *other) {
    if (Implements(other)) return true;
    if (cType->IsEquivalentTo(other)) return true;
    return (extends && extends->IsCompatibleWith(other));
}

bool ClassDecl::Implements(Type *other) {
    if (!other->IsNamedType()) return false;
    Decl *toMatch = dynamic_cast<NamedType*>(other)->GetDeclForType();
    if (!convImp) PrepareScope(); //jdz hack
    for (int i = 0; i < convImp->NumElements(); i++) {
        InterfaceDecl *id = convImp->Nth(i);
        if (id == toMatch) return true;
    }
    return false;
}

InterfaceDecl::InterfaceDecl(Identifier *n, List<Decl*> *m) : Decl(n) {
    Assert(n != NULL && m != NULL);
    (members=m)->SetParentAll(this);
}

void InterfaceDecl::Check() {
    PrepareScope();
    members->CheckAll();
}
  
Scope *InterfaceDecl::PrepareScope() {
    if (nodeScope) return nodeScope;
    nodeScope = new Scope();  
    members->DeclareAll(nodeScope);
    return nodeScope;
}
bool InterfaceDecl::ClassMeetsObligation(ClassDecl *c) {
    for (int i = 0; i < members->NumElements();i++) {
        FnDecl *m = dynamic_cast<FnDecl*>(members->Nth(i));
        FnDecl *found = dynamic_cast<FnDecl*>(c->FindDecl(m->GetId(), kShallow));
        if (!found || (found->GetParent() != c && !found->MatchesPrototype(m))) {
            return false;
	}
    }
    return true;
}
	
FnDecl::FnDecl(Identifier *n, Type *r, List<VarDecl*> *d) : Decl(n) {
    Assert(n != NULL && r!= NULL && d != NULL);
    (returnType=r)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
    if (strcmp(n->GetName(), "main") == 0)
    {
            mainDefined = true;
    }
    currLocation = -8;
}

void FnDecl::SetFunctionBody(Stmt *b) {
    //currLocation = -8; 
    (body=b)->SetParent(this);
    //cout << "END OF FUNC " << currLocation << endl;
}

void FnDecl::Check() {
    Assert(parent != NULL);
    nodeScope = new Scope();
    formals->DeclareAll(nodeScope);
    CheckPrototype();
    int curr;
    if(dynamic_cast<ClassDecl*>(parent))
        curr = 8;
    else
        curr = 4;
    for(int i = 0; i < formals->NumElements(); i++)
    {
        Location *l = new Location(fpRelative, curr, formals->Nth(i)->GetName());
        formals->Nth(i)->setLocation(l);
        formals->Nth(i)->offset = curr;
        curr = curr + 4;
    }
    if (body)
    {
       body->startingOffset = -8;
	   body->Check();
    }
}

void FnDecl::CheckPrototype() {
    returnType->Check();
    if (returnType->IsError()) returnType = Type::errorType;
    formals->CheckAll();
}

bool FnDecl::ConflictsWithPrevious(Decl *prev) {
    if (prev == NULL || prev == this)
	return false;
    // special case error for method override
    if (IsMethodDecl() && prev->IsMethodDecl() && parent != prev->GetParent()) { 
        if (!MatchesPrototype(dynamic_cast<FnDecl*>(prev))) {
            ReportError::OverrideMismatch(this);
            return true;
        }
        return false;
    }
    ReportError::DeclConflict(this, prev);
    return true;
}

bool FnDecl::IsMethodDecl() 
  { return dynamic_cast<ClassDecl*>(parent) != NULL || dynamic_cast<InterfaceDecl*>(parent) != NULL; }

bool FnDecl::MatchesPrototype(FnDecl *other) {
    CheckPrototype();
    other->CheckPrototype();
    if (returnType != Type::errorType && other->returnType != Type::errorType &&
	!returnType->IsEquivalentTo(other->returnType)) return false;
    if (formals->NumElements() != other->formals->NumElements())
        return false;
    for (int i = 0; i < formals->NumElements(); i++) {
	Type* t1 = formals->Nth(i)->GetDeclaredType();
	Type* t2 = other->formals->Nth(i)->GetDeclaredType();
        if (t1 != Type::errorType && t2 != Type::errorType && !t1->IsEquivalentTo(t2))
            return false;
    }
    return true;
}

void VarDecl::Emit() {
        
}

void ClassDecl::Emit() {
        //Emit Functions with different Labels
        //Emit Body of those functions
        //
        //Generate V-Table
        //char *a = (char*)malloc(50);
        List<const char*> *m = new List<const char*>;
        int curr = 0;
        for(int i = 0; i < members->NumElements(); i++)
        {
            //char *a = (char*)malloc(50);

            if(members->Nth(i)->IsFnDecl())
            {
                ((FnDecl*)members->Nth(i))->offset = curr;
                curr = curr + 4;
                char *a = (char*)malloc(50);

                sprintf(a, "_%s.%s", GetName(), members->Nth(i)->GetName());
                //codegen.GenLabel(a);
                members->Nth(i)->Emit();
                //printf("%s",a);

                m->Append(a);
            }
        }
        codegen.GenVTable(GetName(), vTable);
}

void FnDecl::Emit() {
        char temp[32];
        if(dynamic_cast<ClassDecl*>(parent) != nullptr)
        {
            char *a = (char*)malloc(50);
            sprintf(a, "_%s.%s", dynamic_cast<ClassDecl*>(parent)->GetName(),
                            GetName());
            codegen.GenLabel(a);
        }
        else
        {  
            sprintf(temp, "_%s", id->GetName());
            if(!strcmp(id->GetName(), "main"))
                codegen.GenLabel(id->GetName());
            else
                codegen.GenLabel(temp);
        }

        BeginFunc *func = codegen.GenBeginFunc();

        //func->SetFrameSize(body->localSpaceRequired());
        int start = codegen.getTempNum();
        body->Emit();
        int end = codegen.getTempNum();
        int diff = end - start;
        diff = diff * 4;
        //cout << "DIFF: " << diff << endl;
        int vars = 0;
        if(dynamic_cast<StmtBlock*>(body))
            vars = ((StmtBlock*)body)->getNumVars();
        vars = vars * 4;
        int frameSize = vars + diff;
        func->SetFrameSize(frameSize);
        codegen.GenEndFunc();
        codegen.newSpace = 0;
}

void InterfaceDecl::Emit() {
        /* TODO */
}

Scope *FnDecl::PrepareScope()
{
        if (nodeScope)
        {
                return nodeScope;
        }

        nodeScope = new Scope();

        for (int i = 0; i < formals->NumElements(); i++) {
                nodeScope->Declare(formals->Nth(i));
        }

        return nodeScope;
}
