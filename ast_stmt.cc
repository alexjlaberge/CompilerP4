/* File: ast_stmt.cc
 * -----------------
 * Implementation of statement node classes.
 */
#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "scope.h"
#include "errors.h"
#include <cassert>
#include <iostream>

using namespace std;


Program::Program(List<Decl*> *d) {
    Assert(d != NULL);
    (decls=d)->SetParentAll(this);
}

void Program::Check() {
    nodeScope = new Scope();
    decls->DeclareAll(nodeScope);
    decls->CheckAll();
    int curr = 0;
    for(int i = 0; i < decls->NumElements(); i++)
    {
        if(dynamic_cast<VarDecl*>(decls->Nth(i)))
        {
            Location *l = new Location(gpRelative, curr, decls->Nth(i)->GetName());
            ((VarDecl*)decls->Nth(i))->setLocation(l);
            ((VarDecl*)decls->Nth(i))->isGP = true;
            ((VarDecl*)decls->Nth(i))->offset = curr;
            curr += 4;

        }
    }
}

StmtBlock::StmtBlock(List<VarDecl*> *d, List<Stmt*> *s) {
    Assert(d != NULL && s != NULL);
    (decls=d)->SetParentAll(this);
    (stmts=s)->SetParentAll(this);
}
void StmtBlock::Check() {
    nodeScope = new Scope();
    decls->DeclareAll(nodeScope);
    decls->CheckAll();
    stmts->CheckAll();
}

ConditionalStmt::ConditionalStmt(Expr *t, Stmt *b) { 
    Assert(t != NULL && b != NULL);
    (test=t)->SetParent(this); 
    (body=b)->SetParent(this);
}

void ConditionalStmt::Check() {
    if (!test->CheckAndComputeResultType()->IsCompatibleWith(Type::boolType))
	ReportError::TestNotBoolean(test);
    body->Check();
}

ForStmt::ForStmt(Expr *i, Expr *t, Expr *s, Stmt *b): LoopStmt(t, b) { 
    Assert(i != NULL && t != NULL && s != NULL && b != NULL);
    (init=i)->SetParent(this);
    (step=s)->SetParent(this);
}
void ForStmt::Check() {
    init->Check();
    step->Check();
    ConditionalStmt::Check();
}

IfStmt::IfStmt(Expr *t, Stmt *tb, Stmt *eb): ConditionalStmt(t, tb) { 
    Assert(t != NULL && tb != NULL); // else can be NULL
    elseBody = eb;
    if (elseBody) elseBody->SetParent(this);
}
void IfStmt::Check() {
    ConditionalStmt::Check();
    if (elseBody) elseBody->Check();
}


void BreakStmt::Check() {
    if (!FindSpecificParent<LoopStmt>())
        ReportError::BreakOutsideLoop(this);
}

ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) { 
    Assert(e != NULL);
    (expr=e)->SetParent(this);
}
void ReturnStmt::Check() {
    Type *got = expr->CheckAndComputeResultType();
    Type *expected =  FindSpecificParent<FnDecl>()->GetReturnType();
    if (!got->IsCompatibleWith(expected))
	ReportError::ReturnMismatch(this, got, expected);
}
  
PrintStmt::PrintStmt(List<Expr*> *a) {    
    Assert(a != NULL);
    (args=a)->SetParentAll(this);
}
void PrintStmt::Check() {
    for (int i = 0; i < args->NumElements();i++) {
	Type *t = args->Nth(i)->CheckAndComputeResultType();
	if (t->IsEquivalentTo(Type::errorType)) continue;
	if (!(t->IsEquivalentTo(Type::intType) || t->IsEquivalentTo(Type::stringType) || t->IsEquivalentTo(Type::boolType)))
	  ReportError::PrintArgMismatch(args->Nth(i),i + 1, t);
    }
}

void Program::Emit() {
    /* pp4: here is where the code generation is kicked off.
     *      The general idea is perform a tree traversal of the
     *      entire program, generating instructions as you go.
     *      Each node can have its own way of translating itself,
     *      which makes for a great use of inheritance and
     *      polymorphism in the node classes.
     */
        if (!mainDefined)
        {
                ReportError::NoMainFound();
		return;
        }

        for(int i = 0; i < decls->NumElements(); i++)
        {
            decls->Nth(i)->Emit();
        }

        codegen.DoFinalCodeGen();
}

void StmtBlock::Emit() {
        for(int i = 0; i < decls->NumElements(); i++)
            decls->Nth(i)->Emit();
        FnDecl* parentFn;
        Node* parents = parent;
        while(!dynamic_cast<FnDecl*>(parents))
        {
            parents = parents->GetParent();
        }
        parentFn = dynamic_cast<FnDecl*>(parents);
        int curr = -8 - (codegen.newSpace*4);
        for(int i = 0; i < decls->NumElements(); i++)
        {
            //cout << decls->Nth(i)->GetName() <<  " "<< parentFn->currLocation << endl; 
            Location *l = new Location(fpRelative, parentFn->currLocation, decls->Nth(i)->GetName());
            decls->Nth(i)->currLocation = parentFn->currLocation;
            decls->Nth(i)->setLocation(l);
            decls->Nth(i)->offset = curr;
            curr = curr - 4;
            parentFn->currLocation = parentFn->currLocation - 4;
        }
        codegen.newSpace += decls->NumElements();
        for(int i = 0; i < stmts->NumElements(); i++)
            stmts->Nth(i)->Emit();
}

int StmtBlock::getNumVars()
{
    int t = decls->NumElements();
    for(int i = 0; i < stmts->NumElements(); i++)
    {
        if(dynamic_cast<StmtBlock*>(stmts->Nth(i)))
        {
            t += ((StmtBlock*)stmts->Nth(i))->getNumVars();
        }
        if(dynamic_cast<ConditionalStmt*>(stmts->Nth(i)))
        {
            Stmt* a = ((ConditionalStmt*)stmts->Nth(i))->getBody();
            if(dynamic_cast<StmtBlock*>(a))
            {
                t += ((StmtBlock*)a)->getNumVars();
            }
        }
        if(dynamic_cast<IfStmt*>(stmts->Nth(i)))
        {
            Stmt* a = ((IfStmt*)stmts->Nth(i))->getElse();
            if(dynamic_cast<StmtBlock*>(a))
            {
                t += ((StmtBlock*)a)->getNumVars();
            }
        }
    }
    return t;
}

void IfStmt::Emit() {
         test->Emit();
         char* temp = codegen.NewLabel();
         char* temp2;
         codegen.GenIfZ(test->loc, temp);
         body->Emit();
         if(elseBody != nullptr)
         {
            temp2 = codegen.NewLabel();
            codegen.GenGoto(temp2);
            codegen.GenLabel(temp);
            elseBody->Emit();
            codegen.GenLabel(temp2);
         }
         else
         {
            codegen.GenLabel(temp);
         }
}

void ForStmt::Emit() {

        init->Emit();
        char* temp1 = codegen.NewLabel();
        char* temp2 = codegen.NewLabel();
        breakLabels.push_back(temp2);
        codegen.GenLabel(temp1);
        test->Emit();
        codegen.GenIfZ(test->loc, temp2);
        body->Emit();
        step->Emit();
        breakLabels.pop_back();
        codegen.GenGoto(temp1);
        codegen.GenLabel(temp2);

}

void WhileStmt::Emit() {
        char* temp1 = codegen.NewLabel();
        char* temp2 = codegen.NewLabel();
        breakLabels.push_back(temp2);
        codegen.GenLabel(temp1);
        test->Emit();
        codegen.GenIfZ(test->loc, temp2);
        body->Emit();
        breakLabels.pop_back();
        codegen.GenGoto(temp1);
        codegen.GenLabel(temp2);
}

void PrintStmt::Emit() {
        Location *loc1;
        for(int i = 0; i < args->NumElements(); i++)
        {
            args->Nth(i)->Emit();
            if(dynamic_cast<ArrayAccess*>(args->Nth(i)))
            {
                args->Nth(i)->loc = codegen.GenLoad(args->Nth(i)->loc);
            }
            if(args->Nth(i)->CheckAndComputeResultType() == Type::boolType)
            {
                codegen.GenBuiltInCall(PrintBool, args->Nth(i)->loc, nullptr);
            }
            else if(args->Nth(i)->CheckAndComputeResultType() == Type::stringType)
            {
                codegen.GenBuiltInCall(PrintString, args->Nth(i)->loc, nullptr);
            }
            else if(args->Nth(i)->CheckAndComputeResultType() == Type::intType)
            {
                codegen.GenBuiltInCall(PrintInt, args->Nth(i)->loc, nullptr);
            }
        }
}

void ReturnStmt::Emit() {
        expr->Emit();
        /*if(dynamic_cast<FieldAccess*>(expr) && ((FieldAccess*)expr)->getBase())
        {
            int offset = ((FieldAccess*)expr)->offset;
            Location *tmpThis = new Location(fpRelative, 0, "this");
            expr->loc = codegen.GenLoad(tmpThis, offset);
        }*/
        codegen.GenReturn(expr->loc);
}

void BreakStmt::Emit() {
        codegen.GenGoto(breakLabels[breakLabels.size()-1]);
}

size_t Stmt::localSpaceRequired() {
        /* This should never be called */
        assert(0);
        return 0;
}

size_t StmtBlock::localSpaceRequired() {
        size_t bytes = 0;

        for (int i = 0; i < decls->NumElements(); i++) {
                bytes += decls->Nth(i)->getSize();
        }

        return bytes;
}


