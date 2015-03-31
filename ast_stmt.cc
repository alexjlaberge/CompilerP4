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
        }

        for(int i = 0; i < decls->NumElements(); i++)
        {
            cout << "cum" << flush;
            decls->Nth(i)->Emit();
        }

        codegen.DoFinalCodeGen();
}

void StmtBlock::Emit() {
        /* TODO */
        for(int i = 0; i < decls->NumElements(); i++)
            decls->Nth(i)->Emit();
        for(int i = 0; i < stmts->NumElements(); i++)
            stmts->Nth(i)->Emit();
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

        /* TODO */
}

void ForStmt::Emit() {
        /* TODO */
}

void WhileStmt::Emit() {
        /* TODO */
}

void PrintStmt::Emit() {
        /* TODO */
        Location *loc1;
        for(int i = 0; i < args->NumElements(); i++)
        {
            args->Nth(i)->Emit();
            //codegen->args->Nth(i)->loc
        }
}

void ReturnStmt::Emit() {
        /* TODO */
        expr->Emit();
        codegen.GenReturn(expr->loc);
}

void BreakStmt::Emit() {
        /* TODO */
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


