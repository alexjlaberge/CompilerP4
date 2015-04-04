/* File: ast_stmt.h
 * ----------------
 * The Stmt class and its subclasses are used to represent
 * statements in the parse tree.  For each statment in the
 * language (for, if, return, etc.) there is a corresponding
 * node class for that construct. 
 */


#ifndef _H_ast_stmt
#define _H_ast_stmt

#include "list.h"
#include "ast.h"
#include <vector>

class Decl;
class VarDecl;
class Expr;
using namespace std;

static vector<char*> breakLabels;
  
class Program : public Node
{
  protected:
     List<Decl*> *decls;
     
  public:
     Program(List<Decl*> *declList);
     void Check();
     virtual void Emit();
};

class Stmt : public Node
{
  public:
     int startingOffset;
     Stmt() : Node() {}
     Stmt(yyltype loc) : Node(loc) {}
     virtual size_t localSpaceRequired();
};

class StmtBlock : public Stmt 
{
  protected:
    List<VarDecl*> *decls;
    List<Stmt*> *stmts;
    
  public:
    StmtBlock(List<VarDecl*> *variableDeclarations, List<Stmt*> *statements);
    void Check();
    virtual void Emit();
    virtual size_t localSpaceRequired();
    int getNumVars();

};

  
class ConditionalStmt : public Stmt
{
  protected:
    Expr *test;
    Stmt *body;
  
  public:
    Stmt* getBody(){return body;}
    ConditionalStmt(Expr *testExpr, Stmt *body);
    void Check();
};

class LoopStmt : public ConditionalStmt 
{
  public:
    LoopStmt(Expr *testExpr, Stmt *body)
            : ConditionalStmt(testExpr, body) {}
};

class ForStmt : public LoopStmt 
{
  protected:
    Expr *init, *step;
  
  public:
    ForStmt(Expr *init, Expr *test, Expr *step, Stmt *body);
    void Check();
    virtual void Emit();
};

class WhileStmt : public LoopStmt 
{
  public:
    WhileStmt(Expr *test, Stmt *body) : LoopStmt(test, body) {}
    virtual void Emit();
};

class IfStmt : public ConditionalStmt 
{
  protected:
    Stmt *elseBody;
  
  public:
    Stmt *getElse() {return elseBody;}
    IfStmt(Expr *test, Stmt *thenBody, Stmt *elseBody);
    void Check();
    virtual void Emit();
};

class BreakStmt : public Stmt 
{
  public:
    BreakStmt(yyltype loc) : Stmt(loc) {}
    void Check();
    virtual void Emit();
};

class ReturnStmt : public Stmt  
{
  protected:
    Expr *expr;
  
  public:
    ReturnStmt(yyltype loc, Expr *expr);
    void Check();
    virtual void Emit();
};

class PrintStmt : public Stmt
{
  protected:
    List<Expr*> *args;
    
  public:
    PrintStmt(List<Expr*> *arguments);
    void Check();
    virtual void Emit();
};


#endif
