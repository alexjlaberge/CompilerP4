/* File: ast_expr.h
 * ----------------
 * The Expr class and its subclasses are used to represent
 * expressions in the parse tree.  For each expression in the
 * language (add, call, New, etc.) there is a corresponding
 * node class for that construct. 
 */


#ifndef _H_ast_expr
#define _H_ast_expr

#include "ast.h"
#include "ast_stmt.h"
#include "list.h"
#include "codegen.h"

class NamedType; // for new
class Type; // for NewArray
class ClassDecl; // for This
class Location;


class Expr : public Stmt 
{
  public:
    bool isLeft;
    Expr(yyltype loc) : Stmt(loc) {}
    Expr() : Stmt() {}
    void Check() { CheckAndComputeResultType(); }
    virtual Type* CheckAndComputeResultType() = 0;
    Location* loc;
};

/* This node type is used for those places where an expression is optional.
 * We could use a NULL pointer, but then it adds a lot of checking for
 * NULL. By using a valid, but no-op, node, we save that trouble */
class EmptyExpr : public Expr
{
  public:
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class IntConstant : public Expr 
{
  protected:
    int value;
  
  public:
    IntConstant(yyltype loc, int val);
    Type *CheckAndComputeResultType();
    int getVal() {return value;}
    virtual void Emit();
};

class DoubleConstant : public Expr 
{
  protected:
    double value;
    
  public:
    DoubleConstant(yyltype loc, double val);
    Type *CheckAndComputeResultType();
    virtual void Emit();
};

class BoolConstant : public Expr 
{
  protected:
    bool value;
    
  public:
    BoolConstant(yyltype loc, bool val);
    Type *CheckAndComputeResultType();
    virtual void Emit();
};

class StringConstant : public Expr 
{ 
  protected:
    char *value;
    
  public:
    StringConstant(yyltype loc, const char *val);
    Type *CheckAndComputeResultType();
    virtual void Emit();
    
};

class NullConstant: public Expr 
{
  public: 
    NullConstant(yyltype loc) : Expr(loc) {}
    Type *CheckAndComputeResultType();
    virtual void Emit();
};

class Operator : public Node 
{
  protected:
    char tokenString[4];
    
  public:
    Operator(yyltype loc, const char *tok);
    friend std::ostream& operator<<(std::ostream& out, Operator *o) { return out << o->tokenString; }
    const char *str() { return tokenString; }
    char* getChar() {return tokenString;}
 };
 
class CompoundExpr : public Expr
{
  protected:
    Operator *op;
    Expr *left, *right; // left will be NULL if unary
    
  public:
    CompoundExpr(Expr *lhs, Operator *op, Expr *rhs); // for binary
    CompoundExpr(Operator *op, Expr *rhs);             // for unary
    void ReportErrorForIncompatibleOperands(Type *lhs, Type *rhs);
    bool CanDoArithmetic(Type *lhs, Type *rhs);
};

class ArithmeticExpr : public CompoundExpr 
{
  public:
    ArithmeticExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
    ArithmeticExpr(Operator *op, Expr *rhs) : CompoundExpr(op,rhs) {}
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class RelationalExpr : public CompoundExpr 
{
  public:
    RelationalExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class EqualityExpr : public CompoundExpr 
{
  public:
    EqualityExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
    const char *GetPrintNameForNode() { return "EqualityExpr"; }
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class LogicalExpr : public CompoundExpr 
{
  public:
    LogicalExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
    LogicalExpr(Operator *op, Expr *rhs) : CompoundExpr(op,rhs) {}
    const char *GetPrintNameForNode() { return "LogicalExpr"; }
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class AssignExpr : public CompoundExpr 
{
  public:
    AssignExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
    const char *GetPrintNameForNode() { return "AssignExpr"; }
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class LValue : public Expr 
{
  public:
    LValue(yyltype loc) : Expr(loc) {}
    virtual void Emit();
};

class This : public Expr 
{
  protected:
    ClassDecl *enclosingClass;
    
  public:
    This(yyltype loc) : Expr(loc), enclosingClass(NULL)  {}
    Type* CheckAndComputeResultType();

    virtual void Emit();
};

class ArrayAccess : public LValue 
{
  protected:
    Expr *base, *subscript;
    
  public:
    ArrayAccess(yyltype loc, Expr *base, Expr *subscript);
    Type *CheckAndComputeResultType();
    virtual void Emit();
};

/* Note that field access is used both for qualified names
 * base.field and just field without qualification. We don't
 * know for sure whether there is an implicit "this." in
 * front until later on, so we use one node type for either
 * and sort it out later. */
class FieldAccess : public LValue 
{
  protected:
    Expr *base;	// will be NULL if no explicit base
    Identifier *field;
    
  public:
    int offset;
    FieldAccess(Expr *base, Identifier *field); //ok to pass NULL base
    Type* CheckAndComputeResultType();
    bool hasBase(){return (base!=nullptr);}
    Expr* getBase(){return base;}
    Identifier* getField(){return field;}
    virtual void Emit();
};

/* Like field access, call is used both for qualified base.field()
 * and unqualified field().  We won't figure out until later
 * whether we need implicit "this." so we use one node type for either
 * and sort it out later. */
class Call : public Expr 
{
  protected:
    Expr *base;	// will be NULL if no explicit base
    Identifier *field;
    List<Expr*> *actuals;
    
  public:
    Call(yyltype loc, Expr *base, Identifier *field, List<Expr*> *args);
    Decl *GetFnDecl();
    Type *CheckAndComputeResultType();
    virtual void Emit();
};

class NewExpr : public Expr
{
  protected:
    NamedType *cType;
    
  public:
    NewExpr(yyltype loc, NamedType *clsType);
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class NewArrayExpr : public Expr
{
  protected:
    Expr *size;
    Type *elemType;
    
  public:
    NewArrayExpr(yyltype loc, Expr *sizeExpr, Type *elemType);
    Type* CheckAndComputeResultType();
    virtual void Emit();
};

class ReadIntegerExpr : public Expr
{
  public:
    ReadIntegerExpr(yyltype loc) : Expr(loc) {}
    Type *CheckAndComputeResultType();
    virtual void Emit();
};

class ReadLineExpr : public Expr
{
  public:
    ReadLineExpr(yyltype loc) : Expr (loc) {}
    Type *CheckAndComputeResultType();
    virtual void Emit();
};

    
#endif
