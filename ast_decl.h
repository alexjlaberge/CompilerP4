/* File: ast_decl.h
 * ----------------
 * In our parse tree, Decl nodes are used to represent and
 * manage declarations. There are 4 subclasses of the base class,
 * specialized for declarations of variables, functions, classes,
 * and interfaces.
 */

#ifndef _H_ast_decl
#define _H_ast_decl

#include "ast.h"
#include "ast_type.h"
#include "list.h"

class Identifier;
class Stmt;
class FnDecl;
class InterfaceDecl;
class Location;

//int currLocation = -8;
//static int currDiff = 0;

class Decl : public Node 
{
  protected:
    Identifier *id;
  
  public:
    int currLocation;
    Decl(Identifier *name);
    friend std::ostream& operator<<(std::ostream& out, Decl *d) { return out << d->id; }
    Identifier *GetId() { return id; }
    const char *GetName() { return id->GetName(); }
    
    virtual bool ConflictsWithPrevious(Decl *prev);

    virtual bool IsVarDecl() { return false; } // jdz: could use typeid/dynamic_cast for these
    virtual bool IsClassDecl() { return false; }
    virtual bool IsInterfaceDecl() { return false; }
    virtual bool IsFnDecl() { return false; } 
    virtual bool IsMethodDecl() { return false; }
    virtual bool IsIvarDecl() { return false; }
};

class VarDecl : public Decl 
{
  protected:
    Type *type;
    Location* loc;
    
  public:
    bool isGP;
    int offset;
    VarDecl(Identifier *name, Type *type);
    void Check();
    Type *GetDeclaredType() { return type; }
    bool IsVarDecl() { return true; }
    bool IsIvarDecl();
    void setLocation(Location *nloc) {loc = nloc;}
    Location* getLocation() {return loc;}
    size_t getSize();
    virtual void Emit();
};

class ClassDecl : public Decl 
{
  protected:
    List<Decl*> *members;
    NamedType *extends;
    List<NamedType*> *implements;
    NamedType *cType;
    List<InterfaceDecl*> *convImp;
    // NamedType matching with conv
    List<NamedType*> *interfaceDecl_to_namedType;
    
  public:
    List<const char*> *vTable;
    ClassDecl(Identifier *name, NamedType *extends, 
              List<NamedType*> *implements, List<Decl*> *members);
    void Check();
    bool IsClassDecl() { return true; }
    virtual Scope *PrepareScope();
    List<InterfaceDecl*> *GetImplementedInterfaces() { return convImp; }
    bool IsCompatibleWith(Type *type);
    bool Implements(Type *intf);
    Type *GetDeclaredType() { return cType; } //  used by "this"
    const char *GetClassName() { return id->GetName(); }
    virtual void Emit();
    List<const char*>* getVTable();
    int getNumVars();

    int getSize()
    {
        int temp = 0;
        for(int i = 0; i < members->NumElements(); i++)
        {
            if(dynamic_cast<VarDecl*>(members->Nth(i)))
            {
                temp++;
            }
        }
        return (4* temp) + 4;
    }
};

class InterfaceDecl : public Decl 
{
  protected:
    List<Decl*> *members;
    
  public:
    InterfaceDecl(Identifier *name, List<Decl*> *members);
    void Check();
    bool IsInterfaceDecl() { return true; }
    Scope *PrepareScope();
    bool ClassMeetsObligation(ClassDecl *c);
    virtual void Emit();
};

class FnDecl : public Decl 
{
  protected:
    List<VarDecl*> *formals;
    Type *returnType;
    Stmt *body;
    
  public:
    int size;
    int offset;
    FnDecl(Identifier *name, Type *returnType, List<VarDecl*> *formals);
    void SetFunctionBody(Stmt *b);
    void Check();
    void CheckPrototype();
    bool IsFnDecl() { return true; }
    bool IsMethodDecl();
    bool ConflictsWithPrevious(Decl *prev);
    bool MatchesPrototype(FnDecl *other);
    Type *GetReturnType() {return returnType; }
    List<VarDecl*> *GetFormals() { return formals; }
    virtual void Emit();
    virtual Scope *PrepareScope();
};

#endif
