/* File: ast_expr.cc
 * -----------------
 * Implementation of expression node classes.
 */
#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include <string.h>
#include "errors.h"

Type *EmptyExpr::CheckAndComputeResultType() { return Type::voidType; } 

IntConstant::IntConstant(yyltype loc, int val) : Expr(loc) {
    value = val;
}
Type *IntConstant::CheckAndComputeResultType() { 
    return Type::intType;
}

DoubleConstant::DoubleConstant(yyltype loc, double val) : Expr(loc) {
    value = val;
}
Type *DoubleConstant::CheckAndComputeResultType() { 
    return Type::doubleType;
}

BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc) {
    value = val;
}
Type *BoolConstant::CheckAndComputeResultType() { 
    return Type::boolType;
}

StringConstant::StringConstant(yyltype loc, const char *val) : Expr(loc) {
    Assert(val != NULL);
    value = strdup(val);
}
Type *StringConstant::CheckAndComputeResultType() {
    return Type::stringType;
}
Type *NullConstant::CheckAndComputeResultType() { 
    return Type::nullType;
}

Operator::Operator(yyltype loc, const char *tok) : Node(loc) {
    Assert(tok != NULL);
    strncpy(tokenString, tok, sizeof(tokenString));
}
CompoundExpr::CompoundExpr(Expr *l, Operator *o, Expr *r) 
  : Expr(Join(l->GetLocation(), r->GetLocation())) {
    Assert(l != NULL && o != NULL && r != NULL);
    (op=o)->SetParent(this);
    (left=l)->SetParent(this); 
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Operator *o, Expr *r) 
  : Expr(Join(o->GetLocation(), r->GetLocation())) {
    Assert(o != NULL && r != NULL);
    left = NULL; 
    (op=o)->SetParent(this);
    (right=r)->SetParent(this);
}
void CompoundExpr::ReportErrorForIncompatibleOperands(Type *lhs, Type *rhs) {
    if (!lhs) { //unary op
        ReportError::IncompatibleOperand(op, rhs);
    } else { // binary op
        ReportError::IncompatibleOperands(op, lhs, rhs);
    }
}

bool CompoundExpr::CanDoArithmetic(Type *lhs, Type *rhs) {
    if (lhs && lhs != Type::errorType && rhs != Type::errorType)
	return rhs->IsNumeric() && rhs->IsEquivalentTo(lhs);
    if (!lhs || lhs == Type::errorType)
	return rhs->IsNumeric() || rhs == Type::errorType;
    return rhs != Type::errorType || lhs->IsNumeric();
}


Type *GetResultType(Type *lhs, Type *rhs) {
    Type *lesser = rhs;
    if (lhs) lesser = lesser->LesserType(lhs);
    if (!lesser || !lesser->IsNumeric())
	return Type::errorType;
    return lesser;
}

Type*ArithmeticExpr::CheckAndComputeResultType() {
    Type *lType = left?left->CheckAndComputeResultType():NULL, *rType = right->CheckAndComputeResultType();
    if (!CanDoArithmetic(lType, rType))
	ReportErrorForIncompatibleOperands(lType, rType);
    return GetResultType(lType, rType);
}

Type* RelationalExpr::CheckAndComputeResultType() {
    Type*lhs = left->CheckAndComputeResultType(), *rhs = right->CheckAndComputeResultType();
    if (!CanDoArithmetic(lhs, rhs))
	ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Type* EqualityExpr::CheckAndComputeResultType() {
   Type*lhs = left->CheckAndComputeResultType(), *rhs = right->CheckAndComputeResultType();
    if (!lhs->IsCompatibleWith(rhs) && !rhs->IsCompatibleWith(lhs))
	ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Type* LogicalExpr::CheckAndComputeResultType() {
    Type *lhs = left ?left->CheckAndComputeResultType() :NULL, *rhs = right->CheckAndComputeResultType();
    if ((lhs && !lhs->IsCompatibleWith(Type::boolType)) ||
	  (!rhs->IsCompatibleWith(Type::boolType)))
	ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Type * AssignExpr::CheckAndComputeResultType() {
    Type *lhs = left->CheckAndComputeResultType(), *rhs = right->CheckAndComputeResultType();
    if (!rhs->IsCompatibleWith(lhs)) {
        ReportErrorForIncompatibleOperands(lhs, rhs);
        return Type::errorType;
    }
    return lhs;
}
Type* This::CheckAndComputeResultType() {
    if (!enclosingClass) enclosingClass = FindSpecificParent<ClassDecl>();
   if (!enclosingClass)  
       ReportError::ThisOutsideClassScope(this);
   if (!enclosingClass) return Type::errorType;
   return enclosingClass->GetDeclaredType();
}

   
  
ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this); 
    (subscript=s)->SetParent(this);
}
Type *ArrayAccess::CheckAndComputeResultType() {
    Type *baseT = base->CheckAndComputeResultType();
    if ((baseT != Type::errorType) && !baseT->IsArrayType()) 
        ReportError::BracketsOnNonArray(base);
    if (!subscript->CheckAndComputeResultType()->IsCompatibleWith(Type::intType))
	ReportError::SubscriptNotInteger(subscript);
    return baseT->IsArrayType() ? dynamic_cast<ArrayType*>(baseT)->GetArrayElemType() : Type::errorType;
}

     
FieldAccess::FieldAccess(Expr *b, Identifier *f) 
  : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation()) {
    Assert(f != NULL); // b can be be NULL (just means no explicit base)
    base = b; 
    if (base) base->SetParent(this); 
    (field=f)->SetParent(this);
}


Type* FieldAccess::CheckAndComputeResultType() {
    Type *baseType = base ? base->CheckAndComputeResultType() : NULL;
    Decl *ivar = field->GetDeclRelativeToBase(baseType);
    if (ivar && ivar->IsIvarDecl() && !base) { // add implicit "this"
        base = new This(*field->GetLocation());
        base->SetParent(this);
        baseType = base->CheckAndComputeResultType();
    }
    if (base) {
        if (baseType == Type::errorType)
            return Type::errorType;
        else if (!ivar || !ivar->IsVarDecl()) {
            ReportError::FieldNotFoundInBase(field, baseType);
            return Type::errorType;
        } else {
            ClassDecl *enclosingClass = FindSpecificParent<ClassDecl>(); // check cur scope for compatibility
            Type *withinClass = (enclosingClass? enclosingClass->GetDeclaredType() : NULL);
            if (ivar && (!withinClass|| !withinClass->IsCompatibleWith(baseType))) {
                ReportError::InaccessibleField(field, baseType);
                return Type::errorType;
            } 
        }
    } else if (!ivar || !ivar->IsVarDecl()) {
        ReportError::IdentifierNotDeclared(field, LookingForVariable);
        return Type::errorType;
    }
    return ivar ? (dynamic_cast<VarDecl *>(ivar))->GetDeclaredType() : Type::errorType;
  }


Call::Call(yyltype loc, Expr *b, Identifier *f, List<Expr*> *a) : Expr(loc)  {
    Assert(f != NULL && a != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
    (actuals=a)->SetParentAll(this);
}
// special-case code for length() on arrays... sigh.
Type* Call::CheckAndComputeResultType() {
    Type *baseType = base ? base->CheckAndComputeResultType() : NULL;
    FnDecl *fd = dynamic_cast<FnDecl *>(field->GetDeclRelativeToBase(baseType));
    if (fd && fd->IsMethodDecl() && !base) { // add implicit "this"
        base = new This(*field->GetLocation());
        base->SetParent(this);
        baseType = base->CheckAndComputeResultType();
   }
   List<Type*> aTypes;
    for (int i = 0; i < actuals->NumElements(); i++) 
        aTypes.Append(actuals->Nth(i)->CheckAndComputeResultType());
// jdz cascade, above loop checks actuals before function confirmed.
// what about excess actuals? what if function doesn't exist at all?
    if (baseType && baseType->IsArrayType() && strcmp(field->GetName(), "length") == 0) {
	if (actuals->NumElements() != 0) 
            ReportError::NumArgsMismatch(field, 0, actuals->NumElements());
	return Type::intType;
    }
    if (baseType == Type::errorType) {
	return Type::errorType;
    }
    if (baseType && !fd) { // had receiver, but no field in receiver (not class, wrong name, etc.)
	ReportError::FieldNotFoundInBase(field, baseType);
        return Type::errorType;
    } else if (!fd) { // no base, bad function
	ReportError::IdentifierNotDeclared(field, LookingForFunction);
        return Type::errorType;
    }  

    List<VarDecl*> *formals = fd->GetFormals();
    if (formals->NumElements() != actuals->NumElements()) {
	ReportError::NumArgsMismatch(field, formals->NumElements(), actuals->NumElements());
    }
    for (int i = 0; i < formals->NumElements(); i++) {
	if (i >= actuals->NumElements()) break;
        Type *at = aTypes.Nth(i);
        if (!at->IsCompatibleWith(formals->Nth(i)->GetDeclaredType()))
            ReportError::ArgMismatch(actuals->Nth(i), i+1, at,
                                    formals->Nth(i)->GetDeclaredType());
    }
    return fd->GetReturnType();
}
 

NewExpr::NewExpr(yyltype loc, NamedType *c) : Expr(loc) { 
  Assert(c != NULL);
  (cType=c)->SetParent(this);
}

Type* NewExpr::CheckAndComputeResultType() {
    if (!cType->IsClass()) {
        ReportError::IdentifierNotDeclared(cType->GetId(), LookingForClass);
        return Type::errorType;
    }
    return cType; 
}

NewArrayExpr::NewArrayExpr(yyltype loc, Expr *sz, Type *et) : Expr(loc) {
    Assert(sz != NULL && et != NULL);
    (size=sz)->SetParent(this); 
    (elemType=et)->SetParent(this);
}
Type *NewArrayExpr::CheckAndComputeResultType() {
    Type *st = size->CheckAndComputeResultType();
    if (!st->IsCompatibleWith(Type::intType))
	ReportError::NewArraySizeNotInteger(size);
    elemType->Check();
    if (elemType->IsError())
	return Type::errorType;
    yyltype none;
    return new ArrayType(none, elemType);
}
  

Type *ReadIntegerExpr::CheckAndComputeResultType() { return Type::intType; }
Type *ReadLineExpr::CheckAndComputeResultType() { return Type::stringType; }

void IntConstant::Emit() {
    temps = 1;
    loc = codegen.GenLoadConstant(getVal());
}

void DoubleConstant::Emit() {
        /* TODO */
}

void BoolConstant::Emit() {
        /* TODO */
    temps = 1;
    if(value)
        loc = codegen.GenLoadConstant(1);
    else
        loc = codegen.GenLoadConstant(0);
}

void StringConstant::Emit() {
        /* TODO */
    temps = 1;
    loc = codegen.GenLoadConstant(value);
}

void ArrayAccess::Emit() {
        /* TODO */
        base->Emit();
        subscript->Emit();
        Location *zero = codegen.GenLoadConstant(0);
        Location *check1 = codegen.GenBinaryOp("<", subscript->loc, zero);
        Location *size = codegen.GenLoad(base->loc, -4);
        Location *tmp1 = codegen.GenBinaryOp("<", subscript->loc, size);
        Location *tmp2 = codegen.GenBinaryOp("==", tmp1, zero);
        Location *check2 = codegen.GenBinaryOp("||", check1, tmp2);
        char* go = codegen.NewLabel();
        codegen.GenIfZ(check2, go);
        Location *msg = codegen.GenLoadConstant(
                        "Decaf runtime error: Array subscript out of bounds\\n");
        codegen.GenBuiltInCall(PrintString, msg, nullptr);
        codegen.GenBuiltInCall(Halt, nullptr, nullptr);
        codegen.GenLabel(go);
        Location *four = codegen.GenLoadConstant(4);
        Location *position = codegen.GenBinaryOp("*", four, subscript->loc);
        Location *calculatedPos = codegen.GenBinaryOp("+", base->loc, position);
        loc = calculatedPos;


}

void NewExpr::Emit() {
        //Gen Location for named type
        Location *className = new Location(fpRelative, 0, cType->GetId()->GetName());
        Location *four = codegen.GenLoadConstant(4);
        loc = codegen.GenBuiltInCall(Alloc, four, nullptr);
        Location *tmp = codegen.GenTempVariable();
        codegen.GenAssign(tmp, className);
        codegen.GenStore(loc, tmp);

}

void NewArrayExpr::Emit() {

        /**
         * 1. Compute size
         * 2. If size < 1, runtime error
         * 3. Call builtin Alloc
         */

        size->Emit();
        Location *one = codegen.GenLoadConstant(1);
        loc = codegen.GenBinaryOp("<", size->loc, one);

        char *tmp = codegen.NewLabel();
        codegen.GenIfZ(loc, tmp);

        /* error message */
        Location *msg = codegen.GenLoadConstant(
                        "Decaf runtime error: Array size is <= 0\\n");
        codegen.GenBuiltInCall(PrintString, msg, nullptr);
        codegen.GenBuiltInCall(Halt, nullptr, nullptr);

        codegen.GenLabel(tmp);
        Location *newOne = codegen.GenLoadConstant(1);
        Location *arrSize = codegen.GenBinaryOp("+", newOne, size->loc);
        Location *four = codegen.GenLoadConstant(4);
        Location *modSize = codegen.GenBinaryOp("*", arrSize, four);
        Location *sizeLocation = codegen.GenBuiltInCall(Alloc, modSize, nullptr);
        codegen.GenStore(sizeLocation, size->loc);
        loc = codegen.GenBinaryOp("+", sizeLocation, four);

}

void Call::Emit() {
        /** TODO
         * 1. Make a temporary variable
         * 2. Generate a function call instruction
         * 3. Put the return value in the temporary variable
         */
        if(base == nullptr)
        {
            char* tmp = (char*)malloc(50);
            sprintf(tmp, "_%s", field->GetName());
            if(!strcmp(field->GetName(), "main"))
            {
                loc = codegen.GenLCall(field->GetName(),
                        CheckAndComputeResultType() != Type::voidType);
            }
            else
            {
                loc = codegen.GenLCall(tmp,
                        CheckAndComputeResultType() != Type::voidType);
            }
        }
        else
        {
            //Calculate func offset
            int offset = 0;
            field->Emit();

            //Load the base
            Location *tmp = new Location(fpRelative, 0, field->GetName());
            Location *classLocation = codegen.GenLoad(tmp, offset);
            //Load from base + offset
            Location *fnLocation = codegen.GenLoad(classLocation, offset);
            //Set loc to be the call
            loc = codegen.GenACall(fnLocation,
                        CheckAndComputeResultType() != Type::voidType);
        }

}

void FieldAccess::Emit() {
        //TODO
        if(base == nullptr)
        {
            int location = ((VarDecl*)FindDecl(field))->offset;
            loc = new Location(fpRelative, location, field->GetName());
        }

        else
        {
            offset = ((VarDecl*)FindDecl(field))->offset;
            base->Emit();
            /*if(dynamic_cast<This*>(base))
            {
                Location* tLoc = new Location(fpRelative, 0, "this");
                loc = codegen.GenLoad(tLoc, offset);
            }
            else
            {
                Location *baseLoc = codegen.GenLoad(base->loc, offset);
            }*/

        }

}

void LValue::Emit() {
        /* TODO */
}

void AssignExpr::Emit() {
        left->Emit();
        right->Emit();
        if(dynamic_cast<ArrayAccess*>(right))
        {
            right->loc = codegen.GenLoad(right->loc);
        }
        if(dynamic_cast<FieldAccess*>(right) && ((FieldAccess*)right)->getBase())
        {
            Expr* base = ((FieldAccess*)right)->getBase();
            if(dynamic_cast<This*>(base))
            {
                int offset = ((FieldAccess*)right)->offset;
                Location *tmpThis = new Location(fpRelative, 0, "this");
                right->loc = codegen.GenLoad(tmpThis, offset);
            }
            else
            {
                //Location* baseLoc = codegen.GenLoad(base->loc, )
                int offset = ((FieldAccess*)right)->offset;
                Location *tmpThis = new Location(fpRelative, 0, "this");
                right->loc = codegen.GenLoad(tmpThis, offset);
            }
        }
        if(dynamic_cast<ArrayAccess*>(left))
        {
            codegen.GenStore(left->loc, right->loc);
        }
        else if(dynamic_cast<FieldAccess*>(left) && ((FieldAccess*)left)->getBase())
        {
            int offset = ((FieldAccess*)left)->offset;
            Location *tmpThis = new Location(fpRelative, 0, "this");
            codegen.GenStore(tmpThis, right->loc, offset);
        }
        else
            codegen.GenAssign(left->loc, right->loc);
}

void LogicalExpr::Emit() {
        if(left == nullptr)
        {
            right->Emit();
            Location *loc1 = codegen.GenLoadConstant(0);
            loc = codegen.GenBinaryOp("==", right->loc, loc1);
            return;
        }
        left->Emit();
        right->Emit();
        loc = codegen.GenBinaryOp(op->getChar(), left->loc, right->loc);

}

void EqualityExpr::Emit() {
        left->Emit();
        right->Emit();
        if(!strcmp(op->getChar(), "=="))
            loc = codegen.GenBinaryOp(op->getChar(), left->loc, right->loc);
        else if(!strcmp(op->getChar(), "!="))
        {
            Location *loc1 = codegen.GenBinaryOp("==", left->loc, right->loc);
            Location *loc2 = codegen.GenLoadConstant(0);
            loc = codegen.GenBinaryOp("==", loc1, loc2);
        }
}

void RelationalExpr::Emit() {
        left->Emit();
        right->Emit();
        if(!strcmp(op->getChar(), ">="))
        {
            Location *loc1 = codegen.GenBinaryOp("<", right->loc, left->loc);
            Location *loc2 = codegen.GenBinaryOp("==", left->loc, right->loc);
            loc = codegen.GenBinaryOp("||", loc1, loc2);
        }
        else if(!strcmp(op->getChar(), "<="))
        {
            Location *loc1 = codegen.GenBinaryOp("<", left->loc, right->loc);
            Location *loc2 = codegen.GenBinaryOp("==", left->loc, right->loc);
            loc = codegen.GenBinaryOp("||", loc1, loc2);
        }
        else if(!strcmp(op->getChar(), ">"))
        {
            loc = codegen.GenBinaryOp("<", right->loc, left->loc);
        }
        else
        {
            loc = codegen.GenBinaryOp(op->getChar(), left->loc, right->loc);
        }

}

void ArithmeticExpr::Emit() {
    if(left == nullptr)
    {
        right->Emit();
        Location *loc1 = codegen.GenLoadConstant(0);
        loc = codegen.GenBinaryOp("-", loc1, right->loc);
        return;
    }
    left->Emit();
    right->Emit();

    Location* lLoc = left->loc;
    Location* rLoc = right->loc;

    loc = codegen.GenBinaryOp(op->getChar(), lLoc, rLoc);


}

void EmptyExpr::Emit() {
}

void ReadLineExpr::Emit() {
        loc = codegen.GenBuiltInCall(ReadLine, nullptr, nullptr);
}

void ReadIntegerExpr::Emit() {
        loc = codegen.GenBuiltInCall(ReadInteger, nullptr, nullptr);
}

void NullConstant::Emit() {
        /* TODO */
}


       
