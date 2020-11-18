
// Third party imports
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"

// Application imports
#include "lexer.h"

// Namespaces
using namespace llvm;


extern LLVMContext context;
extern IRBuilder<> builder;
extern std::unique_ptr<Module> module;


Type* getTypeLL(int type)
{
  switch (type)
  {
    case VOID_TOK:
      return Type::getVoidTy(context);
    case FLOAT_TOK:
      return Type::getFloatTy(context);
    case INT_TOK:
      return Type::getIntNTy(context, 32); 
    case BOOL_TOK:
      return Type::getIntNTy(context, 1); 
    default:
      return nullptr;
  }
}

Constant* getFloatLL(float value)
{
  return ConstantFP::get(getTypeLL(FLOAT_TOK), value);
}

Constant* getIntLL(int value)
{
  return ConstantInt::get(getTypeLL(INT_TOK), value, true);
}

Constant* getBoolLL(bool value)
{
  return ConstantInt::get(getTypeLL(BOOL_TOK), int(value), false);
}

Constant* getTypeDefaultLL(int type)
{
  switch (type)
  {
    case FLOAT_TOK:
      return getFloatLL(0.0f);
    case INT_TOK:
      return getIntLL(0);
    case BOOL_TOK:
      return getBoolLL(false);
    default:
      return nullptr;
  }
}

bool isBoolLL(Type* type)
{
  return type == getTypeLL(BOOL_TOK);
}

bool isBoolLL(Value* value)
{
  return isBoolLL(value->getType());
}

bool isIntLL(Type* type)
{
  return type == getTypeLL(INT_TOK);
}

bool isIntLL(Value* value)
{
  return isIntLL(value->getType());
}

bool isFloatLL(Type* type)
{
  return type == getTypeLL(FLOAT_TOK);
}

bool isFloatLL(Value* value)
{
  return isFloatLL(value->getType());
}

bool isVoidLL(Type* type)
{
  return type == getTypeLL(VOID_TOK);
}

Value* boolCastLL(Value* value)
{
  // Emit cast operation for the appropriate type
  return isFloatLL(value) ? builder.CreateFCmpONE(value, getFloatLL(0.0), "cast") 
                          : builder.CreateICmpNE(value, getBoolLL(false), "cast");
}

Value* floatCastLL(Value* value)
{
  return isFloatLL(value) ? 
    value : builder.CreateCast(Instruction::SIToFP, value, getTypeLL(FLOAT_TOK), "cast");
}

// Determines whether t1 is a wider type than t2.
bool isWiderLL(Type* t1, Type* t2)
{
  // t1 is the widest type but t2 is not.
  if (isFloatLL(t1) && !isFloatLL(t2))
    return true;
  
  // t1 is the second-widest type but t2 is not.
  if (isIntLL(t1) && !isFloatLL(t2) && !isIntLL(t2))
    return true;
  
  // t1 cannot be wider.
  return false;
}