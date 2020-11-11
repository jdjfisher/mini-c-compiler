
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

Value* boolCastLL(Value* value)
{
  bool f = value->getType() == getTypeLL(FLOAT_TOK);
  return f ? builder.CreateFCmpONE(value, getFloatLL(0.0), "cond") : builder.CreateICmpNE(value, getBoolLL(false), "cond");
}