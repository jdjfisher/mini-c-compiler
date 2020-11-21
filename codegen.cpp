
// Third party imports
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"

// Standard library imports
#include <deque>

// Application imports
#include "codegen.h"
#include "lexer.h"
#include "ast.h"

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

// Searches all level the scope stack for the given symbol.
Value* findSymbol(Scopes& symbols, std::string name)
{
  // Iterate over scopes of increasing depth.
  for (auto& scope : symbols)
  {
    // Try find the symbol in the scope.
    auto symbol = scope[name];
    if (symbol)
      return symbol;
  }

  return nullptr;
}

Value* BinOpNode::codegen(Scopes& symbols)
{
  // Codegen the operands.
  Value* l_v = left->codegen(symbols);
  Value* r_v = right->codegen(symbols);
  assert(l_v && r_v);

  // Determine the number float operands
  char f = isFloatLL(l_v) + isFloatLL(r_v);

  // Cast one of the operands to a float to match.
  if (f == 1 && ! isFloatLL(l_v))
  {
    l_v = floatCastLL(l_v);
  }
  else if (f == 1 && ! isFloatLL(r_v))
  {
    r_v = floatCastLL(r_v);
  }

  // Codegen the operation for the given operator.
  switch (op.type)
  {
    // Logical operators.
    case OR:
      return builder.CreateSelect(boolCastLL(l_v), getBoolLL(true), boolCastLL(r_v), "or");
    case AND:
      return builder.CreateSelect(boolCastLL(l_v), boolCastLL(r_v), getBoolLL(false), "and");

    // Arithmetic operators.
    case PLUS:
      return builder.CreateBinOp(f ? Instruction::FAdd : Instruction::Add, l_v, r_v, "add");
    case MINUS:
      return builder.CreateBinOp(f ? Instruction::FSub : Instruction::Sub, l_v, r_v, "sub");
    case ASTERIX:
      return builder.CreateBinOp(f ? Instruction::FMul : Instruction::Mul, l_v, r_v, "mul");
    case DIV:
      return builder.CreateBinOp(f ? Instruction::FDiv : Instruction::SDiv, l_v, r_v, "div");
    case MOD:
      return builder.CreateBinOp(f ? Instruction::FRem : Instruction::SRem, l_v, r_v, "mod");

    // Comparison operators.
    case EQ:
      return f ? builder.CreateFCmpOEQ(l_v, r_v, "eq") : builder.CreateICmpEQ(l_v, r_v, "eq"); 
    case NE:
      return f ? builder.CreateFCmpONE(l_v, r_v, "ne") : builder.CreateICmpNE(l_v, r_v, "ne");
    case LE:
      return f ? builder.CreateFCmpOLE(l_v, r_v, "le") : builder.CreateICmpSLE(l_v, r_v, "le");
    case GE:
      return f ? builder.CreateFCmpOGE(l_v, r_v, "ge") : builder.CreateICmpSGE(l_v, r_v, "ge");
    case LT:
      return f ? builder.CreateFCmpOLT(l_v, r_v, "lt") : builder.CreateICmpSLT(l_v, r_v, "lt");
    case GT:
      return f ? builder.CreateFCmpOGT(l_v, r_v, "gt") : builder.CreateICmpSGT(l_v, r_v, "gt");

    default:
      throw SemanticError(op, "invalid binary operator '" + op.lexeme + "'");
  }
};

Value* AssignNode::codegen(Scopes& symbols)
{
  // Codegen the value.
  Value* value = e->codegen(symbols);
  assert(value);

  // Try find the variable in the scope else check the global table.
  Value* variable = findSymbol(symbols, id.lexeme);
  if (!variable) 
    variable = module->getNamedGlobal(id.lexeme); 

  if (!variable) 
    throw SemanticError(id, "undefined variable '" + id.lexeme + "'");

  // Typecheck.
  if (isWiderLL(value->getType(), variable->getType()->getPointerElementType()))
    throw SemanticError(id, "cannot assign wider type to '" + id.lexeme + "'");

  // Emit the store instruction.
  builder.CreateStore(value, variable);

  // Return the value.
  return value;
};

void VarDeclNode::codegen()
{
  // Check that a variable with the name has not already been declared.
  if (module->getNamedGlobal(id.lexeme))
    throw SemanticError(id, "redeclaration of '" + id.lexeme + "'");

  // Get the initial value for the declaration type.
  Constant* init = getTypeDefaultLL(type.type);
  assert(init); 

  // Create the global variable.
  GlobalVariable* global = new GlobalVariable(
    *module,
    getTypeLL(type.type),
    false, 
    GlobalValue::ExternalLinkage,
    init,
    id.lexeme
  );
  assert(global);
}

void VarDeclNode::codegen(Scopes& symbols)
{
  // Check that a variable with the name has not already been declared.
  if (symbols.front()[id.lexeme])
    throw SemanticError(id, "redeclaration of '" + id.lexeme + "'");

  // Create an alloca for this variable.
  Function* function = builder.GetInsertBlock()->getParent();
  IRBuilder<> t_builder (&function->getEntryBlock(), function->getEntryBlock().begin());
  AllocaInst* alloc = t_builder.CreateAlloca(getTypeLL(type.type), nullptr, id.lexeme);
  assert(alloc);

  // Get the initial value for the declaration type.
  Constant* init = getTypeDefaultLL(type.type);
  assert(init); 

  // Emit initial assignment of default value.
  builder.CreateStore(init, alloc);

  // Add the alloca to the symbol table.
  symbols.front()[id.lexeme] = alloc;
};

void BlockStmtNode::codegen(Scopes& symbols)
{
  // Push a new scope level.
  Scope scope;
  symbols.push_front(scope);

  // Generate the block.
  for (const auto& d : decls)
    d->codegen(symbols);
  for (const auto& s : stmts)
    s->codegen(symbols);

  // Pop the same scope level.
  symbols.pop_front();
};

void ReturnStmtNode::codegen(Scopes& symbols)
{
  Function* function = builder.GetInsertBlock()->getParent();

  // Enforce no return value in void functions.
  if (isVoidLL(function->getReturnType()) && e)
    throw SemanticError(tok, "cannot return a value from a void function");

  // Enforce a return value in non-void functions.
  if (!isVoidLL(function->getReturnType()) && !e)
    throw SemanticError(tok, "must return a value in non-void function");

  // Codegen the return value.
  Value* value = e ? e->codegen(symbols) : nullptr /* void */;

  // If there is a return type, check the return value type.
  if (value && isWiderLL(value->getType(), function->getReturnType()))
    throw SemanticError(tok, "cannot return wider type");

  // Emit the return instruction.
  builder.CreateRet(value); 
};

void IfStmtNode::codegen(Scopes& symbols)
{
  Function* function = builder.GetInsertBlock()->getParent();

  // Codegen the condition value.
  Value* cond_v = cond->codegen(symbols);
  assert(cond_v);

  // Cast the value to a boolean.
  cond_v = boolCastLL(cond_v);


  // Create blocks.
  BasicBlock* then_bb = BasicBlock::Create(context, "then", function);
  BasicBlock* else_bb = else_ ? BasicBlock::Create(context, "else", function) : nullptr;
  BasicBlock* join_bb = BasicBlock::Create(context, "join", function);

  // Create conditional branch
  builder.CreateCondBr(cond_v, then_bb, else_ ? else_bb : join_bb);

  // Emit the then block.
  builder.SetInsertPoint(then_bb);
  Scope scope;
  symbols.push_front(scope);
  then->codegen(symbols);
  symbols.pop_front();
  builder.CreateBr(join_bb);

  // Emit the else block.
  if (else_)
  {
    builder.SetInsertPoint(else_bb);
    Scope scope;
    symbols.push_front(scope);
    else_->codegen(symbols);
    symbols.pop_front();
    builder.CreateBr(join_bb);
  }

  // Set the insersion point to after the if construct.
  builder.SetInsertPoint(join_bb);
};

void WhileStmtNode::codegen(Scopes& symbols)
{
  // Make the new basic block for the loop header, inserting after current block.
  Function* function = builder.GetInsertBlock()->getParent();
  BasicBlock* loop_bb = BasicBlock::Create(context, "loop", function);

  // Insert an explicit fall through from the current block to the loop_bb.
  builder.CreateBr(loop_bb);
  builder.SetInsertPoint(loop_bb);

  // Codegen the loop condition.
  Value* cond_v = cond->codegen(symbols);
  assert(cond_v);

  // Cast the value to a boolean.
  cond_v = boolCastLL(cond_v);

  // Create basic blocks for the loop body and the join point.
  BasicBlock* body_bb = BasicBlock::Create(context, "body", function);
  BasicBlock* join_bb = BasicBlock::Create(context, "join", function);

  // Create a conditional branch.
  builder.CreateCondBr(cond_v, body_bb, join_bb);

  // Codegen the loop body.
  builder.SetInsertPoint(body_bb);
  Scope scope;
  symbols.push_front(scope);
  loop->codegen(symbols);
  symbols.pop_front();
  builder.CreateBr(loop_bb);

  // Set the insersion point to after the while construct.
  builder.SetInsertPoint(join_bb);
};

Function* FunSignNode::codegen()
{
  // Create return and arg types.
  Type* returnType = getTypeLL(type.type);
  std::vector<Type*> argTypes;
  for (const auto& param : params)
  {
    argTypes.push_back(getTypeLL(param->getTypeLL()));
  }

  // Create a new function type.
  FunctionType* ft = FunctionType::get(returnType, argTypes, false);
  Function* function = Function::Create(ft, Function::ExternalLinkage, id.lexeme, module.get());

  // Set the name for all the arguments.
  unsigned i = 0;
  for (auto& arg : function->args())
  {
    arg.setName(params[i++]->getName()); 
  }

  return function;
};

void FunDeclNode::codegen()
{
  // Search for a declared extern prototpye.
  Function* function = module->getFunction(sign->getName());

  // Codegen the signature if there is no prototype.
  if (!function)
    function = sign->codegen();

  // Check the function body has not already been defined.
  if (!function->empty())
    throw SemanticError(sign->getId(), "duplicate definition of function '" + sign->getId().lexeme + "'");

  // Create a new basic block to start insertion into.
  BasicBlock* body_bb = BasicBlock::Create(context, "body_bb", function);
  builder.SetInsertPoint(body_bb);

  // Create a new scope and record the function arguments.
  Scope scope;
  
  for (auto& arg : function->args())
  {
    // Create an alloca for this variable.
    IRBuilder<> t_b (&function->getEntryBlock(), function->getEntryBlock().begin());
    AllocaInst* alloc = t_b.CreateAlloca(arg.getType(), nullptr, arg.getName());
    assert(alloc);

    // Store the initial value into the alloca.
    builder.CreateStore(&arg, alloc);

    // Add arguments to variable symbol table.
    scope[arg.getName().str()] = alloc;
  }

  // Push the function root scope;
  Scopes symbols;
  symbols.push_front(scope);

  // Codegen the body of the function.
  body->codegen(symbols);
};

Value* UnaryNode::codegen(Scopes& symbols)
{
  // Codegen the expression value.
  Value* value = expr->codegen(symbols);
  assert(value);

  // Codegen the operation for the given operator
  switch (op.type) 
  {
    case MINUS:
    {
      if (isBoolLL(value))
        throw SemanticError(op, "cannot negate values of type 'bool'");

      // Emit negation operation for the appropriate type
      return isFloatLL(value) ? builder.CreateFNeg(value, "neg") : builder.CreateNeg(value, "neg");
    }
    case NOT:
      // Cast value to bool and then compare it to false
      return builder.CreateICmpEQ(boolCastLL(value), getBoolLL(false), "not");
    
    default:
      throw SemanticError(op, "invalid unary operator '" + op.lexeme + "'");
  }
};

Value* VariableNode::codegen(Scopes& symbols)
{
  // Look this variable up.
  Value *value = findSymbol(symbols, id.lexeme);
  if (!value) 
    value = module->getNamedGlobal(id.lexeme); 

  if (!value) 
    throw SemanticError(id, "undefined variable '" + id.lexeme + "'");

  // Emit the load instruction.
  return builder.CreateLoad(value, id.lexeme.c_str());
};

Value* CallNode::codegen(Scopes& symbols)
{
  // Lookup the function in the global module table.
  Function *function = module->getFunction(id.lexeme);

  if (!function) 
    throw SemanticError(id, "undefined function '" + id.lexeme + "'");
  
  if (function->arg_size() != args.size()) 
    throw SemanticError(id, "incorrect number of args passed to '" + id.lexeme + "'"); 

  // Codegen the call arguments.
  std::vector<Value *> args_v;
  for (unsigned i = 0, e = args.size(); i != e; ++i)
  {
    Value* value = args[i]->codegen(symbols);
    Type* type = function->getFunctionType()->getParamType(i);
    assert(value);

    if (value->getType() != type)
      throw SemanticError(id, "incorrect argument type in call to '" + id.lexeme + "'");

    args_v.push_back(value);
  }

  // Emit the call instruction.
  return builder.CreateCall(function, args_v, "call");
};