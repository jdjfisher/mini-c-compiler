
#pragma once

// Third party imports
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

// Standard library imports
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <utility>
#include <vector>
#include <exception>
#include <assert.h> 
#include <sstream>

// Application imports
#include "lexer.h"
#include "codegen.h"

// Namespaces
using namespace llvm;

// Aliases
using SymbolTable = std::map<std::string, AllocaInst*>;


extern LLVMContext context;
extern IRBuilder<> builder;
extern std::unique_ptr<Module> module;


//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

class Node 
{
  public:
    virtual ~Node() {}
    virtual std::string to_string(std::string indent = "") const;
};

class ExprNode : public Node 
{
  public:
    virtual Value* codegen(SymbolTable& symbols) = 0;
};

class BinOpNode : public ExprNode 
{
  private:
    std::unique_ptr<ExprNode> left;
    std::unique_ptr<ExprNode> right;
    TOKEN op;

  public:
    BinOpNode(
      std::unique_ptr<ExprNode> left, std::unique_ptr<ExprNode> right, TOKEN op
    ) : left(std::move(left)), right(std::move(right)), op(op)
    {}
    virtual Value* codegen(SymbolTable& symbols) override
    {
      // Codegen the operands.
      Value* l_v = left->codegen(symbols);
      Value* r_v = right->codegen(symbols);
      assert(l_v && r_v);

      // If there is a float operand cast the other operand if necessary  
      char f = isFloatLL(l_v) + isFloatLL(r_v);

      if (f && ! isFloatLL(l_v))
      {
        l_v = floatCastLL(l_v);
      }
      else if (f && ! isFloatLL(r_v))
      {
        r_v = floatCastLL(r_v);
      }
      
      // Codegen the operation for the given operator
      switch (op.type)
      {
        // Logical operators
        case OR:
          return builder.CreateSelect(boolCastLL(l_v), getBoolLL(true), boolCastLL(r_v), "or");
        case AND:
          return builder.CreateSelect(boolCastLL(l_v), boolCastLL(r_v), getBoolLL(false), "and");

        // Arithmetic operators
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

        // Comparison operators
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
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bin_op> " + op.lexeme + "\n"
        + left->to_string(indent + "  ") + right->to_string(indent + "  ");
    };
};

class AssignNode : public ExprNode
{
  private:
    TOKEN id;
    std::unique_ptr<ExprNode> e;

  public:
    AssignNode(TOKEN id, std::unique_ptr<ExprNode> e) : id(id), e(std::move(e)) {}
    virtual Value* codegen(SymbolTable& symbols) override
    {
      // Codegen the value.
      Value* value = e->codegen(symbols);
      assert(value);

      // Try find the variable in the scope else check the global table.
      Value* variable = symbols[id.lexeme];
      if (!variable) 
        variable = module->getNamedGlobal(id.lexeme); 

      if (!variable) 
        throw SemanticError(id, "undefined variable '" + id.lexeme + "'");

      // if (value->getType() != variable->getType())
      //   throw SemanticError(id, "incorrect type assigned to '" + id.lexeme + "'");

      // Emit the store instruction.
      builder.CreateStore(value, variable);

      // Return the value.
      return value;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<assign> " + id.lexeme + "\n" + e->to_string(indent + "  ");
    };
};

class DeclNode : public Node
{
  public:
    virtual void codegen() = 0;
};

class VarDeclNode : public DeclNode 
{
  private:
    TOKEN type;
    TOKEN id;

  public:
    VarDeclNode(TOKEN type, TOKEN id) : type(type), id(id) {}
    virtual void codegen() override
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
    void codegen(SymbolTable& symbols)
    {
      // Check that a variable with the name has not already been declared.
      if (symbols[id.lexeme])
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
      symbols[id.lexeme] = alloc;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<var_decl> " + type.lexeme + " " + id.lexeme + "\n";
    };
};

class StmtNode : public Node
{
  public:
    virtual void codegen(std::map<std::string, AllocaInst *> &symbols) = 0;
};

class BlockStmtNode : public StmtNode 
{
  private:
    std::vector<std::unique_ptr<VarDeclNode>> decls;
    std::vector<std::unique_ptr<StmtNode>> stmts;

  public:
    BlockStmtNode(
      std::vector<std::unique_ptr<VarDeclNode>> decls,
      std::vector<std::unique_ptr<StmtNode>> stmts
    ) : decls(std::move(decls)), stmts(std::move(stmts))
    {}
    virtual void codegen(SymbolTable& symbols) override
    {
      for (const auto& d : decls)
        d->codegen(symbols);

      for (const auto& s : stmts)
        s->codegen(symbols);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<block_stmt>\n";

      for (const auto& d : decls)
        str += d->to_string(indent + "  ");

      for (const auto& s : stmts)
        str += s->to_string(indent + "  ");
      
      return str;   
    };
};

class ExprStmtNode : public StmtNode
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ExprStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual void codegen(SymbolTable& symbols) override
    {
      e->codegen(symbols);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<expr_stmt>\n";
      if (e) str += e->to_string(indent + "  ");
      return str;
    };
};

class ReturnStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ReturnStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual void codegen(SymbolTable& symbols) override
    {
      Function* function = builder.GetInsertBlock()->getParent();

      // Codegen the return value.
      Value* value = e ? e->codegen(symbols) : nullptr /* void */;

      // Determine the return type.
      Type* type = value ? value->getType() : getTypeLL(VOID_TOK);

      // Typecheck the return type.
      if (type != function->getReturnType())
        throw SemanticError("incorrect return type");

      // Emit the return instruction.
      builder.CreateRet(value); 
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<return_stmt>\n";
      if (e) str += e->to_string(indent + "  ");
      return str;
    };
};

class IfStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<BlockStmtNode> then;
    std::unique_ptr<BlockStmtNode> else_;

  public:
    IfStmtNode(
      std::unique_ptr<ExprNode> cond, 
      std::unique_ptr<BlockStmtNode> then, 
      std::unique_ptr<BlockStmtNode> else_
    ) : cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) 
    {}
    virtual void codegen(SymbolTable& symbols) override
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
      builder.CreateCondBr(cond_v, then_bb, else_bb ? else_bb : join_bb);

      // Emit the then block.
      builder.SetInsertPoint(then_bb);
      then->codegen(symbols);
      builder.CreateBr(join_bb);
 
      // Emit the else block.
      if (else_)
      {
        builder.SetInsertPoint(else_bb);
        else_->codegen(symbols);
        builder.CreateBr(join_bb);
      }

      // Emit the join block.
      builder.SetInsertPoint(join_bb);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<if_stmt>\n";
      str += cond->to_string(indent + "  ");
      str += then->to_string(indent + "  ");
      str += else_->to_string(indent + "  ");
      return str;
    };
};

class WhileStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StmtNode> loop;

  public:
    WhileStmtNode(
      std::unique_ptr<ExprNode> cond, 
      std::unique_ptr<StmtNode> loop
    ) : cond(std::move(cond)), loop(std::move(loop)) 
    {}
    virtual void codegen(SymbolTable& symbols) override
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
      loop->codegen(symbols);
      builder.CreateBr(loop_bb);

      // Set the insertion point after the loop.
      builder.SetInsertPoint(join_bb);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<while_stmt>\n";
      str += cond->to_string(indent + "  ");
      str += loop->to_string(indent + "  ");
      return str;
    };
};

class ParamNode : public Node 
{
  private:
    TOKEN type;
    TOKEN id;

  public:
    ParamNode(TOKEN type, TOKEN id) : type(type), id(id) {}
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<param> " + type.lexeme + " " + id.lexeme + "\n";
    };
    std::string getName() { return id.lexeme; };
    int getTypeLL() { return type.type; };
};

class FunSignNode : public Node 
{
  private:
    TOKEN type;
    TOKEN id;
    std::vector<std::unique_ptr<ParamNode>> params;

  public:
    FunSignNode(
      TOKEN type, 
      TOKEN id, 
      std::vector<std::unique_ptr<ParamNode>> params
    ) : type(type), id(id), params(std::move(params)) 
    {}
    TOKEN getId() const { return id; };
    Function* codegen()
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

      // Set names for all arguments
      unsigned i = 0;
      for (auto& arg : function->args())
      {
        arg.setName(params[i++]->getName()); 
      }

      return function;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<fun_sign> " + type.lexeme + " " 
        + id.lexeme + "\n";
        
      indent += "  ";
      str += indent + "<params>\n";

      for (const auto& param : params)
        str += param->to_string(indent + "  ");

      return str;
    };
    std::string getName() { return id.lexeme; };
};

class FunDeclNode : public DeclNode 
{
  private:
    std::unique_ptr<FunSignNode> sign;
    std::unique_ptr<BlockStmtNode> body;

  public:
    FunDeclNode(
      std::unique_ptr<FunSignNode> sign, 
      std::unique_ptr<BlockStmtNode> body
    ) : sign(std::move(sign)), body(std::move(body)) 
    {}
    virtual void codegen() override
    {
      // First, check for an existing function from a previous 'extern' declaration.
      Function* function = module->getFunction(sign->getName());

      // 
      if (!function)
        function = sign->codegen();

      if (!function->empty())
        throw SemanticError(sign->getId(), "duplicate definition of function '" + sign->getId().lexeme + "'");

      // Create a new basic block to start insertion into.
      BasicBlock* body_bb = BasicBlock::Create(context, "body_bb", function);
      builder.SetInsertPoint(body_bb);

      // Record the function arguments in the map.
      SymbolTable symbols;
      
      for (auto& arg : function->args())
      {
        // Create an alloca for this variable.
        IRBuilder<> t_b (&function->getEntryBlock(), function->getEntryBlock().begin());
        AllocaInst* alloc = t_b.CreateAlloca(arg.getType(), nullptr, arg.getName());
        assert(alloc);

        // Store the initial value into the alloca.
        builder.CreateStore(&arg, alloc);

        // Add arguments to variable symbol table.
        symbols[arg.getName().str()] = alloc;
      }

      // Codegen the body of the function.
      body->codegen(symbols);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return sign->to_string(indent) + body->to_string(indent + "  ");
    };
};


class ProgramNode : public Node 
{
  private:
    std::vector<std::unique_ptr<FunSignNode>> externs;
    std::vector<std::unique_ptr<DeclNode>> decls;

  public:
    ProgramNode(
      std::vector<std::unique_ptr<FunSignNode>> externs,
      std::vector<std::unique_ptr<DeclNode>> decls
    ) : externs(std::move(externs)), decls(std::move(decls)) 
    {}
    void codegen()
    {
      for (const auto& e : externs)
        e->codegen();

      for (const auto& d : decls)
        d->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<program>\n";
      str += indent + "  <externs>\n";

      for (const auto& e : externs)
        str += e->to_string(indent + "    ");
      
      str += indent + "  <decls>\n";

      for (const auto& d : decls)
        str += d->to_string(indent + "    ");
      
      return str;
    };
};

class UnaryNode : public ExprNode 
{
  private:
    TOKEN op;
    std::unique_ptr<ExprNode> expr;

  public:
    UnaryNode(TOKEN op, std::unique_ptr<ExprNode> expr) : op(op), expr(std::move(expr)) {}
    virtual Value* codegen(SymbolTable& symbols) override
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
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<unary>" + op.lexeme + "\n" + expr->to_string(indent + "  ");
    };
};

class VariableNode : public ExprNode 
{
  private:
    TOKEN id;

  public:
    VariableNode(TOKEN id): id(id) {}
    virtual Value* codegen(SymbolTable& symbols) override
    {
      // Look this variable up.
      Value *value = symbols[id.lexeme];
      if (!value) 
        value = module->getNamedGlobal(id.lexeme); 

      if (!value) 
        throw SemanticError(id, "undefined variable '" + id.lexeme + "'");

      // Emit the load instruction.
      return builder.CreateLoad(value, id.lexeme.c_str());
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<variable> " + id.lexeme + "\n";
    };
};

class CallNode : public ExprNode 
{
  private:
    TOKEN id;
    std::vector<std::unique_ptr<ExprNode>> args;

  public:
    CallNode(
      TOKEN id, 
      std::vector<std::unique_ptr<ExprNode>> args
    ) : id(id), args(std::move(args)) 
    {}
    virtual Value* codegen(SymbolTable& symbols) override
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
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str =  indent + "<call> " + id.lexeme + "\n";

      for (const auto& arg : args)
        str += arg->to_string(indent + "  ");

      return str; 
    };
};

class FloatNode : public ExprNode 
{
  private:
    float value;

  public:
    FloatNode(TOKEN tok)
    {
      value = std::stof(tok.lexeme); 
    }
    virtual Value* codegen(SymbolTable& symbols) override
    {
      return getFloatLL(value);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<float> " + std::to_string(value) + "\n";
    };
};

class IntNode : public ExprNode 
{
  private:
    int value;

  public:
    IntNode(TOKEN tok)
    {
      value = std::stoi(tok.lexeme); 
    }
    virtual Value* codegen(SymbolTable& symbols) override
    {
      return getIntLL(value);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<int> " + std::to_string(value) + "\n";
    };
};

class BoolNode : public ExprNode 
{
  private:
    bool value;

  public:
    BoolNode(TOKEN tok)
    {
      value = tok.lexeme == "true";
    }
    virtual Value* codegen(SymbolTable& symbols) override
    {
      return getBoolLL(value);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bool> " + std::to_string(value) + "\n";
    };
};
