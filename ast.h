
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
#include "llvm/IR/Verifier.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

// Standard library imports
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

// Application imports
#include "lexer.h"

// Namespaces
using namespace llvm;

extern LLVMContext context;
extern IRBuilder<> builder;
extern std::unique_ptr<Module> module;
static std::map<std::string, Value *> namedValues;


static Type* getTypeLL(int type)
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

static Constant* getFloatLL(float value)
{
  return ConstantFP::get(getTypeLL(FLOAT_TOK), value);
}

static Constant* getIntLL(int value)
{
  return ConstantInt::get(getTypeLL(INT_TOK), value, true);
}

static Constant* getBoolLL(bool value)
{
  return ConstantInt::get(getTypeLL(BOOL_TOK), int(value), false);
}

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// Node - Base class for all AST nodes.
class Node 
{
  public:
    virtual ~Node() {}
    virtual std::string to_string(std::string indent = "") const;
};

// ExprNode - Class for ...
class ExprNode : public Node 
{
  public:
    virtual Value* codegen() = 0;
};

// FactorNode - Class for ...
class BinOpNode : public ExprNode 
{
  private:
    std::unique_ptr<ExprNode> l;
    std::unique_ptr<ExprNode> r;
    TOKEN op;

  public:
    BinOpNode(
      std::unique_ptr<ExprNode> l, std::unique_ptr<ExprNode> r, TOKEN op
    ) : l(std::move(l)), r(std::move(r)), op(op) {}
    virtual Value* codegen() override
    {
      Value* l_v = l->codegen();
      Value* r_v = r->codegen();
      if (!l_v || !r_v) return nullptr;

      switch (op.type) 
      {
        case OR:
          return builder.CreateSelect(l_v, getBoolLL(true), r_v);
        case AND:
          return builder.CreateSelect(l_v, r_v, getBoolLL(false));
        case PLUS:
          return builder.CreateFAdd(l_v, r_v, "add");
        case MINUS:
          return builder.CreateFSub(l_v, r_v, "sub");
        case ASTERIX:
          return builder.CreateFMul(l_v, r_v, "mul");
        case DIV:
          return builder.CreateFDiv(l_v, r_v, "div");
        case MOD:
          return builder.CreateFRem(l_v, r_v, "rem");
        case EQ:
          return builder.CreateFCmpUEQ(l_v, r_v, "eq");
        case NE:
          return builder.CreateFCmpUNE(l_v, r_v, "ne");
        case LE:
          return builder.CreateFCmpULE(l_v, r_v, "le");
        case GE:
          return builder.CreateFCmpUGE(l_v, r_v, "ge");
        case LT:
          return builder.CreateFCmpULT(l_v, r_v, "lt");
        case GT:
          return builder.CreateFCmpUGT(l_v, r_v, "gt");
        default:
          return nullptr; // invalid binary operator
      }
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bin_op> " + op.lexeme + "\n"
        + l->to_string(indent + "  ") + r->to_string(indent + "  ");
    };
};

// AssignNode - Class for ...
class AssignNode : public ExprNode
{
  private:
    TOKEN id;
    std::unique_ptr<ExprNode> e;

  public:
    AssignNode(TOKEN id, std::unique_ptr<ExprNode> e) : id(id), e(std::move(e)) {}
    virtual Value* codegen() override
    {
      return nullptr;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<assign> " + id.lexeme + "\n" + e->to_string(indent + "  ");
    };
};

// VarDeclNode - Class for ...
class VarDeclNode : public Node 
{
  private:
    TOKEN type;
    TOKEN id;

  public:
    VarDeclNode(TOKEN type, TOKEN id) : type(type), id(id) {}
    void codegen()
    {
      // ...
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<var_decl> " + type.lexeme + " " + id.lexeme + "\n";
    };
};

// LocalDeclsNode - Class for ...
class LocalDeclsNode : public Node 
{
  private:
    std::vector<std::unique_ptr<VarDeclNode>> decls;

  public:
    LocalDeclsNode(std::vector<std::unique_ptr<VarDeclNode>> decls) : decls(std::move(decls)) {}
    void codegen() 
    {
      for (const auto& decl : decls)
        decl->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<local_decls>\n";
      for (const auto& d : decls)
      {
        str += d->to_string(indent + "  ");
      }
      return str;  
    };
};

// StmtNode - Class for ...
class StmtNode : public Node
{
  public:
    virtual void codegen() = 0;
};

// StmtListNode - Class for ...
class StmtListNode : public Node 
{
  private:
    std::vector<std::unique_ptr<StmtNode>> stmts;

  public:
    StmtListNode(std::vector<std::unique_ptr<StmtNode>> stmts) : stmts(std::move(stmts)) {}
    void codegen()
    {
      for (const auto& stmt : stmts)
        stmt->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<stmt_list>\n";
      for (const auto& stmt : stmts)
      {
        str += stmt->to_string(indent + "  ");
      }
      return str;  
    };
};

// BlockStmtNode - Class for ...
class BlockStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<LocalDeclsNode> lds;
    std::unique_ptr<StmtListNode> sl;

  public:
    BlockStmtNode(
      std::unique_ptr<LocalDeclsNode> lds,
      std::unique_ptr<StmtListNode> sl
    ) : lds(std::move(lds)), sl(std::move(sl))
    {}
    virtual void codegen() override
    {
      lds->codegen();
      sl->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<block_stmt>\n";
      str += lds->to_string(indent + "  ");
      str += sl->to_string(indent + "  ");
      return str;   
    };
};

// ExprStmtNode - Class for ...
class ExprStmtNode : public StmtNode // TODO: remove node
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ExprStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual void codegen() override
    {
      e->codegen(); // ...
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<expr_stmt>\n";
      if (e) str += e->to_string(indent + "  ");
      return str;
    };
};

// ReturnStmtNode - Class for ...
class ReturnStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ReturnStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual void codegen() override
    {
      builder.CreateRet(e->codegen());
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<return_stmt>\n";
      if (e) str += e->to_string(indent + "  ");
      return str;
    };
};

// IfStmtNode - Class for ...
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
    virtual void codegen() override
    {
      Value* cond_v = cond->codegen();

      // Convert condition to a bool by comparing non-equal to 0.0.
      cond_v = builder.CreateFCmpONE(cond_v, getFloatLL(0.0f), "ifcond");

      Function* function = builder.GetInsertBlock()->getParent();

      // Create blocks.
      BasicBlock* then_bb = BasicBlock::Create(context, "then", function);
      BasicBlock* else_bb = else_ ? BasicBlock::Create(context, "else", function) : nullptr;
      BasicBlock* join_bb = BasicBlock::Create(context, "join", function);

      // Create conditional branch
      builder.CreateCondBr(cond_v, then_bb, else_bb ? else_bb : join_bb);

      // Emit then value.
      builder.SetInsertPoint(then_bb);
      then->codegen();
      builder.CreateBr(join_bb);
 
      // Emit else block.
      if (else_)
      {
        builder.SetInsertPoint(else_bb);
        else_->codegen();
        builder.CreateBr(join_bb);
      }

      // Emit join block.
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

// WhileStmtNode - Class for ...
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
    virtual void codegen() override
    {
      // Make the new basic block for the loop header, inserting after current block.
      Function* function = builder.GetInsertBlock()->getParent();
      BasicBlock* loop_bb = BasicBlock::Create(context, "loop", function);

      // Insert an explicit fall through from the current block to the loop_bb.
      builder.CreateBr(loop_bb);
      builder.SetInsertPoint(loop_bb);

      // Generate code for the loop condition.
      Value* cond_v = cond->codegen();

      // Create basic blocks for the loop body and the join point.
      BasicBlock* body_bb = BasicBlock::Create(context, "body", function);
      BasicBlock* join_bb = BasicBlock::Create(context, "join", function);

      // Create a conditional branch.
      builder.CreateCondBr(cond_v, body_bb, join_bb);

      // 
      builder.SetInsertPoint(body_bb);
      loop->codegen();
      builder.CreateBr(loop_bb);

      // 
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

// ParamNode - Class for ...
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

// FunSignNode - Class for ...
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
      FunctionType *ft = FunctionType::get(returnType, argTypes, false);
      Function *f = Function::Create(ft, Function::ExternalLinkage, id.lexeme, module.get());

      // Set names for all arguments.
      unsigned i = 0;
      for (auto& arg : f->args())
      {
        arg.setName(params[i++]->getName()); 
      }

      return f;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<fun_sign> " + type.lexeme + " " 
        + id.lexeme + "\n";
        
      indent += "  ";
      str += indent + "<params>\n";

      for (const auto& param : params)
      {
        str += param->to_string(indent + "  ");
      }
      return str;
    };
    std::string getName() { return id.lexeme; };
};

// FunDeclNode - Class for ...
class FunDeclNode : public Node 
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
    Function* codegen()
    {
      // First, check for an existing function from a previous 'extern' declaration.
      Function* f = module->getFunction(sign->getName());

      if (!f)
      {
        f = sign->codegen();
        if (!f) return nullptr;
      }

      if (!f->empty()) return nullptr; // Function cannot be redefined.

      // Create a new basic block to start insertion into.
      BasicBlock* body_bb = BasicBlock::Create(context, "body_bb", f);
      builder.SetInsertPoint(body_bb);
      body->codegen();

      // Record the function arguments in the namedValues map.
      namedValues.clear();
      for (const auto &arg : f->args())
      {
        namedValues[arg.getName()] = (Value*) &arg;
      }

      // Validate the generated code, checking for consistency.
      verifyFunction(*f);
    
      return f;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return sign->to_string(indent) + body->to_string(indent + "  ");
    };
};

// DeclNode - Class for ...
class DeclNode : public Node 
{
  private:
    std::unique_ptr<VarDeclNode> vd;
    std::unique_ptr<FunDeclNode> fd;

  public:
    DeclNode(std::unique_ptr<VarDeclNode> vd) : vd(std::move(vd)) {}
    DeclNode(std::unique_ptr<FunDeclNode> fd) : fd(std::move(fd)) {}
    void codegen()
    {
      if (vd)
      {
        vd->codegen(); 
      } 
      else
      {
        fd->codegen();
      }        
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return vd ? vd->to_string(indent) : fd->to_string(indent);
    };
};

// DeclsNode - Class for ...
class DeclsNode : public Node 
{
  private:
    std::vector<std::unique_ptr<DeclNode>> decls;

  public:
    DeclsNode(std::vector<std::unique_ptr<DeclNode>> decls) : decls(std::move(decls)) {}
    void codegen()
    {
      for (const auto& decl : decls) 
        decl->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<decls>\n";
      for (const auto& decl : decls)
      {
        str += decl->to_string(indent + "  ");
      }
      return str;
    };
};

// ExternsNode - Class for ...
class ExternsNode : public Node 
{
  private:
    std::vector<std::unique_ptr<FunSignNode>> externs;

  public:
    ExternsNode(std::vector<std::unique_ptr<FunSignNode>> externs) : externs(std::move(externs)) {}
    void codegen()
    {
      for (const auto& e : externs)
        e->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<externs>\n";
      for (const auto& e : externs)
      {
        str += e->to_string(indent + "  ");
      }
      return str;
    };
};

// ProgramNode - Class for ...
class ProgramNode : public Node 
{
  private:
    std::unique_ptr<ExternsNode> externs;
    std::unique_ptr<DeclsNode> decls;

  public:
    ProgramNode(
      std::unique_ptr<ExternsNode> externs, 
      std::unique_ptr<DeclsNode> decls
    ) : externs(std::move(externs)), decls(std::move(decls)) 
    {}
    void codegen()
    {
      if (externs) externs->codegen();
      decls->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<program>\n";
      if (externs) str += externs->to_string(indent + "  ");
      str += decls->to_string(indent + "  ");
      return str;
    };
};

// UnaryNode - Class for ...
class UnaryNode : public ExprNode 
{
  private:
    TOKEN op;
    std::unique_ptr<ExprNode> expr;

  public:
    UnaryNode(TOKEN op, std::unique_ptr<ExprNode> expr) : op(op), expr(std::move(expr)) {}
    virtual Value* codegen() override
    {
      Value* expr_v = expr->codegen();
      if (!expr_v) return nullptr;

      switch (op.type) 
      {
        case MINUS:
          return builder.CreateNeg(expr_v, "neg");;
        case NOT:
          return builder.CreateICmpEQ(expr_v, getBoolLL(false), "not");
        default:
          return nullptr;  // invalid unary operator
      }
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<unary>" + op.lexeme + "\n" + expr->to_string(indent + "  ");
    };
};

// VariableNode - Class for variable identifier references like sum, user_name
class VariableNode : public ExprNode 
{
  private:
    TOKEN id;

  public:
    VariableNode(TOKEN id): id(id) {}
    virtual Value* codegen() override
    {
      // Look this variable up.
      Value *v = namedValues[id.lexeme];
      if (!v) return nullptr; // Unknown variable name.

      return v;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<variable> " + id.lexeme + "\n";
    };
};

// CallNode - Class for ...
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
    virtual Value* codegen() override
    {
      // Look up the name in the global module table.
      Function *function = module->getFunction(id.lexeme);

      if (!function) return nullptr;                           // Unknown function referenced
      if (function->arg_size() != args.size()) return nullptr; // Incorrect number of arguments passed

      std::vector<Value *> args_v;
      for (unsigned i = 0, e = args.size(); i != e; ++i)
      {
        args_v.push_back(args[i]->codegen());
        if (!args_v.back()) return nullptr;
      }

      return builder.CreateCall(function, args_v, "call");
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str =  indent + "<call> " + id.lexeme + "\n";
      for (const auto& arg : args)
      {
        str += arg->to_string(indent + "  ");
      }
      return str; 
    };
};

// FloatNode - Class for floating point literals like ...
class FloatNode : public ExprNode 
{
  private:
    float value;

  public:
    FloatNode(TOKEN tok)
    {
      value = std::stof(tok.lexeme); 
    }
    virtual Value* codegen() override
    {
      return getFloatLL(value);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<float> " + std::to_string(value) + "\n";
    };
};

// IntNode - Class for integer literals like 1, 2, 10,
class IntNode : public ExprNode 
{
  private:
    int value;

  public:
    IntNode(TOKEN tok)
    {
      value = std::stoi(tok.lexeme); 
    }
    virtual Value* codegen() override
    {
      return getIntLL(value);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<int> " + std::to_string(value) + "\n";
    };
};

// BoolNode - Class for boolean literals true, false
class BoolNode : public ExprNode 
{
  private:
    bool value;

  public:
    BoolNode(TOKEN tok)
    {
      value = tok.lexeme == "true"; // TODO: check
    }
    virtual Value* codegen() override
    {
      return getBoolLL(value);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bool> " + std::to_string(value) + "\n";
    };
};
