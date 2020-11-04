
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
#include <exception>

// Application imports
#include "lexer.h"

// Namespaces
using namespace llvm;

// Aliases
using SymbolTable = std::map<std::string, AllocaInst*>;


extern LLVMContext context;
extern IRBuilder<> builder;
extern std::unique_ptr<Module> module;
static SymbolTable globalValues;


class SemanticError: public std::exception
{
  private:
    std::string m;
    // TOKEN t;

  public:
    SemanticError(std::string m) : m(m) {}
    virtual const char* what() const throw()
    {
      return m.c_str();
    }
};


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

static AllocaInst* createEntryBlockAlloca(Function* f,
                                          Type* type,
                                          const std::string& iden)
{
  IRBuilder<> t_b(&f->getEntryBlock(), f->getEntryBlock().begin());
  return t_b.CreateAlloca(type, 0, iden.c_str());
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
    virtual Value* codegen(SymbolTable& symbols) = 0;
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
    virtual Value* codegen(SymbolTable& symbols) override
    {
      Value* l_v = l->codegen(symbols);
      Value* r_v = r->codegen(symbols);
      if (!l_v || !r_v) return nullptr;

      // std::cout << "ops: " << l_v->getType() << " " << r_v->getType() << "\n";
      // l_v->getType()->print();

      switch (op.type)
      {
        case OR:
          return builder.CreateSelect(l_v, getBoolLL(true), r_v, "or");
        case AND:
          return builder.CreateSelect(l_v, r_v, getBoolLL(false), "and");
        case PLUS:
          return builder.CreateAdd(l_v, r_v, "add");
        case MINUS:
          return builder.CreateSub(l_v, r_v, "sub");
        case ASTERIX:
          return builder.CreateMul(l_v, r_v, "mul");
        case DIV:
          return builder.CreateSDiv(l_v, r_v, "div");
        case MOD:
          return builder.CreateSRem(l_v, r_v, "rem");
        case EQ:
          return builder.CreateICmpEQ(l_v, r_v, "eq"); 
        case NE:
          return builder.CreateICmpNE(l_v, r_v, "ne");
        case LE:
          return builder.CreateICmpSLE(l_v, r_v, "le");
        case GE:
          return builder.CreateICmpSGE(l_v, r_v, "ge");
        case LT:
          return builder.CreateICmpSLT(l_v, r_v, "lt");
        case GT:
          return builder.CreateICmpSGT(l_v, r_v, "gt");
        default:
          throw SemanticError("Invalid binary operator");
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
    virtual Value* codegen(SymbolTable& symbols) override
    {
      //
      Value* value = e->codegen(symbols);

      // Look up the name.
      Value* variable = symbols[id.lexeme];
      if (!variable) throw SemanticError("Unknown variable name: " + id.lexeme);

      //
      builder.CreateStore(value, variable);
      return value;
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
      // TODO: implement
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
    virtual void codegen(std::map<std::string, AllocaInst *> &symbols) = 0;
};

// BlockStmtNode - Class for ...
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
        d->codegen();

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

// ExprStmtNode - Class for ...
class ExprStmtNode : public StmtNode // TODO: remove node
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ExprStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual void codegen(SymbolTable& symbols) override
    {
      e->codegen(symbols); // ...
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
    virtual void codegen(SymbolTable& symbols) override
    {
      builder.CreateRet(e->codegen(symbols));
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
    virtual void codegen(SymbolTable& symbols) override
    {
      Value* cond_v = cond->codegen(symbols);

      // Convert condition to a bool by comparing non-equal to 0.
      cond_v = builder.CreateICmpNE(cond_v, getBoolLL(false), "if-cond");

      Function* function = builder.GetInsertBlock()->getParent();

      // Create blocks.
      BasicBlock* then_bb = BasicBlock::Create(context, "then", function);
      BasicBlock* else_bb = else_ ? BasicBlock::Create(context, "else", function) : nullptr;
      BasicBlock* join_bb = BasicBlock::Create(context, "join", function);

      // Create conditional branch
      builder.CreateCondBr(cond_v, then_bb, else_bb ? else_bb : join_bb);

      // Emit then value.
      builder.SetInsertPoint(then_bb);
      then->codegen(symbols);
      builder.CreateBr(join_bb);
 
      // Emit else block.
      if (else_)
      {
        builder.SetInsertPoint(else_bb);
        else_->codegen(symbols);
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
    virtual void codegen(SymbolTable& symbols) override
    {
      // Make the new basic block for the loop header, inserting after current block.
      Function* function = builder.GetInsertBlock()->getParent();
      BasicBlock* loop_bb = BasicBlock::Create(context, "loop", function);

      // Insert an explicit fall through from the current block to the loop_bb.
      builder.CreateBr(loop_bb);
      builder.SetInsertPoint(loop_bb);

      // Generate code for the loop condition.
      Value* cond_v = cond->codegen(symbols);

      // Create basic blocks for the loop body and the join point.
      BasicBlock* body_bb = BasicBlock::Create(context, "body", function);
      BasicBlock* join_bb = BasicBlock::Create(context, "join", function);

      // Create a conditional branch.
      builder.CreateCondBr(cond_v, body_bb, join_bb);

      // 
      builder.SetInsertPoint(body_bb);
      loop->codegen(symbols);
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

      // Set names for all arguments
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
        str += param->to_string(indent + "  ");

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
    void codegen()
    {
      // First, check for an existing function from a previous 'extern' declaration.
      Function* function = module->getFunction(sign->getName());

      // 
      if (!function)
      {
        function = sign->codegen();
      }

      if (!function->empty()) throw SemanticError("Function cannot be redefined");

      // Create a new basic block to start insertion into.
      BasicBlock* body_bb = BasicBlock::Create(context, "body_bb", function);
      builder.SetInsertPoint(body_bb);

      // Record the function arguments in the map.
      SymbolTable symbols;
      
      for (auto& arg : function->args())
      {
        // Create an alloca for this variable.
        AllocaInst* alloca = createEntryBlockAlloca(function, arg.getType(), arg.getName().str());

        // Store the initial value into the alloca.
        builder.CreateStore(&arg, alloca);

        // Add arguments to variable symbol table.
        symbols[arg.getName().str()] = alloca;
      }

      // Generate the body
      body->codegen(symbols);

      // Validate the generated code, checking for consistency.
      verifyFunction(*function);
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

// ProgramNode - Class for ...
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

// UnaryNode - Class for ...
class UnaryNode : public ExprNode 
{
  private:
    TOKEN op;
    std::unique_ptr<ExprNode> expr;

  public:
    UnaryNode(TOKEN op, std::unique_ptr<ExprNode> expr) : op(op), expr(std::move(expr)) {}
    virtual Value* codegen(SymbolTable& symbols) override
    {
      Value* expr_v = expr->codegen(symbols);
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
    virtual Value* codegen(SymbolTable& symbols) override
    {
      // Look this variable up.
      Value *v = symbols[id.lexeme];
      if (!v) throw SemanticError("Unknown variable name: " + id.lexeme);

      // Load the value.
      return builder.CreateLoad(v, id.lexeme.c_str());
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
    virtual Value* codegen(SymbolTable& symbols) override
    {
      // Look up the name in the global module table.
      Function *function = module->getFunction(id.lexeme);

      if (!function) return nullptr;                           // Unknown function referenced
      if (function->arg_size() != args.size()) return nullptr; // Incorrect number of arguments passed

      std::vector<Value *> args_v;
      for (unsigned i = 0, e = args.size(); i != e; ++i)
      {
        args_v.push_back(args[i]->codegen(symbols));
        if (!args_v.back()) return nullptr;
      }

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
    virtual Value* codegen(SymbolTable& symbols) override
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
    virtual Value* codegen(SymbolTable& symbols) override
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
    virtual Value* codegen(SymbolTable& symbols) override
    {
      return getBoolLL(value);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bool> " + std::to_string(value) + "\n";
    };
};
