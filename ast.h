
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


// Type helper TODO: move/cleanup
static Type* typeLookup(int type)
{
  switch (type)
  {
    case VOID_TOK:
      return Type::getVoidTy(context);
    case FLOAT_TOK:
      return Type::getFloatTy(context);
    case INT_TOK:
    case BOOL_TOK:
      return Type::getFloatTy(context); // TODO: sort
    default:
      return nullptr;
  }
}

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// Node - Base class for all AST nodes.
class Node 
{
  public:
    virtual ~Node() {}
    virtual Value* codegen() = 0;
    virtual std::string to_string(std::string indent = "") const;
};

// ExprNode - Class for ...
class ExprNode : public Node {};

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
      Value *l_v = l->codegen();
      Value *r_v = r->codegen();
      if (!l_v || !r_v) return nullptr;

      switch (op.type) 
      {
        case OR:
          return nullptr; // TODO: finish
        case AND:
          return nullptr; // TODO: finish
        case PLUS:
          return builder.CreateFAdd(l_v, r_v, "addtmp");
        case MINUS:
          return builder.CreateFSub(l_v, r_v, "subtmp");
        case ASTERIX:
          return builder.CreateFMul(l_v, r_v, "multmp");
        case DIV:
          return builder.CreateFDiv(l_v, r_v, "divtmp");
        case MOD:
          return builder.CreateFRem(l_v, r_v, "remtmp");
        case EQ:
          return builder.CreateFCmpUEQ(l_v, r_v, "cmptmp");
        case NE:
          return builder.CreateFCmpUNE(l_v, r_v, "cmptmp");
        case LE:
          return builder.CreateFCmpULE(l_v, r_v, "cmptmp");
        case GE:
          return builder.CreateFCmpUGE(l_v, r_v, "cmptmp");
        case LT:
          return builder.CreateFCmpULT(l_v, r_v, "cmptmp");
        case GT:
          return builder.CreateFCmpUGT(l_v, r_v, "cmptmp");
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
      return NULL;
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
    virtual Value* codegen() override
    {
      return NULL;
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
    std::unique_ptr<VarDeclNode> vd;
    std::unique_ptr<LocalDeclsNode> lds;

  public:
    LocalDeclsNode() {}
    LocalDeclsNode(
      std::unique_ptr<VarDeclNode> vd, std::unique_ptr<LocalDeclsNode> lds
    ) : vd(std::move(vd)), lds(std::move(lds)) {}
    virtual Value* codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<local_decls>\n";
      if (vd) str += vd->to_string(indent + "  ");
      if (lds) str += lds->to_string(indent + "  ");
      return str;  
    };
};

// StmtNode - Class for ...
class StmtNode : public Node {};

// StmtListNode - Class for ...
class StmtListNode : public Node 
{
  private:
    std::unique_ptr<StmtNode> s;
    std::unique_ptr<StmtListNode> sl;

  public:
    StmtListNode() {}
    StmtListNode(
      std::unique_ptr<StmtNode> s, std::unique_ptr<StmtListNode> sl
    ) : s(std::move(s)), sl(std::move(sl)) {}
    virtual Value* codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<stmt_list>\n";
      if (s) str += s->to_string(indent + "  ");
      if (sl) str += sl->to_string(indent + "  ");
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
      std::unique_ptr<LocalDeclsNode> lds, std::unique_ptr<StmtListNode> sl
    ) : lds(std::move(lds)), sl(std::move(sl)) {}
    virtual Value* codegen() override
    {
      return NULL;
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
class ExprStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ExprStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual Value* codegen() override
    {
      return NULL;
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
    virtual Value* codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<return_stmt>\n";
      if (e) str += e->to_string(indent + "  ");
      return str;
    };
};

// ElseStmtNode - Class for ...
class ElseStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<BlockStmtNode> bs;

  public:
    ElseStmtNode(std::unique_ptr<BlockStmtNode> bs = nullptr) : bs(std::move(bs)) {}
    virtual Value* codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<else_stmt>\n";
      if (bs) str += bs->to_string(indent + "  ");
      return str;
    };
};

// IfStmtNode - Class for ...
class IfStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> e;
    std::unique_ptr<BlockStmtNode> bs;
    std::unique_ptr<ElseStmtNode> es;

  public:
    IfStmtNode(
      std::unique_ptr<ExprNode> e, std::unique_ptr<BlockStmtNode> bs, std::unique_ptr<ElseStmtNode> es
    ) : e(std::move(e)), bs(std::move(bs)), es(std::move(es)) {}
    virtual Value* codegen() override
    {
      Value *e_v = e->codegen();
      if (!e_v) return nullptr;

      // Convert condition to a bool by comparing non-equal to 0.0.
      e_v = builder.CreateFCmpONE(e_v, ConstantFP::get(context, APFloat(0.0)), "ifcond");

      Function *f = builder.GetInsertBlock()->getParent();

      // // Create blocks for the then and else cases.  Insert the 'then' block at the end of the function.
      // BasicBlock *then_bb = BasicBlock::Create(context, "then", function);
      // BasicBlock *else_bb = BasicBlock::Create(context, "else");
      // BasicBlock *merge_bb = BasicBlock::Create(context, "ifcont");

      // builder.CreateCondBr(e_v, then_bb, else_bb);

      // // Emit then value.
      // builder.SetInsertPoint(then_bb);

      // Value *then_v = bs->codegen();
      // if (!then_v) return nullptr;

      // builder.CreateBr(merge_bb);
      // // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
      // then_bb = builder.GetInsertBlock();
      // // Emit else block.
      // f->getBasicBlockList().push_back(else_bb);
      // builder.SetInsertPoint(else_bb);

      // Value *else_v = Else->codegen();
      // if (!else_v) return nullptr;

      // builder.CreateBr(merge_bb);
      // // codegen of 'Else' can change the current block, update else_bb for the PHI.
      // else_bb = Builder.GetInsertBlock();

      // // Emit merge block.
      // f->getBasicBlockList().push_back(merge_bb);
      // builder.SetInsertPoint(merge_bb);

      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<if_stmt>\n";
      str += e->to_string(indent + "  ");
      str += bs->to_string(indent + "  ");
      str += es->to_string(indent + "  ");
      return str;
    };
};

// WhileStmtNode - Class for ...
class WhileStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> e;
    std::unique_ptr<StmtNode> s;

  public:
    WhileStmtNode(std::unique_ptr<ExprNode> e, std::unique_ptr<StmtNode> s) : e(std::move(e)), s(std::move(s)) {}
    virtual Value* codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<while_stmt>\n";
      str += e->to_string(indent + "  ");
      str += s->to_string(indent + "  ");
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
    virtual Value* codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<param> " + type.lexeme + " " + id.lexeme + "\n";
      return str;
    };
    std::string getName() { return id.lexeme; };
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
    virtual Value* codegen() override
    {
      // 
      Type* returnType = typeLookup(type.type);
      std::vector<Type*> argTypes;
 
      for (const auto& param : params)
      {
        // argTypes.push_back(param); // TODO: get type
      }
    
      // Create a new function type
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
        + id.lexeme + "\n" + indent + "  " + "<params>\n";

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
    std::unique_ptr<FunSignNode> fs;
    std::unique_ptr<BlockStmtNode> bs;

  public:
    FunDeclNode(std::unique_ptr<FunSignNode> fs, std::unique_ptr<BlockStmtNode> bs
    ) : fs(std::move(fs)), bs(std::move(bs)) {}
    virtual Value* codegen() override
    {
      // First, check for an existing function from a previous 'extern' declaration.
      Function* f = module->getFunction(fs->getName());

      if (!f)
      {
        f = (Function*) fs->codegen();
        if (!f) return nullptr;
      }

      if (!f->empty()) return nullptr; // Function cannot be redefined.

      // Create a new basic block to start insertion into.
      BasicBlock *bb = BasicBlock::Create(context, "entry", f);
      builder.SetInsertPoint(bb);

      // Record the function arguments in the namedValues map.
      namedValues.clear();
      for (auto &arg : f->args())
      {
        namedValues[arg.getName()] = &arg;
      }

      if (Value *r_v = bs->codegen()) 
      {
        // Finish off the function.
        builder.CreateRet(r_v);

        // Validate the generated code, checking for consistency.
        verifyFunction(*f);

        return (Value*) f;
      }

      // Error reading body, remove function.
      f->eraseFromParent();
      return nullptr;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return fs->to_string(indent) + bs->to_string(indent + "  ");
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
    virtual Value* codegen() override
    {
      return NULL;
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
    std::unique_ptr<DeclNode> d;
    std::unique_ptr<DeclsNode> ds;

  public:
    DeclsNode(
      std::unique_ptr<DeclNode> d, std::unique_ptr<DeclsNode> ds = nullptr
    ) : d(std::move(d)), ds(std::move(ds)) {}
    virtual Value* codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<decls>\n";
      str += d->to_string(indent + "  ");
      if (ds) str += ds->to_string(indent + "  ");
      return str;
    };
};

// ExternsNode - Class for ...
class ExternsNode : public Node 
{
  private:
    std::unique_ptr<FunSignNode> e;
    std::unique_ptr<ExternsNode> es;

public:
  ExternsNode(
    std::unique_ptr<FunSignNode> e, std::unique_ptr<ExternsNode> es = nullptr
  ) : e(std::move(e)), es(std::move(es)) {}
  virtual Value* codegen() override
  {
    return NULL;
  };
  virtual std::string to_string(std::string indent = "") const override
  {
    std::string str = indent + "<externs>\n";
    str += e->to_string(indent + "  ");
    if (es) str += es->to_string(indent + "  ");
    return str;
  };
};

// ProgramNode - Class for ...
class ProgramNode : public Node 
{
  private:
    std::unique_ptr<ExternsNode> el;
    std::unique_ptr<DeclsNode> dl;

  public:
    ProgramNode(
      std::unique_ptr<ExternsNode> el, std::unique_ptr<DeclsNode> dl
    ) : el(std::move(el)), dl(std::move(dl)) {}
    virtual Value* codegen() override
    {
      return NULL; // TODO: Translate to LLVM IR
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<program>\n";
      if (el) str += el->to_string(indent + "  ");
      str += dl->to_string(indent + "  ");
      return str;
    };
};

// UnaryNode - Class for ...
class UnaryNode : public ExprNode 
{
  private:
    TOKEN op;
    std::unique_ptr<ExprNode> l;

  public:
    UnaryNode(TOKEN op, std::unique_ptr<ExprNode> l) : op(op), l(std::move(l)) {}
    virtual Value* codegen() override
    {
      Value *l_v = l->codegen();
      if (!l_v) return nullptr;

      switch (op.type) 
      {
        case NOT:
          return nullptr;
        case MINUS:
          return nullptr;
        default:
          return nullptr;  // invalid unary operator
      }
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<unary>" + op.lexeme + "\n" + l->to_string(indent + "  ");
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
    ) : id(id), args(std::move(args)) {}
    virtual Value* codegen() override
    {
      // Look up the name in the global module table.
      Function *f = module->getFunction(id.lexeme);

      if (!f) return nullptr;                           // Unknown function referenced
      if (f->arg_size() != args.size()) return nullptr; // Incorrect number of arguments passed

      std::vector<Value *> a_v;
      for (unsigned i = 0, e = args.size(); i != e; ++i)
      {
        a_v.push_back(args[i]->codegen());
        if (!a_v.back()) return nullptr;
      }

      return builder.CreateCall(f, a_v, "calltmp");
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<call> " + id.lexeme + "\n";
      // + a->to_string(indent + "  "); TODO: fix 
    };
};

// IntNode - Class for integer literals like 1, 2, 10,
class IntNode : public ExprNode 
{
  private:
    int val;

  public:
    IntNode(TOKEN tok)
    {
      val = std::stoi(tok.lexeme); 
    }
    virtual Value* codegen() override
    {
      return ConstantInt::get(context, APInt(sizeof(int)*8, val));
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<int> " + std::to_string(val) + "\n";
    };
};

// FloatNode - Class for floating point literals like ...
class FloatNode : public ExprNode 
{
  private:
    float val;

  public:
    FloatNode(TOKEN tok)
    {
      val = std::stof(tok.lexeme); 
    }
    virtual Value* codegen() override
    {
      return ConstantFP::get(context, APFloat(val));
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<float> " + std::to_string(val) + "\n";
    };
};

// BoolNode - Class for boolean literals true, false
class BoolNode : public ExprNode 
{
  private:
    bool val;

  public:
    BoolNode(TOKEN tok)
    {
      val = tok.lexeme == "true"; // TODO: check
    }
    virtual Value* codegen() override
    {
      return ConstantInt::get(context, APInt(sizeof(bool)*8, val));
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bool> " + std::to_string(val) + "\n";
    };
};
