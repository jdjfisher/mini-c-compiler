
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

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// Node - Base class for all AST nodes.
class Node 
{
  public:
    virtual ~Node() {}
    virtual Value *codegen() = 0;
    virtual std::string to_string(std::string indent = "") const;
};

// ExprNode - Class for ...
class ExprNode : public Node {};

// LiteralNode - Class for ...
class LiteralNode : public ExprNode {};

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
    virtual Value *codegen() override
    {
      Value *l_v = l->codegen();
      Value *r_v = r->codegen();
      if (!l_v || !r_v) return nullptr;

      switch (op.type) 
      {
        // case OR
        // case AND
        case PLUS:
          return builder.CreateFAdd(l_v, r_v, "addtmp");
        case MINUS:
          return builder.CreateFSub(l_v, r_v, "subtmp");
        case ASTERIX:
          return builder.CreateFMul(l_v, r_v, "multmp");
        // case DIV
        // case MOD
        // case EQ
        // case NE
        case LT:
        {
          l_v = builder.CreateFCmpULT(l_v, r_v, "cmptmp");
          return builder.CreateUIToFP(l_v, Type::getDoubleTy(context), "booltmp");
        }
        case GT:
        {
          l_v = builder.CreateFCmpUGT(l_v, r_v, "cmptmp");
          return builder.CreateUIToFP(l_v, Type::getDoubleTy(context), "booltmp");
        }
        default:
          return nullptr; // invalid binary operator"
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
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<assign> " + id.lexeme + "\n" + e->to_string(indent + "  ");
    };
};

// ArgListNode - Class for ...
class ArgListNode : public Node 
{
  private:
    std::unique_ptr<ExprNode> e;
    std::unique_ptr<ArgListNode> al;

  public:
    ArgListNode(
      std::unique_ptr<ExprNode> e, std::unique_ptr<ArgListNode> al = nullptr
    ) : e(std::move(e)), al(std::move(al)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<arg_list>\n";
      str += e->to_string(indent + "  ");
      if (al) str += al->to_string(indent + "  ");
      return str;
    };
};

// ArgsNode - Class for ...
class ArgsNode : public Node 
{
  private:
    std::unique_ptr<ArgListNode> al;

  public:
    ArgsNode(std::unique_ptr<ArgListNode> al = nullptr) : al(std::move(al)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<args>\n";
      str += al->to_string(indent + "  ");
      return str;
    };
};

// VarTypeNode - Class for ...
class VarTypeNode : public Node 
{
  private:
    TOKEN tok;

  public:
    VarTypeNode(TOKEN tok) : tok(tok) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<var_type> " + tok.lexeme + "\n";
      return str;
    };
};

// VarDeclNode - Class for ...
class VarDeclNode : public Node 
{
  private:
    std::unique_ptr<VarTypeNode> vt;
    TOKEN id;

  public:
    VarDeclNode(std::unique_ptr<VarTypeNode> vt, TOKEN id) : vt(std::move(vt)), id(id) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<var_decl> " + id.lexeme + "\n";
      str += vt->to_string(indent + "  ");
      return str;  
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
    virtual Value *codegen() override
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
    virtual Value *codegen() override
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
    virtual Value *codegen() override
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
    virtual Value *codegen() override
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
    virtual Value *codegen() override
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
    virtual Value *codegen() override
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
    virtual Value *codegen() override
    {
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
    virtual Value *codegen() override
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
    std::unique_ptr<VarTypeNode> vt;
    TOKEN id;

  public:
    ParamNode(std::unique_ptr<VarTypeNode> vt, TOKEN id) : vt(std::move(vt)), id(id) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<param> " + id.lexeme + "\n";
      str += vt->to_string(indent + "  ");
      return str;
    };
};

// ParamListNode - Class for ...
class ParamListNode : public Node 
{
  private:
    std::unique_ptr<ParamNode> p;
    std::unique_ptr<ParamListNode> pl;

  public:
    ParamListNode(
      std::unique_ptr<ParamNode> p, std::unique_ptr<ParamListNode> pl = nullptr
    ) : p(std::move(p)), pl(std::move(pl)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<param_list>\n";
      str += p->to_string(indent + "  ");
      if (pl) str += pl->to_string(indent + "  ");
      return str;
    };
};

// ParamsNode - Class for ...
class ParamsNode : public Node 
{
  private:
    std::unique_ptr<ParamListNode> pl;

  public:
    ParamsNode(std::unique_ptr<ParamListNode> pl = nullptr) : pl(std::move(pl)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<params>\n";
      str += pl->to_string(indent + "  ");
      return str;
    };
};

// FunTypeNode - Class for ...
class FunTypeNode : public Node 
{
  private:
    std::unique_ptr<VarTypeNode> vt;

  public:
    FunTypeNode(std::unique_ptr<VarTypeNode> vt = nullptr) : vt(std::move(vt)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + (vt ? "<fun_type>\n" : "<fun_type> void\n");
      if (vt) str += vt->to_string(indent + "  ");
      return str;
    };
};

// FunDeclNode - Class for ...
class FunDeclNode : public Node 
{
  private:
    std::unique_ptr<FunTypeNode> ft;
    std::unique_ptr<ParamsNode> p;
    std::unique_ptr<BlockStmtNode> bs;
    TOKEN id;

  public:
    FunDeclNode(std::unique_ptr<FunTypeNode> ft, 
                std::unique_ptr<ParamsNode> p, 
                std::unique_ptr<BlockStmtNode> bs,
                TOKEN id
    ) : ft(std::move(ft)), p(std::move(p)), bs(std::move(bs)), id(id) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<fun_decl> " + id.lexeme + "\n";
      str += ft->to_string(indent + "  ");
      str += p->to_string(indent + "  ");
      str += bs->to_string(indent + "  ");
      return str;
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
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<decl>\n";
      if (vd) str += vd->to_string(indent + "  ");
      if (fd) str += fd->to_string(indent + "  ");
      return str;
    };
};

// DeclListNode - Class for ...
class DeclListNode : public Node 
{
  private:
    std::unique_ptr<DeclNode> d;
    std::unique_ptr<DeclListNode> dl;

  public:
    DeclListNode(
      std::unique_ptr<DeclNode> d, std::unique_ptr<DeclListNode> dl = nullptr
    ) : d(std::move(d)), dl(std::move(dl)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<decl_list>\n";
      str += d->to_string(indent + "  ");
      if (dl) str += dl->to_string(indent + "  ");
      return str;
    };
};

// ExternNode - Class for ...
class ExternNode : public Node 
{
  private:
    std::unique_ptr<FunTypeNode> ft;
    std::unique_ptr<ParamsNode> p;
    TOKEN id;

  public:
    ExternNode(
      std::unique_ptr<FunTypeNode> ft, std::unique_ptr<ParamsNode> p, TOKEN id
    ) : ft(std::move(ft)), p(std::move(p)), id(id) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<extern> " + id.lexeme + "\n";
      str += ft->to_string(indent + "  ");
      if (p) str += p->to_string(indent + "  ");
      return str;
    };
};

// ExternListNode - Class for ...
class ExternListNode : public Node 
{
  private:
    std::unique_ptr<ExternNode> e;
    std::unique_ptr<ExternListNode> el;

public:
  ExternListNode(
    std::unique_ptr<ExternNode> e, std::unique_ptr<ExternListNode> el = nullptr
  ) : e(std::move(e)), el(std::move(el)) {}
  virtual Value *codegen() override
  {
    return NULL;
  };
  virtual std::string to_string(std::string indent = "") const override
  {
    std::string str = indent + "<extern_list>\n";
    str += e->to_string(indent + "  ");
    if (el) str += el->to_string(indent + "  ");
    return str;
  };
};

// ProgramNode - Class for ...
class ProgramNode : public Node 
{
  private:
    std::unique_ptr<ExternListNode> el;
    std::unique_ptr<DeclListNode> dl;

  public:
    ProgramNode(
      std::unique_ptr<ExternListNode> el, std::unique_ptr<DeclListNode> dl
    ) : el(std::move(el)), dl(std::move(dl)) {}
    virtual Value *codegen() override
    {
      return NULL; // TODO: Translate to LLVM IR
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<program>\n";
      str += el->to_string(indent + "  ");
      if (dl) str += dl->to_string(indent + "  ");
      return str;
    };
};

// UnaryNode - Class for ...
class UnaryNode : public LiteralNode 
{
  private:
    TOKEN op;
    std::unique_ptr<LiteralNode> l;

  public:
    UnaryNode(TOKEN op, std::unique_ptr<LiteralNode> l) : op(op), l(std::move(l)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<unary>" + op.lexeme + "\n" + l->to_string(indent + "  ");
    };
};

// ParenthesesNode - Class for ...
class ParenthesesNode : public LiteralNode 
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ParenthesesNode(std::unique_ptr<ExprNode> e) : e(std::move(e)) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<parentheses>\n" + e->to_string(indent + "  ");
    };
};

// VariableNode - Class for variable identifier references like sum, user_name
class VariableNode : public LiteralNode 
{
  private:
    TOKEN id;

  public:
    VariableNode(TOKEN id): id(id) {}
    virtual Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<variable> " + id.lexeme + "\n";
    };
};

// CallNode - Class for ...
class CallNode : public LiteralNode 
{
  private:
    TOKEN id;
    std::unique_ptr<ArgsNode> a;

  public:
    CallNode(TOKEN id, std::unique_ptr<ArgsNode> a) : id(id), a(std::move(a)) {}
    virtual Value *codegen() override
    {
      // Look up the name in the global module table.
      Function *f = module->getFunction(id.lexeme);
      if (!f) return nullptr; // Unknown function referenced

      // If argument mismatch error.
      // if (f->arg_size() != args.size()) return nullptr; // Incorrect number of arguments passed

      // std::vector<Value *> ArgsV;
      // for (unsigned i = 0, e = Args.size(); i != e; ++i) {
      //   ArgsV.push_back(Args[i]->codegen());
      //   if (!ArgsV.back()) return nullptr;
      // }

      // return Builder.CreateCall(CalleeF, ArgsV, "calltmp");

      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<call> " + id.lexeme + "\n";
    };
};

// IntNode - Class for integer literals like 1, 2, 10,
class IntNode : public LiteralNode 
{
  private:
    int val;

  public:
    IntNode(TOKEN tok)
    {
      val = std::stoi(tok.lexeme); 
    }
    virtual Value *codegen() override
    {
      // return ConstantFP::get(context, APInt(val));
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<int> " + std::to_string(val) + "\n";
    };
};

// FloatNode - Class for floating point literals like ...
class FloatNode : public LiteralNode 
{
  private:
    float val;

  public:
    FloatNode(TOKEN tok)
    {
      val = std::stof(tok.lexeme); 
    }
    virtual Value *codegen() override
    {
      return ConstantFP::get(context, APFloat(val));
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<float> " + std::to_string(val) + "\n";
    };
};

// BoolNode - Class for boolean literals true, false
class BoolNode : public LiteralNode 
{
  private:
    bool val;

  public:
    BoolNode(TOKEN tok)
    {
      val = tok.lexeme == "true"; // TODO: check
    }
    virtual Value *codegen() override
    {
      // return ConstantFP::get(context, APInt(int(val)));
      return NULL;
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bool> " + std::to_string(val) + "\n";
    };
};
