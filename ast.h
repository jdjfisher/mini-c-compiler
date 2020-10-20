
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

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// Node - Base class for all AST nodes.
class Node 
{
  public:
    virtual ~Node() {}
    virtual llvm::Value *codegen() = 0;
    virtual std::string to_string() const;
};

// LiteralNode - Class for ...
class LiteralNode : public Node {};

// FactorNode - Class for ...
class FactorNode : public Node 
{
  private:
    std::unique_ptr<LiteralNode> l;
    std::unique_ptr<FactorNode> f;
    TOKEN op;

  public:
    FactorNode(std::unique_ptr<LiteralNode> l) : l(std::move(l)) {}
    FactorNode(
      std::unique_ptr<LiteralNode> l, std::unique_ptr<FactorNode> f, TOKEN op
    ) : l(std::move(l)), f(std::move(f)), op(op) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<factor>";
    };
};

// TermNode - Class for ...
class TermNode : public Node 
{
  private:
    std::unique_ptr<FactorNode> f;
    std::unique_ptr<TermNode> t;
    TOKEN op;

  public:
    TermNode(std::unique_ptr<FactorNode> f) : f(std::move(f)) {}
    TermNode(
      std::unique_ptr<FactorNode> f, std::unique_ptr<TermNode> t, TOKEN op
    ) : f(std::move(f)), t(std::move(t)), op(op) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<term>";
    };
};

// OrderNode - Class for ...
class OrderNode : public Node 
{
  private:
    std::unique_ptr<TermNode> t;
    std::unique_ptr<OrderNode> o;
    TOKEN op;

  public:
    OrderNode(std::unique_ptr<TermNode> t) : t(std::move(t)) {}
    OrderNode(
      std::unique_ptr<TermNode> t, std::unique_ptr<OrderNode> o, TOKEN op
    ) : t(std::move(t)), o(std::move(o)), op(op) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<order>";
    };
};

// EqualNode - Class for ...
class EqualNode : public Node 
{
  private:
    std::unique_ptr<OrderNode> o;
    std::unique_ptr<EqualNode> e;
    TOKEN op;

  public:
    EqualNode(std::unique_ptr<OrderNode> o) : o(std::move(o)) {}
    EqualNode(
      std::unique_ptr<OrderNode> o, std::unique_ptr<EqualNode> e, TOKEN op
    ) : o(std::move(o)), e(std::move(e)), op(op) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<equal>";
    };
};

// ConjNode - Class for ...
class ConjNode : public Node 
{
  private:
    std::unique_ptr<EqualNode> e;
    std::unique_ptr<ConjNode> c;

  public:
    ConjNode(
      std::unique_ptr<EqualNode> e, std::unique_ptr<ConjNode> c = nullptr
    ) : e(std::move(e)), c(std::move(c)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<conj>";
    };
};

// DisjNode - Class for ...
class DisjNode : public Node 
{
  private:
    std::unique_ptr<ConjNode> c;
    std::unique_ptr<DisjNode> d;

  public:
    DisjNode(
      std::unique_ptr<ConjNode> c, std::unique_ptr<DisjNode> d = nullptr
    ) : c(std::move(c)), d(std::move(d)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<disj>";
    };
};

// ExprNode - Class for ...
class ExprNode : public Node 
{
  private:
    TOKEN id;
    std::unique_ptr<ExprNode> e;
    std::unique_ptr<DisjNode> d;

  public:
    ExprNode(TOKEN id, std::unique_ptr<ExprNode> e) : id(id), e(std::move(e)) {}
    ExprNode(std::unique_ptr<DisjNode> d) : d(std::move(d)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<expr>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<argList>";
    };
};

// ArgsNode - Class for ...
class ArgsNode : public Node 
{
  private:
    std::unique_ptr<ArgListNode> al;

  public:
    ArgsNode(std::unique_ptr<ArgListNode> al = nullptr) : al(std::move(al)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<args>";
    };
};

// VarTypeNode - Class for ...
class VarTypeNode : public Node 
{
  private:
    TOKEN tok;

  public:
    VarTypeNode(TOKEN tok) : tok(tok) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<var_type>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<var_decl>" + id.lexeme;
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<local_decls>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<stmt_list>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<block_stmt>";
    };
};

// ExprStmtNode - Class for ...
class ExprStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ExprStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<expr_stmt>";
    };
};

// ReturnStmtNode - Class for ...
class ReturnStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ReturnStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<return_stmt>";
    };
};

// ElseStmtNode - Class for ...
class ElseStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<BlockStmtNode> bs;

  public:
    ElseStmtNode(std::unique_ptr<BlockStmtNode> bs = nullptr) : bs(std::move(bs)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<else_stmt>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<if_stmt>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<while_stmt>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<param> " + id.lexeme;
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<param_list>";
    };
};

// ParamsNode - Class for ...
class ParamsNode : public Node 
{
  private:
    std::unique_ptr<ParamListNode> pl;

  public:
    ParamsNode(std::unique_ptr<ParamListNode> pl = nullptr) : pl(std::move(pl)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<params>";
    };
};

// FunTypeNode - Class for ...
class FunTypeNode : public Node 
{
  private:
    std::unique_ptr<VarTypeNode> vt;

  public:
    FunTypeNode(std::unique_ptr<VarTypeNode> vt = nullptr) : vt(std::move(vt)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return vt ? "<fun_type>" : "<fun_type> void";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<fun_decl> " + id.lexeme;
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<decl>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<decl_list>";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<extern> " + id.lexeme + "\n    " + ft->to_string() + "\n    " + p->to_string() + "\n";
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
  virtual llvm::Value *codegen() override
  {
    return NULL;
  };
  virtual std::string to_string() const override
  {
    std::string s = "<extern_list>\n  " + e->to_string() + "\n";
    if (el) s+= "  " + el->to_string() + "\n";
    
    return s;
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
    virtual llvm::Value *codegen() override
    {
      return NULL; // TODO: Translate to LLVM IR
    };
    virtual std::string to_string() const override
    {
      return "<program>\n  " + el->to_string() + "\n  " + dl->to_string() + "\n";
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<unary>" + op.lexeme;
    };
};

// ParenthesesNode - Class for ...
class ParenthesesNode : public LiteralNode 
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ParenthesesNode(std::unique_ptr<ExprNode> e) : e(std::move(e)) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<parentheses>";
    };
};

// VariableNode - Class for variable identifier references like sum, user_name
class VariableNode : public LiteralNode 
{
  private:
    std::string id;

  public:
    VariableNode(TOKEN tok)
    {
      id = tok.lexeme;
    }
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<variable> " + id;
    };
};

// CallNode - Class for ...
class CallNode : public LiteralNode 
{
  private:
    std::string id;
    std::unique_ptr<ArgsNode> a;

  public:
    CallNode(TOKEN tok, std::unique_ptr<ArgsNode> a) : a(std::move(a))
    {
      id = tok.lexeme;
    }
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<call>" + id;
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<int> " + std::to_string(val);
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<float> " + std::to_string(val);
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
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "<bool> " + std::to_string(val);
    };
};
