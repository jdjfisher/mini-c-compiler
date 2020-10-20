
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
    LiteralNode* l;
    TOKEN op;
    FactorNode* f;

  public:
    FactorNode(LiteralNode* l) : l(l) {}
    FactorNode(LiteralNode* l, TOKEN op, FactorNode* f) : l(l), op(op), f(f) {}
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
    FactorNode* f;
    TOKEN op;
    TermNode* t;

  public:
    TermNode(FactorNode* f) : f(f) {}
    TermNode(FactorNode* f, TOKEN op, TermNode* t) : f(f), op(op), t(t) {}
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
    TermNode* t;
    TOKEN op;
    OrderNode* o;

  public:
    OrderNode(TermNode* t) : t(t) {}
    OrderNode(TermNode* t, TOKEN op, OrderNode* o) : t(t), op(op), o(o) {}
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
    OrderNode* o;
    TOKEN op;
    EqualNode* e;

  public:
    EqualNode(OrderNode* o) : o(o) {}
    EqualNode(OrderNode* o, TOKEN op, EqualNode* e) : o(o), op(op), e(e) {}
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
    EqualNode* e;
    ConjNode* c;

  public:
    ConjNode(EqualNode* e, ConjNode* c = nullptr) : e(e), c(c) {}
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
    ConjNode* c;
    DisjNode* d;

  public:
    DisjNode(ConjNode* c, DisjNode* d = nullptr) : c(c), d(d) {}
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
    ExprNode* e;
    DisjNode* d;

  public:
    ExprNode(TOKEN id, ExprNode* e) : id(id), e(e) {}
    ExprNode(DisjNode* d) : d(d) {}
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
    ExprNode* e;
    ArgListNode* al;

  public:
    ArgListNode(ExprNode* e, ArgListNode* al = nullptr) : e(e), al(al) {}
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
    ArgListNode* al;

  public:
    ArgsNode(ArgListNode* al = nullptr) : al(al) {}
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
    VarTypeNode* vt;
    TOKEN id;

  public:
    VarDeclNode(VarTypeNode* vt, TOKEN id) : vt(vt), id(id) {}
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
    VarDeclNode* vd;
    LocalDeclsNode* lds;

  public:
    LocalDeclsNode() {}
    LocalDeclsNode(VarDeclNode* vd, LocalDeclsNode* lds) : vd(vd), lds(lds) {}
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
    StmtNode* s;
    StmtListNode* sl;

  public:
    StmtListNode() {}
    StmtListNode(StmtNode* s, StmtListNode* sl) : s(s), sl(sl) {}
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
    LocalDeclsNode* lds;
    StmtListNode* sl;

  public:
    BlockStmtNode(LocalDeclsNode* lds, StmtListNode* sl) : lds(lds), sl(sl) {}
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
    ExprNode* e;

  public:
    ExprStmtNode(ExprNode* e = nullptr) : e(e) {}
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
    ExprNode* e;

  public:
    ReturnStmtNode(ExprNode* e = nullptr) : e(e) {}
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
    BlockStmtNode* bs;

  public:
    ElseStmtNode(BlockStmtNode* bs = nullptr) : bs(bs) {}
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
    ExprNode* e;
    BlockStmtNode* bs;
    ElseStmtNode* es;

  public:
    IfStmtNode(ExprNode* e, BlockStmtNode* bs, ElseStmtNode* es) : e(e), bs(bs), es(es) {}
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
    ExprNode* e;
    StmtNode* s;

  public:
    WhileStmtNode(ExprNode* e, StmtNode* s) : e(e), s(s) {}
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
    VarTypeNode* vt;
    TOKEN id;

  public:
    ParamNode(VarTypeNode* vt, TOKEN id) : vt(vt), id(id) {}
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
    ParamNode* p;
    ParamListNode* pl;

  public:
    ParamListNode(ParamNode* p, ParamListNode* pl = nullptr) : p(p), pl(pl) {}
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
    ParamListNode* pl;

  public:
    ParamsNode(ParamListNode* pl = nullptr) : pl(pl) {}
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
    VarTypeNode* vt;

  public:
    FunTypeNode(VarTypeNode* vt = nullptr) : vt(vt) {}
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
    FunTypeNode* ft;
    TOKEN id;
    ParamsNode* p;
    BlockStmtNode* bs;

  public:
    FunDeclNode(FunTypeNode* ft, 
                TOKEN id,
                ParamsNode* p, 
                BlockStmtNode* bs
    ) : ft(ft), id(id), p(p), bs(bs) {}
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
    VarDeclNode* vd;
    FunDeclNode* fd;

  public:
    DeclNode(VarDeclNode* vd) : vd(vd) {}
    DeclNode(FunDeclNode* fd) : fd(fd) {}
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
    DeclNode* d;
    DeclListNode* dl;

  public:
    DeclListNode(DeclNode* d, DeclListNode* dl = nullptr) : d(d), dl(dl) {}
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
    FunTypeNode* ft;
    TOKEN id;
    ParamsNode* p;

  public:
    ExternNode(FunTypeNode* ft, TOKEN id, ParamsNode* p) : ft(ft), id(id), p(p) {}
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
    ExternNode* e;
    ExternListNode* el;

public:
  ExternListNode(ExternNode* e, ExternListNode* el = nullptr) : e(e), el(el) {}
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
    ExternListNode* el;
    DeclListNode* dl;

  public:
    ProgramNode(ExternListNode* el, DeclListNode* dl) : el(el), dl(dl) {}
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
    LiteralNode* l;

  public:
    UnaryNode(TOKEN op, LiteralNode* l) : op(op), l(l) {}
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
    ExprNode* e;

  public:
    ParenthesesNode(ExprNode* e) : e(e) {}
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
    ArgsNode* a;

  public:
    CallNode(TOKEN tok, ArgsNode* a) : a(a)
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
