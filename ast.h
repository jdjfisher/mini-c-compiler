
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

// Forward declarations
class ProgramNode;
class ExternListNode;
class ExternNode;
class DeclListNode;
class DeclNode;
class VarDeclNode;
class FunTypeNode;
class VarTypeNode;
class FunDeclNode;
class ParamsNode;
class ParamListNode;
class ParamNode;
class LocalDeclsNode;
class StmtListNode;
class StmtNode;
class ExprStmtNode;
class BlockStmtNode;
class WhileStmtNode;
class IfStmtNode;
class ElseStmtNode;
class ReturnStmtNode;
class ArgsNode;
class ArgListNode;
class ExprNode;
class DisjNode;
class ConjNode;
class EqualNode;
class OrderNode;
class TermNode;
class FactorNode;
class LiteralNode;
// class IntNode;
// class BoolNode;
// class VariableNode;

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
      return "ProgramNode"; // TODO: Stringify node recursively
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
      return "ExternListNode";
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
        return "ExternNode";
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
      return "DeclListNode";
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
      return "DeclNode";
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
      return "VarDeclNode";
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
      return "FunTypeNode";
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
      return "VarTypeNode";
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
      return "FunDeclNode";
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
      return "ParamsNode";
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
      return "ParamListNode";
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
      return "ParamNode";
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
      return "LocalDeclsNode";
    };
};

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
      return "StmtListNode";
    };
};

// StmtNode - Class for ...
class StmtNode : public Node {};

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
      return "ExprStmtNode";
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
      return "BlockStmtNode";
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
      return "WhileStmtNode";
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
      return "IfStmtNode";
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
      return "ElseStmtNode";
    };
};

// ReturnStmtNode - Class for ...
class ReturnStmtNode : public StmtNode 
{
  private:
    ExprNode* e;

  public:
    ReturnStmtNode(ExprNode* e) : e(e) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "ReturnStmtNode";
    };
};

// ArgsNode - Class for ...
class ArgsNode : public Node 
{
  private:
    ArgListNode* al;

  public:
    ArgsNode(ArgListNode* al) : al(al) {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "ArgsNode";
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
      return "ArgListNode";
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
      return "ExprNode";
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
      return "DisjNode";
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
      return "ConjNode";
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
      return "EqualNode";
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
      return "OrderNode";
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
      return "TermNode";
    };
};

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
      return "FactorNode";
    };
};

// LiteralNode - Class for ...
class LiteralNode : public Node 
{
  private:
    // TODO: ...

  public:
    LiteralNode() {}
    virtual llvm::Value *codegen() override
    {
      return NULL;
    };
    virtual std::string to_string() const override
    {
      return "LiteralNode";
    };
};

// // VariableNode - Class for variable identifier references like sum, user_name
// class VariableNode : public Node 
// {
//   private:
//     std::string val;
//     TOKEN tok;

//   public:
//     VariableNode(TOKEN tok, std::string val) : val(val), tok(tok) {}
//     virtual llvm::Value *codegen() override
//     {
//       return NULL;
//     };
//     virtual std::string to_string() const override
//     {
//       return val;
//     };
// };

// // IntNode - Class for integer literals like 1, 2, 10,
// class IntNode : public Node 
// {
//   private:
//     int val;
//     TOKEN tok;

//   public:
//     IntNode(TOKEN tok, int val) : val(val), tok(tok) {}
//     virtual llvm::Value *codegen() override
//     {
//       return NULL;
//     };
//     virtual std::string to_string() const override
//     {
//       return std::to_string(val);
//     };
// };

// // BoolNode - Class for boolean literals true, false
// class BoolNode : public Node 
// {
//   private:
//     bool val;
//     TOKEN tok;

//   public:
//     BoolNode(TOKEN tok, bool val) : val(val), tok(tok) {}
//     virtual llvm::Value *codegen() override
//     {
//       return NULL;
//     };
//     virtual std::string to_string() const override
//     {
//       return val ? "true" : "false";
//     };
// };
