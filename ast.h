
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
class BlockNode;
class StmtListNode;
class StmtNode;
class ExprStmtNode;
class WhileStmtNode;
class IfStmtNode;
class ElseStmtNode;
class ReturnStmtNode;
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
    ProgramNode(ExternListNode* el,
                DeclListNode* dl            
    ) : el(el), dl(dl) {}
    virtual llvm::Value *codegen() override {
      return NULL; // TODO: Translate to LLVM IR
    };
    virtual std::string to_string() const override {
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
  ExternListNode(ExternNode* e, 
                 ExternListNode* el = nullptr
  ) : e(e), el(el) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
      return "ExternListNode";
  };
};

// ExternNode - Class for ...
class ExternNode : public Node 
{
  private:
    FunTypeNode* ft;
    ParamsNode* p;
    TOKEN id;

  public:
    ExternNode(FunTypeNode* ft, 
              ParamsNode* p,
              TOKEN id
    ) : ft(ft), p(p), id(id) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    DeclListNode(DeclNode* d, 
                  DeclListNode* dl = nullptr
    ) : d(d), dl(dl) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "VarTypeNode";
    };
};

// FunDeclNode - Class for ...
class FunDeclNode : public Node 
{
  private:
    FunTypeNode* ft;
    ParamsNode* p;
    BlockNode* b;
    TOKEN id;

  public:
    FunDeclNode(FunTypeNode* ft, 
                ParamsNode* p, 
                BlockNode* b, 
                TOKEN id
    ) : ft(ft), p(p), b(b), id(id) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    ParamListNode(ParamNode* p,
                  ParamListNode* pl = nullptr
    ) : p(p), pl(pl) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    ParamNode(VarTypeNode* vt, 
              TOKEN id
    ) : vt(vt), id(id) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
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
    LocalDeclsNode(VarDeclNode* vd,
                  LocalDeclsNode* lds
    ) : vd(vd), lds(lds) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "LocalDeclsNode";
    };
};

// BlockNode - Class for ...
class BlockNode : public Node 
{
  private:
    LocalDeclsNode* lds;
    StmtListNode* sl;

  public:
    BlockNode(LocalDeclsNode* lds,
              StmtListNode* sl
    ) : lds(lds), sl(sl) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "BlockNode";
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
    StmtListNode(StmtNode* s,
                StmtListNode* sl
    ) : s(s), sl(sl) {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "StmtListNode";
    };
};

// StmtNode - Class for ...
class StmtNode : public Node 
{
  private:
    // TODO: ...

  public:
    StmtNode() {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "StmtNode";
    };
};

// ExprStmtNode - Class for ...
class ExprStmtNode : public Node 
{
  private:
    // TODO: ...

  public:
    ExprStmtNode() {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "ExprStmtNode";
    };
};

// WhileStmtNode - Class for ...
class WhileStmtNode : public Node 
{
  private:
    // TODO: ...

  public:
    WhileStmtNode() {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "WhileStmtNode";
    };
};

// IfStmtNode - Class for ...
class IfStmtNode : public Node 
{
  private:
    // TODO: ...

  public:
    IfStmtNode() {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "IfStmtNode";
    };
};

// ElseStmtNode - Class for ...
class ElseStmtNode : public Node 
{
  private:
    // TODO: ...

  public:
    ElseStmtNode() {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "ElseStmtNode";
    };
};

// ReturnStmtNode - Class for ...
class ReturnStmtNode : public Node 
{
  private:
    // TODO: ...

  public:
    ReturnStmtNode() {}
    virtual llvm::Value *codegen() override {
      return NULL;
    };
    virtual std::string to_string() const override {
      return "ReturnStmtNode";
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
//     virtual llvm::Value *codegen() override {
//       return NULL;
//     };
//     virtual std::string to_string() const override {
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
//     virtual llvm::Value *codegen() override {
//       return NULL;
//     };
//     virtual std::string to_string() const override {
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
//     virtual llvm::Value *codegen() override {
//       return NULL;
//     };
//     virtual std::string to_string() const override {
//       return val ? "true" : "false";
//     };
// };
