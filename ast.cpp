
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
#include "ast.h"
#include "lexer.h"

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

// ProgramNode - Class for ...
class ProgramNode : public Node 
{
  private:
    std::unique_ptr<ExternListNode> el;
    std::unique_ptr<DeclListNode> dl;

  public:
    ProgramNode(std::unique_ptr<ExternListNode> el,
                std::unique_ptr<DeclListNode> dl            
    ) : el(std::move(el)), dl(std::move(dl)) {}
    virtual llvm::Value *codegen() override {
      return NULL; // TODO: Translate to LLVM IR
    };
    virtual std::string to_string() const override {
      return "ProgramNode"; // TODO: Stringify node recursively
    };
};

// ExternListNode - Class for ...
class ExternListNode : public Node {
private:
  std::unique_ptr<ExternNode> e;
  std::unique_ptr<ExternListNode> el;

public:
  ExternListNode(std::unique_ptr<ExternNode> e, 
                 std::unique_ptr<ExternListNode> el = nullptr
  ) : e(std::move(e)), el(std::move(el)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
      return "ExternListNode";
  };
};

// ExternNode - Class for ...
class ExternNode : public Node {
private:
  std::unique_ptr<FunTypeNode> ft;
  std::unique_ptr<ParamsNode> p;
  TOKEN id;

public:
  ExternNode(std::unique_ptr<FunTypeNode> ft, 
             std::unique_ptr<ParamsNode> p,
             TOKEN id
  ) : ft(std::move(ft)), p(std::move(p)), id(id) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
      return "ExternNode";
  };
};

// DeclListNode - Class for ...
class DeclListNode : public Node {
private:
  std::unique_ptr<DeclNode> d;
  std::unique_ptr<DeclListNode> dl;

public:
  DeclListNode(std::unique_ptr<DeclNode> d, 
                 std::unique_ptr<DeclListNode> dl = nullptr
  ) : d(std::move(d)), dl(std::move(dl)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "DeclListNode";
  };
};

// DeclNode - Class for ...
class DeclNode : public Node {
private:
  std::unique_ptr<VarDeclNode> vd;
  std::unique_ptr<FunDeclNode> fd;

public:
  DeclNode(std::unique_ptr<VarDeclNode> vd) : vd(std::move(vd)) {}
  DeclNode(std::unique_ptr<FunDeclNode> fd) : fd(std::move(fd)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "DeclNode";
  };
};

// VarDeclNode - Class for ...
class VarDeclNode : public Node {
private:
  std::unique_ptr<VarTypeNode> vt;
  TOKEN id;

public:
  VarDeclNode(std::unique_ptr<VarTypeNode> vt, TOKEN id) : vt(std::move(vt)), id(id) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "VarDeclNode";
  };
};

// FunTypeNode - Class for ...
class FunTypeNode : public Node {
private:
  std::unique_ptr<VarTypeNode> vt;

public:
  FunTypeNode(std::unique_ptr<VarTypeNode> vt = nullptr) : vt(std::move(vt)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "FunTypeNode";
  };
};

// VarTypeNode - Class for ...
class VarTypeNode : public Node {
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
class FunDeclNode : public Node {
private:
  std::unique_ptr<FunTypeNode> ft;
  std::unique_ptr<ParamsNode> p;
  std::unique_ptr<BlockNode> b;
  TOKEN id;

public:
  FunDeclNode(std::unique_ptr<FunTypeNode> ft, 
              std::unique_ptr<ParamsNode> p, 
              std::unique_ptr<BlockNode> b, 
              TOKEN id
  ) : ft(std::move(ft)), p(std::move(p)), b(std::move(b)), id(id) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "FunDeclNode";
  };
};

// ParamsNode - Class for ...
class ParamsNode : public Node {
private:
  std::unique_ptr<ParamListNode> pl;

public:
  ParamsNode(std::unique_ptr<ParamListNode> pl = nullptr) : pl(std::move(pl)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "ParamsNode";
  };
};


// ParamListNode - Class for ...
class ParamListNode : public Node {
private:
  std::unique_ptr<ParamNode> p;
  std::unique_ptr<ParamListNode> pl;

public:
  ParamListNode(std::unique_ptr<ParamNode> p,
                std::unique_ptr<ParamListNode> pl = nullptr
  ) : p(std::move(p)), pl(std::move(pl)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "ParamListNode";
  };
};

// ParamNode - Class for ...
class ParamNode : public Node {
private:
  std::unique_ptr<VarTypeNode> vt;
  TOKEN id;

public:
  ParamNode(std::unique_ptr<VarTypeNode> vt, 
            TOKEN id
  ) : vt(std::move(vt)), id(id) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "ParamNode";
  };
};

// LocalDeclsNode - Class for ...
class LocalDeclsNode : public Node {
private:
  std::unique_ptr<VarDeclNode> vd;
  std::unique_ptr<LocalDeclsNode> lds;

public:
  LocalDeclsNode() {}
  LocalDeclsNode(std::unique_ptr<VarDeclNode> vd,
                 std::unique_ptr<LocalDeclsNode> lds
  ) : vd(std::move(vd)), lds(std::move(lds)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "LocalDeclsNode";
  };
};

// BlockNode - Class for ...
class BlockNode : public Node {
private:
  std::unique_ptr<LocalDeclsNode> lds;
  std::unique_ptr<StmtListNode> sl;

public:
  BlockNode(std::unique_ptr<LocalDeclsNode> lds,
            std::unique_ptr<StmtListNode> sl
  ) : lds(std::move(lds)), sl(std::move(sl)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "BlockNode";
  };
};

// StmtListNode - Class for ...
class StmtListNode : public Node {
private:
  std::unique_ptr<StmtNode> s;
  std::unique_ptr<StmtListNode> sl;

public:
  StmtListNode() {}
  StmtListNode(std::unique_ptr<StmtNode> s,
               std::unique_ptr<StmtListNode> sl
  ) : s(std::move(s)), sl(std::move(sl)) {}
  virtual llvm::Value *codegen() override {
    return NULL;
  };
  virtual std::string to_string() const override {
    return "StmtListNode";
  };
};

// StmtNode - Class for ...
class StmtNode : public Node {
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
class ExprStmtNode : public Node {
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
class WhileStmtNode : public Node {
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
class IfStmtNode : public Node {
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
class ElseStmtNode : public Node {
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
class ReturnStmtNode : public Node {
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

// VariableNode - Class for variable identifier references like sum, user_name
// class VariableNode : public Node {
// private:
//   std::string val;
//   TOKEN tok;

// public:
//   VariableNode(TOKEN tok, std::string val) : val(val), tok(tok) {}
//   virtual llvm::Value *codegen() override {
//     return NULL;
//   };
//   virtual std::string to_string() const override {
//     return val;
//   };
// };

// // IntNode - Class for integer literals like 1, 2, 10,
// class IntNode : public Node {
// private:
//   int val;
//   TOKEN tok;

// public:
//   IntNode(TOKEN tok, int val) : val(val), tok(tok) {}
//   virtual llvm::Value *codegen() override {
//     return NULL;
//   };
//   virtual std::string to_string() const override {
//     return std::to_string(val);
//   };
// };

// // BoolNode - Class for boolean literals true, false
// class BoolNode : public Node {
// private:
//   bool val;
//   TOKEN tok;

// public:
//   BoolNode(TOKEN tok, bool val) : val(val), tok(tok) {}
//   virtual llvm::Value *codegen() override {
//     return NULL;
//   };
//   virtual std::string to_string() const override {
//     return val ? "true" : "false";
//   };
// };
