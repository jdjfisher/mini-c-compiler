
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

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// Node - Base class for all AST nodes.
class Node {
public:
  virtual ~Node() {}
  virtual llvm::Value *codegen() = 0;
  virtual std::string to_string() const;
};

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
