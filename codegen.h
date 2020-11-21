
#pragma once

// Third party imports
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"

// Standard library imports
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <string.h>
#include <string>
#include <utility>
#include <exception>
#include <sstream>

// Application imports
#include "lexer.h"

// Namespaces
using namespace llvm;

// Aliases
using Scope = std::map<std::string, AllocaInst*>;
using Scopes = std::deque<Scope>;


class SemanticError: public std::exception
{
  private:
    std::string message;
    TOKEN tok;

  public:
    SemanticError(TOKEN tok, std::string message) : tok(tok), message(message) {}
    std::string getMessage() const
    {
      std::stringstream ss;
      ss << "Semantic error <" << tok.lineNo << ":" << tok.columnNo << "> " << message;

      return ss.str();
    };
};

Type* getTypeLL(int type);
Constant* getFloatLL(float value);
Constant* getIntLL(int value);
Constant* getBoolLL(bool value);
Constant* getTypeDefaultLL(int type);
bool isBoolLL(Type* type);
bool isBoolLL(Value* value);
bool isIntLL(Type* type);
bool isIntLL(Value* value);
bool isFloatLL(Type* type);
bool isFloatLL(Value* value);
bool isVoidLL(Type* type);
bool isWiderLL(Type* t1, Type* t2);
Value* boolCastLL(Value* value);
Value* floatCastLL(Value* value);
