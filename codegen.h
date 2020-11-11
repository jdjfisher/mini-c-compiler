
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


class SemanticError: public std::exception
{
  private:
    std::string message;
    TOKEN tok;

  public:
    SemanticError(std::string message) : message(message) {}
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
bool isBoolLL(Value* value);
bool isFloatLL(Value* value);
Value* boolCastLL(Value* value);
Value* floatCastLL(Value* value);