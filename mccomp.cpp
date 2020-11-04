
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
#include <memory>
#include <queue>
#include <string.h>
#include <string>
#include <system_error>
#include <utility>
#include <exception>

// Application imports
#include "lexer.h"
#include "ast.h"
#include "parser.h"

// Namespaces
using namespace llvm;
using namespace llvm::sys;


FILE *pFile;

LLVMContext context;
IRBuilder<> builder(context);
std::unique_ptr<Module> module;


// Entry point
int main(int argc, char **argv) {
  if (argc == 2) 
  {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL) perror("Error opening file");
  } 
  else 
  {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // Make the module, which holds all the code.
  module = std::make_unique<Module>("mini-c", context);

  try 
  {
    // Run the parser (Incrementally Tokenise source and build AST)
    auto program = parse();

    // Print AST to terminal
    std::cout << program->to_string() << std::endl;

    // Run the codegen (Build IR from AST and perform semantic analysis)
    program->codegen();

    // Print IR to terminal
    module->print(errs(), nullptr);

    // Print out all of the generated code into a file called output.ll
    std::error_code ec;
    raw_fd_ostream dest("output.ll", ec, sys::fs::F_None);
    if (ec) 
    {
      errs() << "Could not open file: " << ec.message();
      return -1;
    }
  }
  catch (const SyntaxError& e)
  {
    std::cout << "Syntax Error: " << e.what() << std::endl;
    return -1;
  }
  catch (const SemanticError& e)
  {
    std::cout << "Semantic Error: " << e.what() << std::endl;
    return -1;
  }
  catch (const std::exception& e)
  {
    return -1;
  }

  // Close the file
  fclose(pFile); 
  return 0;
}
