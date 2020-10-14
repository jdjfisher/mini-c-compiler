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

using namespace llvm;
using namespace llvm::sys;

FILE *pFile;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns one of these for known things.
enum TOKEN_TYPE {

  IDENT = -1,        // [a-zA-Z_][a-zA-Z_0-9]*
  ASSIGN = int('='), // '='

  // delimiters
  LBRA = int('{'),  // left brace
  RBRA = int('}'),  // right brace
  LPAR = int('('),  // left parenthesis
  RPAR = int(')'),  // right parenthesis
  SC = int(';'),    // semicolon
  COMMA = int(','), // comma

  // types
  INT_TOK = -2,   // "int"
  VOID_TOK = -3,  // "void"
  FLOAT_TOK = -4, // "float"
  BOOL_TOK = -5,  // "bool"

  // keywords
  EXTERN = -6,  // "extern"
  IF = -7,      // "if"
  ELSE = -8,    // "else"
  WHILE = -9,   // "while"
  RETURN = -10, // "return"
  // TRUE   = -12,     // "true"
  // FALSE   = -13,     // "false"

  // literals
  INT_LIT = -14,   // [0-9]+
  FLOAT_LIT = -15, // [0-9]+.[0-9]+
  BOOL_LIT = -16,  // "true" or "false" key words

  // logical operators
  AND = -17, // "&&"
  OR = -18,  // "||"

  // operators
  PLUS = int('+'),    // addition or unary plus
  MINUS = int('-'),   // substraction or unary negative
  ASTERIX = int('*'), // multiplication
  DIV = int('/'),     // division
  MOD = int('%'),     // modular
  NOT = int('!'),     // unary negation

  // comparison operators
  EQ = -19,      // equal
  NE = -20,      // not equal
  LE = -21,      // less than or equal to
  LT = int('<'), // less than
  GE = -23,      // greater than or equal to
  GT = int('>'), // greater than

  // special tokens
  EOF_TOK = 0, // signal end of file

  // invalid
  INVALID = -100 // signal invalid token
};

// TOKEN struct is used to keep track of information about a token
struct TOKEN {
  int type = -100;
  std::string lexeme;
  int lineNo;
  int columnNo;
};

static std::string IdentifierStr; // Filled in if IDENT
static int IntVal;                // Filled in if INT_LIT
static bool BoolVal;              // Filled in if BOOL_LIT
static float FloatVal;            // Filled in if FLOAT_LIT
static std::string StringVal;     // Filled in if String Literal
static int lineNo, columnNo;

static TOKEN returnTok(std::string lexVal, int tok_type) {
  TOKEN return_tok;
  return_tok.lexeme = lexVal;
  return_tok.type = tok_type;
  return_tok.lineNo = lineNo;
  return_tok.columnNo = columnNo - lexVal.length() - 1;
  return return_tok;
}

// Read file line by line -- or look for \n and if found add 1 to line number
// and reset column number to 0
/// gettok - Return the next token from standard input.
static TOKEN gettok() {

  static int LastChar = ' ';
  static int NextChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar)) {
    if (LastChar == '\n' || LastChar == '\r') {
      lineNo++;
      columnNo = 1;
    }
    LastChar = getc(pFile);
    columnNo++;
  }

  if (isalpha(LastChar) ||
      (LastChar == '_')) { // identifier: [a-zA-Z_][a-zA-Z_0-9]*
    IdentifierStr = LastChar;
    columnNo++;

    while (isalnum((LastChar = getc(pFile))) || (LastChar == '_')) {
      IdentifierStr += LastChar;
      columnNo++;
    }

    if (IdentifierStr == "int")
      return returnTok("int", INT_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "float")
      return returnTok("float", FLOAT_TOK);
    if (IdentifierStr == "void")
      return returnTok("void", VOID_TOK);
    if (IdentifierStr == "bool")
      return returnTok("bool", BOOL_TOK);
    if (IdentifierStr == "extern")
      return returnTok("extern", EXTERN);
    if (IdentifierStr == "if")
      return returnTok("if", IF);
    if (IdentifierStr == "else")
      return returnTok("else", ELSE);
    if (IdentifierStr == "while")
      return returnTok("while", WHILE);
    if (IdentifierStr == "return")
      return returnTok("return", RETURN);
    if (IdentifierStr == "true") {
      BoolVal = true;
      return returnTok("true", BOOL_LIT);
    }
    if (IdentifierStr == "false") {
      BoolVal = false;
      return returnTok("false", BOOL_LIT);
    }

    return returnTok(IdentifierStr.c_str(), IDENT);
  }

  if (LastChar == '=') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // EQ: ==
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("==", EQ);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("=", ASSIGN);
    }
  }

  if (LastChar == '{') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("{", LBRA);
  }
  if (LastChar == '}') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("}", RBRA);
  }
  if (LastChar == '(') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok("(", LPAR);
  }
  if (LastChar == ')') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(")", RPAR);
  }
  if (LastChar == ';') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(";", SC);
  }
  if (LastChar == ',') {
    LastChar = getc(pFile);
    columnNo++;
    return returnTok(",", COMMA);
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9]+.
    std::string NumStr;

    if (LastChar == '.') { // Floatingpoint Number: .[0-9]+
      do {
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      FloatVal = strtof(NumStr.c_str(), nullptr);
      return returnTok(NumStr, FLOAT_LIT);
    } else {
      do { // Start of Number: [0-9]+
        NumStr += LastChar;
        LastChar = getc(pFile);
        columnNo++;
      } while (isdigit(LastChar));

      if (LastChar == '.') { // Floatingpoint Number: [0-9]+.[0-9]+)
        do {
          NumStr += LastChar;
          LastChar = getc(pFile);
          columnNo++;
        } while (isdigit(LastChar));

        FloatVal = strtof(NumStr.c_str(), nullptr);
        return returnTok(NumStr, FLOAT_LIT);
      } else { // Integer : [0-9]+
        IntVal = strtod(NumStr.c_str(), nullptr);
        return returnTok(NumStr, INT_LIT);
      }
    }
  }

  if (LastChar == '&') {
    NextChar = getc(pFile);
    if (NextChar == '&') { // AND: &&
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("&&", AND);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("&", int('&'));
    }
  }

  if (LastChar == '|') {
    NextChar = getc(pFile);
    if (NextChar == '|') { // OR: ||
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("||", OR);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("|", int('|'));
    }
  }

  if (LastChar == '!') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // NE: !=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("!=", NE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("!", NOT);
      ;
    }
  }

  if (LastChar == '<') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // LE: <=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok("<=", LE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok("<", LT);
    }
  }

  if (LastChar == '>') {
    NextChar = getc(pFile);
    if (NextChar == '=') { // GE: >=
      LastChar = getc(pFile);
      columnNo += 2;
      return returnTok(">=", GE);
    } else {
      LastChar = NextChar;
      columnNo++;
      return returnTok(">", GT);
    }
  }

  if (LastChar == '/') { // could be division or could be the start of a comment
    LastChar = getc(pFile);
    columnNo++;
    if (LastChar == '/') { // definitely a comment
      do {
        LastChar = getc(pFile);
        columnNo++;
      } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

      if (LastChar != EOF)
        return gettok();
    } else
      return returnTok("/", DIV);
  }

  // Check for end of file.  Don't eat the EOF.
  if (LastChar == EOF) {
    columnNo++;
    return returnTok("0", EOF_TOK);
  }

  // Otherwise, just return the character as its ascii value.
  int ThisChar = LastChar;
  std::string s(1, ThisChar);
  LastChar = getc(pFile);
  columnNo++;
  return returnTok(s, int(ThisChar));
}

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() {

  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  TOKEN temp = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok = temp;
}

static void putBackToken(TOKEN tok) { tok_buffer.push_front(tok); }

//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

/// Node - Base class for all AST nodes.
class Node {
public:
  virtual ~Node() {}
  virtual Value *codegen() = 0;
  virtual std::string to_string() const;
};

/// VariableNode - Class for variable identifier references like sum, user_name
class VariableNode : public Node {
  std::string val;
  TOKEN tok;

public:
  VariableNode(TOKEN tok, std::string val) : val(val), tok(tok) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return val;
  };
};

/// IntNode - Class for integer literals like 1, 2, 10,
class IntNode : public Node {
  int val;
  TOKEN tok;

public:
  IntNode(TOKEN tok, int val) : val(val), tok(tok) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return std::to_string(val);
  };
};

/// BoolNode - Class for boolean literals true, false
class BoolNode : public Node {
  bool val;
  TOKEN tok;

public:
  BoolNode(TOKEN tok, bool val) : val(val), tok(tok) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return val ? "true" : "false";
  };
};

/// VarTypeNode - Class for ...
class VarTypeNode : public Node {
  std::string type;

public:
  VarTypeNode(std::string type) : type(type) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "VarTypeNode";
  };
};

/// FunTypeNode - Class for ...
class FunTypeNode : public Node {
  std::unique_ptr<VarTypeNode> vt;

public:
  FunTypeNode(std::unique_ptr<VarTypeNode> vt = nullptr) : vt(std::move(vt)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "FunTypeNode";
  };
};

/// VarDeclNode - Class for ...
class VarDeclNode : public Node {
  std::unique_ptr<VarTypeNode> vt;
  TOKEN id;

public:
  VarDeclNode(std::unique_ptr<VarTypeNode> vt, TOKEN id) : vt(std::move(vt)), id(id) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "VarDeclNode";
  };
};

/// LocalDeclsNode - Class for ...
class LocalDeclsNode : public Node {
  std::unique_ptr<VarDeclNode> vd;
  std::unique_ptr<LocalDeclsNode> lds;

public:
  LocalDeclsNode() {}
  LocalDeclsNode(std::unique_ptr<VarDeclNode> vd,
                 std::unique_ptr<LocalDeclsNode> lds
  ) : vd(std::move(vd)), lds(std::move(lds)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "LocalDeclsNode";
  };
};

/// StmtNode - Class for ...
class StmtNode : public Node {
  // TODO: ...

public:
  StmtNode() {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "StmtNode";
  };
};

/// StmtListNode - Class for ...
class StmtListNode : public Node {
  std::unique_ptr<StmtNode> s;
  std::unique_ptr<StmtListNode> sl;

public:
  StmtListNode() {}
  StmtListNode(std::unique_ptr<StmtNode> s,
                std::unique_ptr<StmtListNode> sl = nullptr
  ) : s(std::move(s)), sl(std::move(sl)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "StmtListNode";
  };
};

/// BlockNode - Class for ...
class BlockNode : public Node {
  std::unique_ptr<LocalDeclsNode> lds;
  std::unique_ptr<StmtListNode> sl;

public:
  BlockNode(std::unique_ptr<LocalDeclsNode> lds,
            std::unique_ptr<StmtListNode> sl
  ) : lds(std::move(lds)), sl(std::move(sl)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "BlockNode";
  };
};

/// ParamNode - Class for ...
class ParamNode : public Node {
  std::unique_ptr<VarTypeNode> vt;
  TOKEN id;

public:
  ParamNode(std::unique_ptr<VarTypeNode> vt, 
            TOKEN id
  ) : vt(std::move(vt)), id(id)
  {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "ParamNode";
  };
};

/// ParamListNode - Class for ...
class ParamListNode : public Node {
  std::unique_ptr<ParamNode> p;
  std::unique_ptr<ParamListNode> pl;

public:
  ParamListNode(std::unique_ptr<ParamNode> p,
                std::unique_ptr<ParamListNode> pl = nullptr
  ) : p(std::move(p)), pl(std::move(pl)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "ParamListNode";
  };
};

/// ParamsNode - Class for ...
class ParamsNode : public Node {
  std::unique_ptr<ParamListNode> pl;

public:
  ParamsNode(std::unique_ptr<ParamListNode> pl) : pl(std::move(pl)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "ParamsNode";
  };
};

/// FunDeclNode - Class for ...
class FunDeclNode : public Node {
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
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "FunDeclNode";
  };
};

/// DeclNode - Class for ...
class DeclNode : public Node {
  std::unique_ptr<VarDeclNode> vd;
  std::unique_ptr<FunDeclNode> fd;

public:
  DeclNode(std::unique_ptr<VarDeclNode> vd, 
           std::unique_ptr<FunDeclNode> fd
  ) : vd(std::move(vd)), fd(std::move(fd)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "DeclNode";
  };
};

/// DeclListNode - Class for ...
class DeclListNode : public Node {

public:
  DeclListNode() {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "DeclListNode";
  };
};

/// ExternNode - Class for ...
class ExternNode : public Node {
  std::unique_ptr<FunTypeNode> ft;
  std::unique_ptr<ParamsNode> p;
  TOKEN id;

public:
  ExternNode(std::unique_ptr<FunTypeNode> ft, 
             std::unique_ptr<ParamsNode> p,
             TOKEN id
  ) : ft(std::move(ft)), p(std::move(p)), id(id) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
      return "ExternNode";
  };
};

/// ExternListNode - Class for ...
class ExternListNode : public Node {
  std::unique_ptr<ExternNode> e;
  std::unique_ptr<ExternListNode> el;

public:
  ExternListNode(std::unique_ptr<ExternNode> e, 
                 std::unique_ptr<ExternListNode> el = nullptr
  ) : e(std::move(e)), el(std::move(el)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
      return "ExternListNode";
  };
};

// ProgramNode - Class for ...
class ProgramNode : public Node {
  std::unique_ptr<DeclListNode> dl;
  std::unique_ptr<ExternListNode> el;

public:
  ProgramNode(std::unique_ptr<DeclListNode> dl,
              std::unique_ptr<ExternListNode> el = nullptr
  ) : dl(std::move(dl)), el(std::move(el)) {}
  virtual Value *codegen() override;
  virtual std::string to_string() const override {
    return "ProgramNode";
  };
};


/* TODO: add other AST nodes as nessasary */

//===----------------------------------------------------------------------===//
// Recursive Descent Parser - Function call for each production
//===----------------------------------------------------------------------===//

static std::unique_ptr<Node> parseLiteral() {
  //
}

static std::unique_ptr<VariableNode> parseVariable() {
  //
}

static std::unique_ptr<IntNode> parseInt() {
  //
}

static std::unique_ptr<BoolNode> parseBool() {
  //
}

static std::unique_ptr<ProgramNode> parseProgram() {
  //
}

static void parse() {
  parseProgram();
}

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;

//===----------------------------------------------------------------------===//
// AST Printer
//===----------------------------------------------------------------------===//

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const Node &ast) {
  os << ast.to_string();
  return os;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main(int argc, char **argv) {
  if (argc == 2) {
    pFile = fopen(argv[1], "r");
    if (pFile == NULL)
      perror("Error opening file");
  } else {
    std::cout << "Usage: ./code InputFile\n";
    return 1;
  }

  // initialize line number and column numbers to zero
  lineNo = 1;
  columnNo = 1;

  // get the first token
  getNextToken();
  while (CurTok.type != EOF_TOK) {
    fprintf(stderr, "Token: %s with type %d\n", CurTok.lexeme.c_str(),
            CurTok.type);
    getNextToken();
  }
  fprintf(stderr, "Lexer Finished\n");

  // Make the module, which holds all the code.
  TheModule = std::make_unique<Module>("mini-c", TheContext);

  // Run the parser now.
  parse();
  fprintf(stderr, "Parsing Finished\n");

  //********************* Start printing final IR **************************
  // Print out all of the generated code into a file called output.ll
  auto Filename = "output.ll";
  std::error_code EC;
  raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

  if (EC) {
    errs() << "Could not open file: " << EC.message();
    return 1;
  }
  // TheModule->print(errs(), nullptr); // print IR to terminal
  TheModule->print(dest, nullptr);
  //********************* End printing final IR ****************************

  fclose(pFile); // close the file that contains the code that was parsed
  return 0;
}
