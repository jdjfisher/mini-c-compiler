
#pragma once

// Standard library imports
#include <memory>
#include <string>

// Application imports
#include "ast.h"
#include "lexer.h"


class SyntaxError: public std::exception
{
  private:
    std::string m;
    // TOKEN t;

  public:
    SyntaxError() {}
    SyntaxError(std::string m) : m(m) {}
    virtual const char* what() const throw()
    {
      return m.c_str();
    }
};


TOKEN getCurrentToken();
TOKEN getNextToken();
std::unique_ptr<ProgramNode> parse();


static std::unique_ptr<ProgramNode> parseProgram();
static std::vector<std::unique_ptr<FunSignNode>> parseExterns();
static std::unique_ptr<FunSignNode> parseExtern();
static std::unique_ptr<FunSignNode> parseFunSign();
static std::vector<std::unique_ptr<DeclNode>> parseDecls();
static std::unique_ptr<DeclNode> parseDecl();
static std::unique_ptr<VarDeclNode> parseVarDecl();
static std::unique_ptr<FunDeclNode> parseFunDecl();
static std::unique_ptr<ParamNode> parseParam();
static std::vector<std::unique_ptr<VarDeclNode>> parseLocalDecls();
static std::vector<std::unique_ptr<StmtNode>> parseStmtList();
static std::unique_ptr<StmtNode> parseStmt();
static std::unique_ptr<BlockStmtNode> parseBlockStmt();
static std::unique_ptr<WhileStmtNode> parseWhileStmt();
static std::unique_ptr<IfStmtNode> parseIfStmt();
static std::unique_ptr<ReturnStmtNode> parseReturnStmt();
static std::unique_ptr<ExprStmtNode> parseExprStmt();
static std::unique_ptr<ExprNode> parseExpr();
static std::unique_ptr<ExprNode> parseDisj();
static std::unique_ptr<ExprNode> parseConj();
static std::unique_ptr<ExprNode> parseEqual();
static std::unique_ptr<ExprNode> parseOrder();
static std::unique_ptr<ExprNode> parseTerm();
static std::unique_ptr<ExprNode> parseFactor();
static std::unique_ptr<ExprNode> parseLiteral();
static TOKEN parseVarType();
static TOKEN parseFunType();

