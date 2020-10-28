
#pragma once

// Standard library imports
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

// Application imports
#include "ast.h"
#include "lexer.h"

TOKEN getCurrentToken();
TOKEN getNextToken();
std::unique_ptr<ProgramNode> parse();

static std::unique_ptr<ProgramNode> parseProgram();
static std::unique_ptr<ExternsNode> parseExterns();
static std::unique_ptr<FunSignNode> parseExtern();
static std::unique_ptr<FunSignNode> parseFunSign();
static std::unique_ptr<DeclsNode> parseDecls();
static std::unique_ptr<DeclNode> parseDecl();
static std::unique_ptr<VarDeclNode> parseVarDecl();
static std::unique_ptr<FunDeclNode> parseFunDecl();
static std::unique_ptr<ParamNode> parseParam();
static std::unique_ptr<LocalDeclsNode> parseLocalDecls();
static std::unique_ptr<StmtListNode> parseStmtList();
static std::unique_ptr<StmtNode> parseStmt();
static std::unique_ptr<BlockStmtNode> parseBlockStmt();
static std::unique_ptr<WhileStmtNode> parseWhileStmt();
static std::unique_ptr<IfStmtNode> parseIfStmt();
static std::unique_ptr<ElseStmtNode> parseElseStmt();
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
static std::unique_ptr<TOKEN> parseVarType();
static std::unique_ptr<TOKEN> parseFunType();