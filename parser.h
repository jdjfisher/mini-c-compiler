
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

TOKEN getCurrentToken();
TOKEN getNextToken();
std::unique_ptr<ProgramNode> parse();

static std::unique_ptr<ProgramNode> parseProgram();
static std::unique_ptr<ExternListNode> parseExternList();
static std::unique_ptr<ExternNode> parseExtern();
static std::unique_ptr<DeclListNode> parseDeclList();
static std::unique_ptr<DeclNode> parseDecl();
static std::unique_ptr<VarDeclNode> parseVarDecl();
static std::unique_ptr<FunDeclNode> parseFunDecl();
static std::unique_ptr<VarTypeNode> parseVarType();
static std::unique_ptr<FunTypeNode> parseFunType();
static std::unique_ptr<ParamsNode> parseParams();
static std::unique_ptr<ParamListNode> parseParamList();
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
static std::unique_ptr<ArgsNode> parseArgs();
static std::unique_ptr<ArgListNode> parseArgList();
static std::unique_ptr<ExprNode> parseExpr();
static std::unique_ptr<DisjNode> parseDisj();
static std::unique_ptr<ConjNode> parseConj();
static std::unique_ptr<EqualNode> parseEqual();
static std::unique_ptr<OrderNode> parseOrder();
static std::unique_ptr<TermNode> parseTerm();
static std::unique_ptr<FactorNode> parseFactor();
static std::unique_ptr<LiteralNode> parseLiteral();