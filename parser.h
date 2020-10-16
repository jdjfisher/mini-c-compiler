
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
void parse();

static ProgramNode* parseProgram();
static ExternListNode* parseExternList();
static ExternNode* parseExtern();
static DeclListNode* parseDeclList();
static DeclNode* parseDecl();
static VarDeclNode* parseVarDecl();
static FunTypeNode* parseFunType();
static VarTypeNode* parseVarType();
static FunDeclNode* parseFunDecl();
static ParamsNode* parseParams();
static ParamListNode* parseParamList();
static ParamNode* parseParam();
static LocalDeclsNode* parseLocalDecls();
static StmtListNode* parseStmtList();
static StmtNode* parseStmt();
static ExprStmtNode* parseExprStmt();
static BlockStmtNode* parseBlockStmt();
static WhileStmtNode* parseWhileStmt();
static IfStmtNode* parseIfStmt();
static ElseStmtNode* parseElseStmt();
static ReturnStmtNode* parseReturnStmt();
static ArgsNode* parseArgs();
static ArgListNode* parseArgList();
static ExprNode* parseExpr();
static DisjNode* parseDisj();
static ConjNode* parseConj();
static EqualNode* parseEqual();
static OrderNode* parseOrder();
static TermNode* parseTerm();
static FactorNode* parseFactor();
static LiteralNode* parseLiteral();