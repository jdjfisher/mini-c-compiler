
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
static BlockNode* parseBlock();
static StmtListNode* parseStmtList();
static StmtNode* parseStmt();
static ExprStmtNode* parseExprStmt();
static IfStmtNode* parseIfStmt();
static WhileStmtNode* parseWhileStmt();
static ReturnStmtNode* parseReturnStmt();