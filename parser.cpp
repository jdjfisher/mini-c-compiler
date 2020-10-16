
// Application imports
#include "parser.h"
#include "ast.h"
#include "lexer.h"

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
/// token the parser is looking at.  getNextToken reads another token from the
/// lexer and updates CurTok with its results.
static TOKEN CurTok;
static std::deque<TOKEN> tok_buffer;

static TOKEN getNextToken() 
{
  if (tok_buffer.size() == 0)
    tok_buffer.push_back(gettok());

  CurTok = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok;
}

static void putBackToken(TOKEN tok) 
{ 
  // if (CurTok != NULL) 
  {
    tok_buffer.push_front(CurTok); 
  }

  CurTok = tok;
}

static void parse() 
{
  // Get the first token.
  getNextToken();

  parseProgram();
}

static std::unique_ptr<ProgramNode> parseProgram() 
{
  auto el = parseExternList();
  if (!el) return nullptr;

  auto dl = parseDeclList();
  if (!dl) return nullptr;

  return std::make_unique<ProgramNode>(std::move(el), std::move(dl));
}

static std::unique_ptr<ExternListNode> parseExternList() 
{
  auto e = parseExtern();
  if (!e) return nullptr;

  auto el = parseExternList();
  if (!el) return nullptr;

  return std::make_unique<ExternListNode>(std::move(e), std::move(el));
}

static std::unique_ptr<ExternNode> parseExtern() 
{
  if (CurTok.type != EXTERN) return nullptr;
  // Consume the EXTERN token.
  getNextToken();

  auto ft = parseFunType();
  if (!ft) return nullptr; 

  if (CurTok.type != IDENT) return nullptr; 
  TOKEN t = CurTok;
  // Consume the IDENT token.
  getNextToken();

  if (CurTok.type != LPAR) return nullptr; 
  // Consume the ( token.
  getNextToken();

  auto p = parseParams();
  if (!p) return nullptr; 

  if (CurTok.type != RPAR) return nullptr; 
  // Consume the ) token.
  getNextToken();

  if (CurTok.type != SC) return nullptr; 
  // Consume the ; token.
  getNextToken();

  return std::make_unique<ExternNode>(
    std::move(ft), std::move(p), t
  );
}

static std::unique_ptr<DeclListNode> parseDeclList() 
{
  auto d = parseDecl();
  if (!d) return nullptr;

  auto dl = parseDeclList();
  if (!dl) return nullptr;

  return std::make_unique<DeclListNode>(std::move(d), std::move(dl));
}

static std::unique_ptr<DeclNode> parseDecl() 
{
  if (auto vd = parseVarDecl()) 
  {
    return std::make_unique<DeclNode>(std::move(vd));
  }

  if (auto fd = parseFunDecl()) 
  {
    return std::make_unique<DeclNode>(std::move(fd));
  }

  return nullptr;
}

static std::unique_ptr<VarDeclNode> parseVarDecl() 
{
  auto vt = parseVarType();
  if (!vt) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consumer the IDENT token.
  getNextToken();

  return std::make_unique<VarDeclNode>(std::move(vt), t);
}

static std::unique_ptr<FunTypeNode> parseFunType() 
{
  if (CurTok.type == VOID_TOK) 
  {
    // Consume the VOID token.
    getNextToken();
    return std::make_unique<FunTypeNode>();
  }

  return std::make_unique<FunTypeNode>(parseVarType());
}

static std::unique_ptr<VarTypeNode> parseVarType() 
{
  switch (CurTok.type)
  {
  case BOOL_TOK:
  case INT_TOK:
  case FLOAT_TOK: {
    TOKEN t = CurTok;
    // Consume the TYPE token.
    getNextToken();
    return std::make_unique<VarTypeNode>(t);
  }
  default:
    return nullptr;
  }
}

static std::unique_ptr<FunDeclNode> parseFunDecl() 
{
  auto ft = parseFunType();
  if (!ft) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consumer the IDENT token.
  getNextToken();

  if (CurTok.type != LPAR) return nullptr;
  // Consumer the ( token.
  getNextToken();

  auto p = parseParams();
  if (!p) return nullptr;

  if (CurTok.type != RPAR) return nullptr;
  // Consumer the ) token.
  getNextToken(); 

  auto b = parseBlock();
  if (!b) return nullptr;

  return std::make_unique<FunDeclNode>(
    std::move(ft), std::move(p), std::move(b), t
  );
}

static std::unique_ptr<ParamsNode> parseParams() 
{
  auto pl = parseParamList();
  
  if (!pl && CurTok.type == VOID_TOK) 
  {
    // Consume the void token.
    getNextToken();
  }

  return std::make_unique<ParamsNode>(std::move(pl));
}

static std::unique_ptr<ParamListNode> parseParamList() 
{
  auto p = parseParam();
  if (!p) return nullptr;

  if (CurTok.type == COMMA)
  {
    // Consume the void token.
    getNextToken();

    auto pl = parseParamList();
    if (!pl) return nullptr;

    return std::make_unique<ParamListNode>(std::move(p), std::move(pl));
  }

  return std::make_unique<ParamListNode>(std::move(p));
}

static std::unique_ptr<ParamNode> parseParam() 
{
  auto vt = parseVarType(); 

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consume the IDENT token.
  getNextToken();

  return std::make_unique<ParamNode>(std::move(vt), t);
}

static std::unique_ptr<LocalDeclsNode> parseLocalDecls() 
{
  if (auto vd = parseVarDecl()) 
  {
    auto ld = parseLocalDecls();
    if (!ld) return nullptr;

    return std::make_unique<LocalDeclsNode>(std::move(vd), std::move(ld));
  }

  return std::make_unique<LocalDeclsNode>();
}

static std::unique_ptr<BlockNode> parseBlock() 
{
  if (CurTok.type != LBRA) return nullptr;
  // Consumer the { token.
  getNextToken();

  auto ld = parseLocalDecls();
  if (!ld) return nullptr;

  auto sl = parseStmtList();
  if (!sl) return nullptr;

  if (CurTok.type != RBRA) return nullptr;
  // Consumer the } token.
  getNextToken();

  return std::make_unique<BlockNode>(std::move(ld), std::move(sl));
}

static std::unique_ptr<StmtListNode> parseStmtList() 
{  
  if (auto s = parseStmt())
  {
    auto sl = parseStmtList();
    if (!sl) return nullptr;

    return std::make_unique<StmtListNode>(std::move(s), std::move(sl));
  }

  return std::make_unique<StmtListNode>();
}

static std::unique_ptr<StmtNode> parseStmt() 
{
  return nullptr;
}

static std::unique_ptr<ExprStmtNode> parseExprStmt() 
{
  return nullptr;
}

static std::unique_ptr<IfStmtNode> parseIfStmt() 
{
  return nullptr;
}

static std::unique_ptr<WhileStmtNode> parseWhileStmt() 
{
  return nullptr;
}

static std::unique_ptr<ReturnStmtNode> parseReturnStmt() 
{
  return nullptr;
}