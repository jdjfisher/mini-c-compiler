
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

TOKEN getCurrentToken() 
{
  return CurTok;
}

TOKEN getNextToken() 
{
  if (tok_buffer.size() == 0)
    tok_buffer.push_back(lexToken());

  CurTok = tok_buffer.front();
  tok_buffer.pop_front();

  return CurTok;
}

static void putBackToken(TOKEN token) 
{ 
  // if (CurTok != NULL) 
  // {
    tok_buffer.push_front(CurTok); 
  // }

  CurTok = token;
}

void parse() 
{
  // Get the first token.
  getNextToken();

  auto p = parseProgram();
}

static ProgramNode* parseProgram() 
{
  auto el = parseExternList();
  if (!el) return nullptr;

  auto dl = parseDeclList();
  if (!dl) return nullptr;

  return new ProgramNode(el, dl);
}

static ExternListNode* parseExternList() 
{
  auto e = parseExtern();
  if (!e) return nullptr;

  auto el = parseExternList();
  if (!el) return nullptr;

  return new ExternListNode(e, el);
}

static ExternNode* parseExtern() 
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

  return new ExternNode(ft, t, p);
}

static DeclListNode* parseDeclList() 
{
  auto d = parseDecl();
  if (!d) return nullptr;

  auto dl = parseDeclList();
  if (!dl) return nullptr;

  return new DeclListNode(d, dl);
}

static DeclNode* parseDecl() 
{
  if (auto vd = parseVarDecl()) 
  {
    return new DeclNode(vd);
  }

  if (auto fd = parseFunDecl()) 
  {
    return new DeclNode(fd);
  }

  return nullptr;
}

static VarDeclNode* parseVarDecl() 
{
  auto vt = parseVarType();
  if (!vt) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consumer the IDENT token.
  getNextToken();

  return new VarDeclNode(vt, t);
}

static FunTypeNode* parseFunType() 
{
  if (CurTok.type == VOID_TOK) 
  {
    // Consume the VOID token.
    getNextToken();
    return new FunTypeNode();
  }

  return new FunTypeNode(parseVarType());
}

static VarTypeNode* parseVarType() 
{
  switch (CurTok.type)
  {
  case BOOL_TOK:
  case INT_TOK:
  case FLOAT_TOK: {
    TOKEN t = CurTok;
    // Consume the TYPE token.
    getNextToken();
    return new VarTypeNode(t);
  }
  default:
    return nullptr;
  }
}

static FunDeclNode* parseFunDecl() 
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

  auto b = parseBlockStmt();
  if (!b) return nullptr;

  return new FunDeclNode(ft, t, p, b);
}

static ParamsNode* parseParams() 
{
  auto pl = parseParamList();
  
  if (!pl && CurTok.type == VOID_TOK) 
  {
    // Consume the void token.
    getNextToken();
  }

  return new ParamsNode(pl);
}

static ParamListNode* parseParamList() 
{
  auto p = parseParam();
  if (!p) return nullptr;

  if (CurTok.type == COMMA)
  {
    // Consume the void token.
    getNextToken();

    auto pl = parseParamList();
    if (!pl) return nullptr;

    return new ParamListNode(p, pl);
  }

  return new ParamListNode(p);
}

static ParamNode* parseParam() 
{
  auto vt = parseVarType(); 

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consume the IDENT token.
  getNextToken();

  return new ParamNode(vt, t);
}

static LocalDeclsNode* parseLocalDecls() 
{
  if (auto vd = parseVarDecl()) 
  {
    auto ld = parseLocalDecls();
    if (!ld) return nullptr;

    return new LocalDeclsNode(vd, ld);
  }

  return new LocalDeclsNode();
}

static BlockStmtNode* parseBlockStmt() 
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

  return new BlockStmtNode(ld, sl);
}

static StmtListNode* parseStmtList() 
{  
  if (auto s = parseStmt())
  {
    auto sl = parseStmtList();
    if (!sl) return nullptr;

    return new StmtListNode(s, sl);
  }

  return new StmtListNode();
}

static StmtNode* parseStmt() 
{
  return nullptr;
}

static ExprStmtNode* parseExprStmt() 
{
  return nullptr;
}

static IfStmtNode* parseIfStmt() 
{
  return nullptr;
}

static WhileStmtNode* parseWhileStmt() 
{
  return nullptr;
}

static ReturnStmtNode* parseReturnStmt() 
{
  return nullptr;
}

static ArgsNode* parseArgs()
{
  return nullptr;
}

static ArgListNode* parseArgList()
{
  return nullptr;
}

static ExprNode* parseExpr()
{
  return nullptr;
}

static DisjNode* parseDisj()
{
  return nullptr;
}

static ConjNode* parseConj()
{
  return nullptr;
}

static EqualNode* parseEqual()
{
  return nullptr;
}

static OrderNode* parseOrder()
{
  return nullptr;
}

static TermNode* parseTerm()
{
  return nullptr;
}

static FactorNode* parseFactor()
{
  return nullptr;
}

static LiteralNode* parseLiteral()
{
  return nullptr;
}
