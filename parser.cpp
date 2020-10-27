
// Application imports
#include "parser.h"
#include "ast.h"
#include "lexer.h"

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

static TOKEN CurTok;
static std::deque<TOKEN> TokBuffer;

TOKEN getCurrentToken() 
{
  return CurTok;
}

TOKEN getNextToken() 
{
  if (TokBuffer.size() == 0)
    TokBuffer.push_back(lexToken());

  CurTok = TokBuffer.front();
  TokBuffer.pop_front();

  return CurTok;
}

static void putBackToken(TOKEN token) 
{ 
  TokBuffer.push_front(CurTok); 

  CurTok = token;
}

std::unique_ptr<ProgramNode> parse() 
{
  // Get the first token.
  getNextToken();

  return parseProgram();
}

static std::unique_ptr<ProgramNode> parseProgram() 
{
  std::unique_ptr<ExternListNode> el;

  if (CurTok.type == EXTERN)
  {
    el = parseExternList();
    if (!el) return nullptr;
  }

  auto dl = parseDeclList();
  if (!dl) return nullptr;

  return std::make_unique<ProgramNode>(std::move(el), std::move(dl));
}

static std::unique_ptr<ExternListNode> parseExternList() 
{
  auto e = parseExtern();
  if (!e) return nullptr;

  if (CurTok.type == EXTERN)
  {
    auto el = parseExternList();
    if (!el) return nullptr;

    return std::make_unique<ExternListNode>(std::move(e), std::move(el));
  }

  return std::make_unique<ExternListNode>(std::move(e));
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

  return std::make_unique<ExternNode>(std::move(p), *ft, t);
}

static std::unique_ptr<DeclListNode> parseDeclList() 
{
  auto d = parseDecl();
  if (!d) return nullptr;

  if (CurTok.type == VOID_TOK  || CurTok.type == INT_TOK ||
      CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    auto dl = parseDeclList();
    if (!dl) return nullptr;

    return std::make_unique<DeclListNode>(std::move(d), std::move(dl));
  }

  return std::make_unique<DeclListNode>(std::move(d));
}

static std::unique_ptr<DeclNode> parseDecl()
{
  TOKEN t1 = getCurrentToken();
  TOKEN t2 = getNextToken();
  TOKEN t3 = getNextToken();
  putBackToken(t2);
  putBackToken(t1);

  if (t3.type == SC)
  {
    auto vd = parseVarDecl();
    if (!vd) return nullptr;

    return std::make_unique<DeclNode>(std::move(vd));
  }
  else if (t3.type == LPAR)
  {
    auto fd = parseFunDecl();
    if (!fd) return nullptr;

    return std::make_unique<DeclNode>(std::move(fd)); 
  }

  return nullptr;
}

static std::unique_ptr<VarDeclNode> parseVarDecl() 
{
  auto vt = parseVarType();
  if (!vt) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  // Consume the IDENT token.
  TOKEN t = CurTok;
  getNextToken();

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  return std::make_unique<VarDeclNode>(*vt, t);
}

static std::unique_ptr<FunDeclNode> parseFunDecl() 
{
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

  auto bs = parseBlockStmt();
  if (!bs) return nullptr;

  return std::make_unique<FunDeclNode>(std::move(p), std::move(bs), *ft, t);
}

static std::unique_ptr<ParamsNode> parseParams() 
{
  std::vector<std::unique_ptr<ParamNode>> params;

  if (CurTok.type == VOID_TOK) 
  {
    // Consume the void token.
    getNextToken();
  }
  else
  {
    while (CurTok.type == INT_TOK || 
           CurTok.type == FLOAT_TOK || 
           CurTok.type == BOOL_TOK)
    {
      auto p = parseParam();
      if (!p) return nullptr;

      if (CurTok.type != COMMA) break;
      
      // Consume the , token.
      getNextToken();
    }
  }

  return std::make_unique<ParamsNode>(std::move(params));
}

static std::unique_ptr<ParamNode> parseParam() 
{
  auto vt = parseVarType(); 
  if (!vt) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consume the IDENT token.
  getNextToken();

  return std::make_unique<ParamNode>(*vt, t);
}

static std::unique_ptr<LocalDeclsNode> parseLocalDecls() 
{
  if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    auto vd = parseVarDecl();
    if (!vd) return nullptr;

    auto ld = parseLocalDecls();
    if (!ld) return nullptr;

    return std::make_unique<LocalDeclsNode>(std::move(vd), std::move(ld));
  }
  
  return std::make_unique<LocalDeclsNode>();
}

static std::unique_ptr<StmtListNode> parseStmtList() 
{  
  switch (CurTok.type)
  {
    case LBRA:
    case IF:
    case WHILE:
    case RETURN:
    case SC:     // FIRST(<expr_stmt>)
    case IDENT:  // FIRST(<expr>)
    case MINUS:
    case NOT:
    case INT_LIT:
    case FLOAT_LIT:
    case BOOL_LIT:
    {
      auto s = parseStmt();
      if (!s) return nullptr;
    
      auto sl = parseStmtList();
      if (!sl) return nullptr;

      return std::make_unique<StmtListNode>(std::move(s), std::move(sl));
    }
    default:
      return std::make_unique<StmtListNode>();
  }
}

static std::unique_ptr<StmtNode> parseStmt() 
{
  switch (CurTok.type)
  {
    case LBRA:
    {
      return parseBlockStmt();
    }
    case IF:
    {
      return parseIfStmt();
    }
    case WHILE:
    {
      return parseWhileStmt();
    }
    case RETURN:
    {
      return parseReturnStmt();
    }
    case IDENT: // FIRST(expr)
    case MINUS:
    case NOT:
    case INT_LIT:
    case FLOAT_LIT:
    case BOOL_LIT:
    case SC:
    {
      return parseExprStmt();
    }
    default:
      return nullptr;
  }
}

static std::unique_ptr<BlockStmtNode> parseBlockStmt() 
{
  if (CurTok.type != LBRA) return nullptr;
  // Consume the { token.
  getNextToken();

  auto ld = parseLocalDecls();
  if (!ld) return nullptr;

  auto sl = parseStmtList();
  if (!sl) return nullptr;

  if (CurTok.type != RBRA) return nullptr;
  // Consume the } token.
  getNextToken();

  return std::make_unique<BlockStmtNode>(std::move(ld), std::move(sl));
}

static std::unique_ptr<WhileStmtNode> parseWhileStmt() 
{
  if (CurTok.type != WHILE) return nullptr;
  // Consume the WHILE token.
  getNextToken();

  if (CurTok.type != LPAR) return nullptr;
  // Consume the ( token.
  getNextToken();

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type != RPAR) return nullptr;
  // Consume the ) token.
  getNextToken();

  auto s = parseStmt();
  if (!s) return nullptr;

  return std::make_unique<WhileStmtNode>(std::move(e), std::move(s));
}

static std::unique_ptr<IfStmtNode> parseIfStmt() 
{
  if (CurTok.type != IF) return nullptr;
  // Consume the IF token.
  getNextToken();

  if (CurTok.type != LPAR) return nullptr;
  // Consume the ( token.
  getNextToken();

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type != RPAR) return nullptr;
  // Consume the ) token.
  getNextToken();

  auto bs = parseBlockStmt();
  if (!bs) return nullptr;

  auto es = parseElseStmt();
  if (!es) return nullptr;

  return std::make_unique<IfStmtNode>(std::move(e), std::move(bs), std::move(es));
}

static std::unique_ptr<ElseStmtNode> parseElseStmt() 
{
  if (CurTok.type == ELSE)
  {
    // Consume the ELSE token.
    getNextToken();

    auto bs = parseBlockStmt();
    if (!bs) return nullptr;

    return std::make_unique<ElseStmtNode>(std::move(bs));
  } 

  return std::make_unique<ElseStmtNode>();
}

static std::unique_ptr<ReturnStmtNode> parseReturnStmt() 
{
  if (CurTok.type != RETURN) return nullptr;
  // Consume the RETURN token.
  getNextToken();

  if (CurTok.type == SC)
  {
    // Consume the ; token.
    getNextToken();

    return std::make_unique<ReturnStmtNode>();
  }

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  return std::make_unique<ReturnStmtNode>(std::move(e));
}

static std::unique_ptr<ExprStmtNode> parseExprStmt() 
{
  if (CurTok.type == SC)
  {
    // Consume the ; token.
    getNextToken();
  
    return std::make_unique<ExprStmtNode>();
  }

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  return std::make_unique<ExprStmtNode>(std::move(e));
}

static std::unique_ptr<ArgsNode> parseArgs()
{
  std::vector<std::unique_ptr<ExprNode>> args;
  
  while (true)
  {
    switch (CurTok.type)
    {
      case IDENT:  // FIRST(<expr>)
      case MINUS:
      case NOT:
      case INT_LIT:
      case FLOAT_LIT:
      case BOOL_LIT:
      {
        auto e = parseExpr();
        if (!e) return NULL;

        args.push_back(std::move(e));

        if (CurTok.type == COMMA)
        {
          // Consume the , token.
          getNextToken();

          // Break from the switch and cycle the loop again
          break;
        }

        // Fall to the default case
      }
      default:
        return std::make_unique<ArgsNode>(std::move(args));
    }
  }
}

static std::unique_ptr<ExprNode> parseExpr()
{
  // Store the current token, look-ahead at the next.
  TOKEN ot = CurTok;
  TOKEN ct = getNextToken();

  if (ot.type == IDENT && ct.type == ASSIGN)
  {
    // Consume the = token.
    getNextToken();

    auto e = parseExpr();
    if (!e) return nullptr;

    return std::make_unique<AssignNode>(ot, std::move(e)); 
  }

  // Revert the look-ahead
  putBackToken(ot);

  return parseDisj();
}

static std::unique_ptr<ExprNode> parseDisj()
{
  auto c = parseConj();
  if (!c) return nullptr;

  if (CurTok.type == OR) 
  {
    // Consume the || token.
    TOKEN op = CurTok;
    getNextToken();

    auto d = parseDisj();
    if (!d) return nullptr;

    return std::make_unique<BinOpNode>(std::move(c), std::move(d), op);
  }

  return std::move(c);
}

static std::unique_ptr<ExprNode> parseConj()
{
  auto e = parseEqual();
  if (!e) return nullptr;

  if (CurTok.type == AND) 
  {
    // Consume the && token.
    TOKEN op = CurTok;
    getNextToken();

    auto c = parseConj();
    if (!c) return nullptr;

    return std::make_unique<BinOpNode>(std::move(e), std::move(c), op);
  }

  return std::move(e);
}

static std::unique_ptr<ExprNode> parseEqual()
{
  auto o = parseOrder();
  if (!o) return nullptr;

  if (CurTok.type == EQ || CurTok.type == NE) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto e = parseEqual();
    if (!e) return nullptr;

    return std::make_unique<BinOpNode>(std::move(o), std::move(e), op);
  }

  return std::move(o);
}

static std::unique_ptr<ExprNode> parseOrder()
{
  auto t = parseTerm();
  if (!t) return nullptr;

  if (CurTok.type == LE || CurTok.type == LT || 
      CurTok.type == GE || CurTok.type == GT
  ) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto o = parseOrder();
    if (!o) return nullptr;

    return std::make_unique<BinOpNode>(std::move(t), std::move(o), op);
  }

  return std::move(t);
}

static std::unique_ptr<ExprNode> parseTerm()
{
  auto f = parseFactor();
  if (!f) return nullptr;

  if (CurTok.type == PLUS || CurTok.type == MINUS) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto t = parseTerm();
    if (!t) return nullptr;

    return std::make_unique<BinOpNode>(std::move(f), std::move(t), op);
  }

  return std::move(f);
}

static std::unique_ptr<ExprNode> parseFactor()
{
  auto l = parseLiteral();
  if (!l) return nullptr;

  if (CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto f = parseFactor();
    if (!f) return nullptr;

    return std::make_unique<BinOpNode>(std::move(l), std::move(f), op);
  }

  return std::move(l);
}

static std::unique_ptr<ExprNode> parseLiteral()
{
  switch (CurTok.type)
  {
    case MINUS:
    case NOT:
    {
      // Consume the token.
      TOKEN op = CurTok;
      getNextToken();

      auto l = parseLiteral();
      if (!l) return nullptr; 

      return std::make_unique<UnaryNode>(op, std::move(l));
    }
    case LPAR:
    {
      // Consume the ( token.
      getNextToken();

      auto e = parseExpr();
      if (!e) return nullptr;

      if (CurTok.type != RPAR) return nullptr;
      // Consume the ) token.
      getNextToken();

      return std::move(e); // TODO: sort
    }
    case IDENT:
    {
      // Consume the IDENT token.
      TOKEN id = CurTok;
      getNextToken();

      if (CurTok.type == LPAR)
      {
        // Consume the ( token.
        getNextToken();

        auto a = parseArgs();
        if (!a) return nullptr;

        if (CurTok.type != RPAR) return nullptr;
        // Consume the ( token.
        getNextToken();

        return std::make_unique<CallNode>(id, std::move(a));
      }

      return std::make_unique<VariableNode>(id);
    }
    case INT_LIT:
    {
      // Consume the token.
      TOKEN t = CurTok;
      getNextToken();

      return std::make_unique<IntNode>(t);
    }
    case FLOAT_LIT:
    {
      // Consume the token.
      TOKEN t = CurTok;
      getNextToken();

      return std::make_unique<FloatNode>(t);
    }
    case BOOL_LIT:
    {
      // Consume the token.
      TOKEN t = CurTok;
      getNextToken();

      return std::make_unique<BoolNode>(t);
    }
    default:
      return nullptr;
  }
}

static std::unique_ptr<TOKEN> parseVarType() 
{
  switch (CurTok.type)
  {
    case BOOL_TOK:
    case INT_TOK:
    case FLOAT_TOK: 
    {
      TOKEN t = CurTok;
      // Consume the TYPE token.
      getNextToken();

      return std::make_unique<TOKEN>(t);
    }
    default:
      return nullptr;
  }
}

static std::unique_ptr<TOKEN> parseFunType() 
{
  if (CurTok.type == VOID_TOK) 
  {
    TOKEN t = CurTok;
    // Consume the VOID token.
    getNextToken();

    return std::make_unique<TOKEN>(t);
  }

  return parseVarType();
}

