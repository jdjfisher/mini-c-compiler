
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
  std::unique_ptr<ExternsNode> es;

  if (CurTok.type == EXTERN)
  {
    es = parseExterns();
  }

  auto ds = parseDecls();

  return std::make_unique<ProgramNode>(std::move(es), std::move(ds));
}

static std::unique_ptr<ExternsNode> parseExterns() 
{
  auto e = parseExtern();

  std::vector<std::unique_ptr<FunSignNode>> externs;
  externs.push_back(std::move(e));

  while (CurTok.type == EXTERN)
  {
    e = parseExtern();

    externs.push_back(std::move(e));
  }

  return std::make_unique<ExternsNode>(std::move(externs));
}

static std::unique_ptr<FunSignNode> parseExtern() 
{
  if (CurTok.type != EXTERN) throw SyntaxError();
  // Consume the EXTERN token.
  getNextToken();

  auto fs = parseFunSign();

  if (CurTok.type != SC) throw SyntaxError(); 
  // Consume the ; token.
  getNextToken();

  return std::move(fs);
}

static std::unique_ptr<FunSignNode> parseFunSign() 
{
  auto ft = parseFunType();

  if (CurTok.type != IDENT) throw SyntaxError(); 
  TOKEN id = CurTok;
  // Consume the IDENT token.
  getNextToken();

  if (CurTok.type != LPAR) throw SyntaxError(); 
  // Consume the ( token.
  getNextToken();

  // Parse params.
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

      params.push_back(std::move(p));

      if (CurTok.type == COMMA)
      {
        // Consume the , token.
        getNextToken();
      }
    }
  }

  if (CurTok.type != RPAR) throw SyntaxError(); 
  // Consume the ) token.
  getNextToken();

  return std::make_unique<FunSignNode>(*ft, id, std::move(params));
}

static std::unique_ptr<DeclsNode> parseDecls() 
{
  auto d = parseDecl();

  std::vector<std::unique_ptr<DeclNode>> decls;
  decls.push_back(std::move(d));

  while (CurTok.type == VOID_TOK  || CurTok.type == INT_TOK ||
         CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    d = parseDecl();

    decls.push_back(std::move(d));
  }

  return std::make_unique<DeclsNode>(std::move(decls));
}

static std::unique_ptr<DeclNode> parseDecl()
{
  TOKEN t1 = getCurrentToken();
  TOKEN t2 = getNextToken();
  TOKEN t3 = getNextToken();
  putBackToken(t2);
  putBackToken(t1);

  switch (t3.type)
  {
    case SC:
      return std::make_unique<DeclNode>(parseVarDecl());
    case LPAR:
      return std::make_unique<DeclNode>(parseFunDecl()); 
    default:
      throw SyntaxError();
  }
}

static std::unique_ptr<VarDeclNode> parseVarDecl() 
{
  auto vt = parseVarType();

  if (CurTok.type != IDENT) throw SyntaxError();
  // Consume the IDENT token.
  TOKEN t = CurTok;
  getNextToken();

  if (CurTok.type != SC) throw SyntaxError();
  // Consume the ; token.
  getNextToken();

  return std::make_unique<VarDeclNode>(*vt, t);
}

static std::unique_ptr<FunDeclNode> parseFunDecl() 
{
  auto fs = parseFunSign();
  auto bs = parseBlockStmt();

  return std::make_unique<FunDeclNode>(std::move(fs), std::move(bs));
}

static std::unique_ptr<ParamNode> parseParam() 
{
  auto vt = parseVarType(); 

  if (CurTok.type != IDENT) throw SyntaxError();
  TOKEN t = CurTok;
  // Consume the IDENT token.
  getNextToken();

  return std::make_unique<ParamNode>(*vt, t);
}

static std::unique_ptr<LocalDeclsNode> parseLocalDecls() 
{
  std::vector<std::unique_ptr<VarDeclNode>> decls;

  while (CurTok.type == INT_TOK || 
         CurTok.type == FLOAT_TOK || 
         CurTok.type == BOOL_TOK)
  {
    decls.push_back(parseVarDecl());
  }
  
  return std::make_unique<LocalDeclsNode>(std::move(decls));
}

static std::unique_ptr<StmtListNode> parseStmtList() 
{  
  std::vector<std::unique_ptr<StmtNode>> stmts;

  while (true)
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

        stmts.push_back(std::move(s));
        break;
      }
      default:
        return std::make_unique<StmtListNode>(std::move(stmts));
    }
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
      throw SyntaxError();
  }
}

static std::unique_ptr<BlockStmtNode> parseBlockStmt() 
{
  if (CurTok.type != LBRA) throw SyntaxError();
  // Consume the { token.
  getNextToken();

  auto ld = parseLocalDecls();
  auto sl = parseStmtList();

  if (CurTok.type != RBRA) throw SyntaxError();
  // Consume the } token.
  getNextToken();

  return std::make_unique<BlockStmtNode>(std::move(ld), std::move(sl));
}

static std::unique_ptr<WhileStmtNode> parseWhileStmt() 
{
  if (CurTok.type != WHILE) throw SyntaxError();
  // Consume the WHILE token.
  getNextToken();

  if (CurTok.type != LPAR) throw SyntaxError();
  // Consume the ( token.
  getNextToken();

  auto e = parseExpr();

  if (CurTok.type != RPAR) throw SyntaxError();
  // Consume the ) token.
  getNextToken();

  auto s = parseStmt();

  return std::make_unique<WhileStmtNode>(std::move(e), std::move(s));
}

static std::unique_ptr<IfStmtNode> parseIfStmt() 
{
  if (CurTok.type != IF) throw SyntaxError();
  // Consume the IF token.
  getNextToken();

  if (CurTok.type != LPAR) throw SyntaxError();
  // Consume the ( token.
  getNextToken();

  auto e = parseExpr();

  if (CurTok.type != RPAR) throw SyntaxError();
  // Consume the ) token.
  getNextToken();

  auto thenB = parseBlockStmt();

  std::unique_ptr<BlockStmtNode> elseB = nullptr;

  if (CurTok.type == ELSE)
  {
    // Consume the ELSE token.
    getNextToken();

    elseB = parseBlockStmt();
  } 

  return std::make_unique<IfStmtNode>(std::move(e), std::move(thenB), std::move(elseB));
}

static std::unique_ptr<ReturnStmtNode> parseReturnStmt() 
{
  if (CurTok.type != RETURN) throw SyntaxError();
  // Consume the RETURN token.
  getNextToken();

  if (CurTok.type == SC)
  {
    // Consume the ; token.
    getNextToken();

    return std::make_unique<ReturnStmtNode>();
  }

  auto e = parseExpr();

  if (CurTok.type != SC) throw SyntaxError();
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

  if (CurTok.type != SC) throw SyntaxError();
  // Consume the ; token.
  getNextToken();

  return std::make_unique<ExprStmtNode>(std::move(e));
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

    return std::make_unique<AssignNode>(ot, std::move(e)); 
  }

  // Revert the look-ahead
  putBackToken(ot);

  return parseDisj();
}

static std::unique_ptr<ExprNode> parseDisj()
{
  auto c = parseConj();

  if (CurTok.type == OR) 
  {
    // Consume the || token.
    TOKEN op = CurTok;
    getNextToken();

    auto d = parseDisj();

    return std::make_unique<BinOpNode>(std::move(c), std::move(d), op);
  }

  return std::move(c);
}

static std::unique_ptr<ExprNode> parseConj()
{
  auto e = parseEqual();

  if (CurTok.type == AND) 
  {
    // Consume the && token.
    TOKEN op = CurTok;
    getNextToken();

    auto c = parseConj();

    return std::make_unique<BinOpNode>(std::move(e), std::move(c), op);
  }

  return std::move(e);
}

static std::unique_ptr<ExprNode> parseEqual()
{
  auto o = parseOrder();

  if (CurTok.type == EQ || CurTok.type == NE) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto e = parseEqual();

    return std::make_unique<BinOpNode>(std::move(o), std::move(e), op);
  }

  return std::move(o);
}

static std::unique_ptr<ExprNode> parseOrder()
{
  auto t = parseTerm();

  if (CurTok.type == LE || CurTok.type == LT || 
      CurTok.type == GE || CurTok.type == GT
  ) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto o = parseOrder();

    return std::make_unique<BinOpNode>(std::move(t), std::move(o), op);
  }

  return std::move(t);
}

static std::unique_ptr<ExprNode> parseTerm()
{
  auto f = parseFactor();

  if (CurTok.type == PLUS || CurTok.type == MINUS) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto t = parseTerm();

    return std::make_unique<BinOpNode>(std::move(f), std::move(t), op);
  }

  return std::move(f);
}

static std::unique_ptr<ExprNode> parseFactor()
{
  auto l = parseLiteral();

  if (CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto f = parseFactor();

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

      return std::make_unique<UnaryNode>(op, std::move(l));
    }
    case LPAR:
    {
      // Consume the ( token.
      getNextToken();

      auto e = parseExpr();

      if (CurTok.type != RPAR) throw SyntaxError();
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

        // Parse args.
        std::vector<std::unique_ptr<ExprNode>> args;
           
        while (CurTok.type == IDENT || CurTok.type == MINUS ||
               CurTok.type == NOT || CurTok.type == INT_LIT ||
               CurTok.type == FLOAT_LIT || CurTok.type == BOOL_LIT)
        {     
          auto e = parseExpr();

          args.push_back(std::move(e));

          if (CurTok.type == COMMA)
          {
            // Consume the , token.
            getNextToken();
          }
        }

        if (CurTok.type != RPAR) throw SyntaxError();
        // Consume the ( token.
        getNextToken();

        return std::make_unique<CallNode>(id, std::move(args));
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
      throw SyntaxError();
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
      throw SyntaxError();
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

