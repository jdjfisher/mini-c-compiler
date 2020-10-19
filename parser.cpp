
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

  std::cout << "Current token: " << CurTok.lexeme << ".\n";

  return CurTok;
}

static void putBackToken(TOKEN token) 
{ 
  // if (CurTok != NULL) 
  // {
  tok_buffer.push_front(CurTok); 
  // }

  std::cout << "Put token back: " << token.lexeme << ".\n";
  std::cout << "Current token: " << CurTok.lexeme << ".\n";

  CurTok = token;
}

ProgramNode* parse() 
{
  // Get the first token.
  getNextToken();

  return parseProgram();
}

static ProgramNode* parseProgram() 
{
  std::cout << "Parsing Program.\n";

  ExternListNode* el;

  if (CurTok.type == EXTERN)
  {
    el = parseExternList();
    if (!el) return nullptr;
  }

  auto dl = parseDeclList();
  if (!dl) return nullptr;

  std::cout << "Parsed Program.\n";
  return new ProgramNode(el, dl);
}

static ExternListNode* parseExternList() 
{
  std::cout << "Parsing ExternList.\n";

  auto e = parseExtern();
  if (!e) return nullptr;

  if (CurTok.type == EXTERN)
  {
    auto el = parseExternList();
    if (!el) return nullptr;

    std::cout << "Parsed ExternList.\n";
    return new ExternListNode(e, el);
  }

  std::cout << "Parsed ExternList.\n";
  return new ExternListNode(e);
}

static ExternNode* parseExtern() 
{
  std::cout << "Parsing Extern.\n";

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

  std::cout << "Parsed Extern.\n";
  return new ExternNode(ft, t, p);
}

static DeclListNode* parseDeclList() 
{
  std::cout << "Parsing DeclList.\n";

  auto d = parseDecl();
  if (!d) return nullptr;

  if (CurTok.type == VOID_TOK  || CurTok.type == INT_TOK ||
      CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    auto dl = parseDeclList();
    if (!dl) return nullptr;

    std::cout << "Parsed DeclList.\n";
    return new DeclListNode(d, dl);
  }

  std::cout << "Parsed DeclList.\n";
  return new DeclListNode(d);
}

static DeclNode* parseDecl() // TODO: fix
{
  std::cout << "Parsing Decl.\n";

  TOKEN t1 = getCurrentToken();
  TOKEN t2 = getNextToken();
  TOKEN t3 = getNextToken();
  putBackToken(t2);
  putBackToken(t1);

  if (t3.type == SC)
  {
    auto vd = parseVarDecl();
    if (!vd) return nullptr;

    std::cout << "Parsed Decl.\n";
    return new DeclNode(vd);
  }
  else if (t3.type == LPAR)
  {
    auto fd = parseFunDecl();
    if (!fd) return nullptr;

    std::cout << "Parsed Decl.\n";
    return new DeclNode(fd); 
  }

  return nullptr;
}

static VarDeclNode* parseVarDecl() 
{
  std::cout << "Parsing VarDecl.\n";

  auto vt = parseVarType();
  if (!vt) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  // Consume the IDENT token.
  TOKEN t = CurTok;
  getNextToken();

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  std::cout << "Parsed VarDecl.\n";
  return new VarDeclNode(vt, t);
}

static FunDeclNode* parseFunDecl() 
{
  std::cout << "Parsing FunDecl.\n";

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

  std::cout << "Parsed FunDecl.\n";
  return new FunDeclNode(ft, t, p, bs);
}

static VarTypeNode* parseVarType() 
{
  std::cout << "Parsing VarType.\n";

  switch (CurTok.type)
  {
    case BOOL_TOK:
    case INT_TOK:
    case FLOAT_TOK: 
    {
      TOKEN t = CurTok;
      // Consume the TYPE token.
      getNextToken();

      std::cout << "Parsed VarType.\n";
      return new VarTypeNode(t);
    }
    default:
      return nullptr;
  }
}

static FunTypeNode* parseFunType() 
{
  std::cout << "Parsing FunType.\n";

  if (CurTok.type == VOID_TOK) 
  {
    // Consume the VOID token.
    getNextToken();

    std::cout << "Parsed FunType.\n";
    return new FunTypeNode();
  }

  auto vt = parseVarType();
  if (!vt) return nullptr;

  std::cout << "Parsed FunType.\n";
  return new FunTypeNode(vt);
}

static ParamsNode* parseParams() 
{
  std::cout << "Parsing Params.\n";

  if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    auto pl = parseParamList();
    if (!pl) return nullptr;

    std::cout << "Parsed Params.\n";
    return new ParamsNode(pl);
  }
  else if (CurTok.type == VOID_TOK) 
  {
    // Consume the void token.
    getNextToken();
  }

  std::cout << "Parsed Params.\n";
  return new ParamsNode();
}

static ParamListNode* parseParamList() 
{
  std::cout << "Parsing ParamList.\n";

  auto p = parseParam();
  if (!p) return nullptr;

  if (CurTok.type == COMMA)
  {
    // Consume the , token.
    getNextToken();

    auto pl = parseParamList();
    if (!pl) return nullptr;

    std::cout << "Parsed ParamList.\n";
    return new ParamListNode(p, pl);
  }

  std::cout << "Parsed ParamList.\n";
  return new ParamListNode(p);
}

static ParamNode* parseParam() 
{
  std::cout << "Parsing Param.\n";

  auto vt = parseVarType(); 
  if (!vt) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consume the IDENT token.
  getNextToken();

  return new ParamNode(vt, t);
}

static LocalDeclsNode* parseLocalDecls() 
{
  std::cout << "Parsing LocalDecls.\n";

  if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    auto vd = parseVarDecl();
    if (!vd) return nullptr;

    auto ld = parseLocalDecls();
    if (!ld) return nullptr;

    return new LocalDeclsNode(vd, ld);
  }
  
  return new LocalDeclsNode();
}

static StmtListNode* parseStmtList() 
{  
  std::cout << "Parsing StmtList.\n";

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

      return new StmtListNode(s, sl);
    }
    default:
      return new StmtListNode();
  }
}

static StmtNode* parseStmt() 
{
  std::cout << "Parsing Stmt.\n";

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

static BlockStmtNode* parseBlockStmt() 
{
  std::cout << "Parsing BlockStmt.\n";

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

  return new BlockStmtNode(ld, sl);
}

static WhileStmtNode* parseWhileStmt() 
{
  std::cout << "Parsing WhileStmt.\n";

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

  return new WhileStmtNode(e, s);
}

static IfStmtNode* parseIfStmt() 
{
  std::cout << "Parsing IfStmt.\n";

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

  return new IfStmtNode(e, bs, es);
}

static ElseStmtNode* parseElseStmt() 
{
  std::cout << "Parsing ElseStmt.\n";

  if (CurTok.type == ELSE)
  {
    // Consume the ELSE token.
    getNextToken();

    auto bs = parseBlockStmt();
    if (!bs) return nullptr;

    return new ElseStmtNode(bs);
  } 

  return new ElseStmtNode();
}

static ReturnStmtNode* parseReturnStmt() 
{
  std::cout << "Parsing ReturnStmt.\n";

  if (CurTok.type != RETURN) return nullptr;
  // Consume the RETURN token.
  getNextToken();

  if (CurTok.type == SC)
  {
    // Consume the ; token.
    getNextToken();

    std::cout << "Parsed ReturnStmt.\n";
    return new ReturnStmtNode();
  }

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  std::cout << "Parsed ReturnStmt.\n";
  return new ReturnStmtNode(e);
}

static ExprStmtNode* parseExprStmt() 
{
  std::cout << "Parsing ExprStmt.\n";

  if (CurTok.type == SC)
  {
    // Consume the ; token.
    getNextToken();
  
    std::cout << "Parsed ExprStmt.\n";
    return new ExprStmtNode();
  }

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  std::cout << "Parsed ExprStmt.\n";
  return new ExprStmtNode(e);
}

static ArgsNode* parseArgs()
{
  std::cout << "Parsing Args.\n";

  switch (CurTok.type)
  {
    case IDENT:  // FIRST(<expr>)
    case MINUS:
    case NOT:
    case INT_LIT:
    case FLOAT_LIT:
    case BOOL_LIT:
    {
      auto al = parseArgList();
      if (!al) return nullptr;

      std::cout << "Parsed Args.\n";
      return new ArgsNode(al);
    }
    default:
      std::cout << "Parsed Args.\n";
      return new ArgsNode();
  }
}

static ArgListNode* parseArgList()
{
  std::cout << "Parsing ArgList.\n";

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type == COMMA)
  {
    // Consume the , token.
    getNextToken();

    auto al = parseArgList();
    if (!al) return nullptr;

    std::cout << "Parsed Args.\n";
    return new ArgListNode(e, al);
  }

  std::cout << "Parsed Args.\n";
  return new ArgListNode(e);
}

static ExprNode* parseExpr()
{
  std::cout << "Parsing Expr.\n";

  // Store the current token, look-ahead at the next.
  TOKEN ot = CurTok;
  TOKEN ct = getNextToken();

  if (ot.type == IDENT && ct.type == ASSIGN)
  {
    // Consume the = token.
    getNextToken();

    auto e = parseExpr();
    if (!e) return nullptr;

    std::cout << "Parsed Expr.\n";
    return new ExprNode(ot, e); 
  }
  else
  {
    // Revert the look-ahead
    putBackToken(ot);

    auto d = parseDisj();
    if (!d) return nullptr;

    std::cout << "Parsed Expr.\n";
    return new ExprNode(d); 
  }
}

static DisjNode* parseDisj()
{
  std::cout << "Parsing Disj.\n";

  auto c = parseConj();
  if (!c) return nullptr;

  if (CurTok.type == OR) 
  {
    // Consume the || token.
    getNextToken();

    auto d = parseDisj();
    if (!d) return nullptr;

    std::cout << "Parsed Disj.\n";
    return new DisjNode(c, d);
  }

  std::cout << "Parsed Disj.\n";
  return new DisjNode(c);
}

static ConjNode* parseConj()
{
  std::cout << "Parsing Conj.\n";

  auto e = parseEqual();
  if (!e) return nullptr;

  if (CurTok.type == OR) 
  {
    // Consume the || token.
    getNextToken();

    auto c = parseConj();
    if (!c) return nullptr;

    std::cout << "Parsed Conj.\n";
    return new ConjNode(e, c);
  }

  std::cout << "Parsed Conj.\n";
  return new ConjNode(e);
}

static EqualNode* parseEqual()
{
  std::cout << "Parsing Equal.\n";

  auto o = parseOrder();
  if (!o) return nullptr;

  if (CurTok.type == EQ || CurTok.type == NE) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto e = parseEqual();
    if (!e) return nullptr;

    std::cout << "Parsed Eq.\n";
    return new EqualNode(o, op, e);
  }

  std::cout << "Parsed Eq.\n";
  return new EqualNode(o);
}

static OrderNode* parseOrder()
{
  std::cout << "Parsing Order.\n";

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

    return new OrderNode(t, op, o);
  }

  return new OrderNode(t);
}

static TermNode* parseTerm()
{
  std::cout << "Parsing Term.\n";

  auto f = parseFactor();
  if (!f) return nullptr;

  if (CurTok.type == PLUS || CurTok.type == MINUS) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto t = parseTerm();
    if (!t) return nullptr;

    return new TermNode(f, op, t);
  }

  return new TermNode(f);
}

static FactorNode* parseFactor()
{
  std::cout << "Parsing Factor.\n";

  auto l = parseLiteral();
  if (!l) return nullptr;

  if (CurTok.type == ASTERIX || CurTok.type == DIV || CurTok.type == MOD) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto f = parseFactor();
    if (!f) return nullptr;

    return new FactorNode(l, op, f);
  }

  return new FactorNode(l);
}

static LiteralNode* parseLiteral()
{
  std::cout << "Parsing Literal.\n";

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

      return new UnaryNode(op, l);
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

      return new ParenthesesNode(e);
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

        return new CallNode(id, a);
      }

      return new VariableNode(id);
    }
    case INT_LIT:
    {
      // Consume the token.
      TOKEN t = CurTok;
      getNextToken();

      return new IntNode(t);
    }
    case FLOAT_LIT:
    {
      // Consume the token.
      TOKEN t = CurTok;
      getNextToken();

      return new FloatNode(t);
    }
    case BOOL_LIT:
    {
      // Consume the token.
      TOKEN t = CurTok;
      getNextToken();

      return new BoolNode(t);
    }
    default:
      return nullptr;
  }
}
