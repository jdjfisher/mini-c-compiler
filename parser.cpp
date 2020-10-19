
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

  std::cout << "Out: " << p->to_string() << "\n"; 
}

static ProgramNode* parseProgram() 
{
  std::cout << "Parsing Program. Current token: " << CurTok.lexeme << "\n";

  ExternListNode* el;

  if (CurTok.type == EXTERN)
  {
    el = parseExternList();
    if (!el) return nullptr;
  }

  auto dl = parseDeclList();
  if (!dl) return nullptr;

  return new ProgramNode(el, dl);
}

static ExternListNode* parseExternList() 
{
  std::cout << "Parsing ExternList. Current token: " << CurTok.lexeme << "\n";

  auto e = parseExtern();
  if (!e) return nullptr;

  ExternListNode* el;

  if (CurTok.type == EXTERN)
  {
    el = parseExternList();
    if (!el) return nullptr;
  }

  return new ExternListNode(e, el);
}

static ExternNode* parseExtern() 
{
  std::cout << "Parsing Extern. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing DeclList. Current token: " << CurTok.lexeme << "\n";

  auto d = parseDecl();
  if (!d) return nullptr;

  DeclListNode* dl;

  if (CurTok.type == VOID_TOK  || CurTok.type == INT_TOK ||
      CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    dl = parseDeclList();
    if (!dl) return nullptr;
  }

  return new DeclListNode(d, dl);
}

static DeclNode* parseDecl() // TODO: fix
{
  std::cout << "Parsing Decl. Current token: " << CurTok.lexeme << "\n";

  TOKEN t1 = getCurrentToken();
  TOKEN t2 = getNextToken();
  TOKEN t3 = getNextToken();
  putBackToken(t3);
  putBackToken(t2);
  putBackToken(t1);

  if (t3.type == SC)
  {
    auto vd = parseVarDecl();
    if (!vd) return nullptr;

    return new DeclNode(vd);
  }
  else if (t3.type == LPAR)
  {
    auto fd = parseFunDecl();
    if (!fd) return nullptr;

    return new DeclNode(fd); 
  }

  return nullptr;
}

static VarDeclNode* parseVarDecl() 
{
  std::cout << "Parsing VarDecl. Current token: " << CurTok.lexeme << "\n";

  auto vt = parseVarType();
  if (!vt) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  // Consume the IDENT token.
  TOKEN t = CurTok;
  getNextToken();

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  return new VarDeclNode(vt, t);
}

static FunDeclNode* parseFunDecl() 
{
  std::cout << "Parsing FunDecl. Current token: " << CurTok.lexeme << "\n";

  auto ft = parseFunType();
  if (!ft) return nullptr;

  if (CurTok.type != IDENT) return nullptr;
  TOKEN t = CurTok;
  // Consume the IDENT token.
  getNextToken();

  if (CurTok.type != LPAR) return nullptr;
  // Consume the ( token.
  getNextToken(); // TODO: Why does this call consume the token?
  getNextToken(); 

  auto p = parseParams();
  if (!p) return nullptr;

  if (CurTok.type != RPAR) return nullptr;
  // Consume the ) token.
  getNextToken(); 

  auto bs = parseBlockStmt();
  if (!bs) return nullptr;

  return new FunDeclNode(ft, t, p, bs);
}

static VarTypeNode* parseVarType() 
{
  std::cout << "Parsing VarType. Current token: " << CurTok.lexeme << "\n";

  switch (CurTok.type)
  {
    case BOOL_TOK:
    case INT_TOK:
    case FLOAT_TOK: 
    {
      TOKEN t = CurTok;
      // Consume the TYPE token.
      getNextToken();

      return new VarTypeNode(t);
    }
    default:
      return nullptr;
  }
}

static FunTypeNode* parseFunType() 
{
  std::cout << "Parsing FunType. Current token: " << CurTok.lexeme << "\n";

  if (CurTok.type == VOID_TOK) 
  {
    // Consume the VOID token.
    getNextToken();
    return new FunTypeNode();
  }

  auto vt = parseVarType();
  if (!vt) return nullptr;

  return new FunTypeNode(vt);
}

static ParamsNode* parseParams() 
{
  std::cout << "Parsing Params. Current token: " << CurTok.lexeme << "\n";

  if (CurTok.type == INT_TOK || CurTok.type == FLOAT_TOK || CurTok.type == BOOL_TOK)
  {
    auto pl = parseParamList();
    if (!pl) return nullptr;

    return new ParamsNode(pl);
  }
  else if (CurTok.type == VOID_TOK) 
  {
    // Consume the void token.
    getNextToken();
  }

  return new ParamsNode();
}

static ParamListNode* parseParamList() 
{
  std::cout << "Parsing ParamList. Current token: " << CurTok.lexeme << "\n";

  auto p = parseParam();
  if (!p) return nullptr;

  if (CurTok.type == COMMA)
  {
    // Consume the , token.
    getNextToken();

    auto pl = parseParamList();
    if (!pl) return nullptr;

    return new ParamListNode(p, pl);
  }

  return new ParamListNode(p);
}

static ParamNode* parseParam() 
{
  std::cout << "Parsing Param. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing LocalDecls. Current token: " << CurTok.lexeme << "\n";

  if (auto vd = parseVarDecl()) 
  {
    auto ld = parseLocalDecls();
    if (!ld) return nullptr;

    return new LocalDeclsNode(vd, ld);
  }
  
  return new LocalDeclsNode();
}

static StmtListNode* parseStmtList() 
{  
  std::cout << "Parsing StmtList. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing Stmt. Current token: " << CurTok.lexeme << "\n";

  if (auto bs = parseBlockStmt())
  {
    return bs;
  }
  else if (auto is = parseIfStmt())
  {
    return is;
  }
  else if (auto ws = parseWhileStmt())
  {
    return ws;
  }
  else if (auto rs = parseReturnStmt())
  {
    return rs;
  }
  else if (auto es = parseExprStmt())
  {
    return es;
  }

  return nullptr;
}

static BlockStmtNode* parseBlockStmt() 
{
  std::cout << "Parsing BlockStmt. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing WhileStmt. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing IfStmt. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing ElseStmt. Current token: " << CurTok.lexeme << "\n";

  if (CurTok.type == ELSE)
  {
    // Consume the ELSE token.
    getNextToken();

    return new ElseStmtNode(parseBlockStmt());
  } 

  return new ElseStmtNode();
}

static ReturnStmtNode* parseReturnStmt() 
{
  std::cout << "Parsing ReturnStmt. Current token: " << CurTok.lexeme << "\n";

  if (CurTok.type != RETURN) return nullptr;
  // Consume the RETURN token.
  getNextToken();

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  return new ReturnStmtNode(e);
}

static ExprStmtNode* parseExprStmt() 
{
  std::cout << "Parsing ExprStmt. Current token: " << CurTok.lexeme << "\n";

  auto e = parseExpr();

  if (CurTok.type != SC) return nullptr;
  // Consume the ; token.
  getNextToken();

  return new ExprStmtNode(e);
}

static ArgsNode* parseArgs()
{
  std::cout << "Parsing Args. Current token: " << CurTok.lexeme << "\n";

  return new ArgsNode(parseArgList());
}

static ArgListNode* parseArgList()
{
  std::cout << "Parsing ArgList. Current token: " << CurTok.lexeme << "\n";

  auto e = parseExpr();
  if (!e) return nullptr;

  if (CurTok.type == COMMA)
  {
    // Consume the , token.
    getNextToken();

    auto al = parseArgList();
    if (!al) return nullptr;

    return new ArgListNode(e, al);
  }

  return new ArgListNode(e);
}

static ExprNode* parseExpr()
{
  std::cout << "Parsing Expr. Current token: " << CurTok.lexeme << "\n";

  // Store the current token, look-ahead at the next.
  TOKEN ct = CurTok;
  TOKEN la = getNextToken();

  if (ct.type == IDENT && la.type == ASSIGN)
  {
    // Consume the = token.
    getNextToken();

    auto e = parseExpr();
    if (!e) return nullptr;

    return new ExprNode(ct, e); 
  }
  else
  {
    // Revert the look-ahead
    putBackToken(ct);

    auto d = parseDisj();
    if (!d) return nullptr;

    return new ExprNode(d); 
  }
}

static DisjNode* parseDisj()
{
  std::cout << "Parsing Disj. Current token: " << CurTok.lexeme << "\n";

  auto c = parseConj();
  if (!c) return nullptr;

  if (CurTok.type == OR) 
  {
    // Consume the || token.
    getNextToken();

    auto d = parseDisj();
    if (!d) return nullptr;

    return new DisjNode(c, d);
  }

  return new DisjNode(c);
}

static ConjNode* parseConj()
{
  std::cout << "Parsing Conj. Current token: " << CurTok.lexeme << "\n";

  auto e = parseEqual();
  if (!e) return nullptr;

  if (CurTok.type == OR) 
  {
    // Consume the || token.
    getNextToken();

    auto c = parseConj();
    if (!c) return nullptr;

    return new ConjNode(e, c);
  }

  return new ConjNode(e);
}

static EqualNode* parseEqual()
{
  std::cout << "Parsing Equal. Current token: " << CurTok.lexeme << "\n";

  auto o = parseOrder();
  if (!o) return nullptr;

  if (CurTok.type == EQ || CurTok.type == NE) 
  {
    // Consume the token.
    TOKEN op = CurTok;
    getNextToken();

    auto e = parseEqual();
    if (!e) return nullptr;

    return new EqualNode(o, op, e);
  }

  return new EqualNode(o);
}

static OrderNode* parseOrder()
{
  std::cout << "Parsing Order. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing Term. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing Factor. Current token: " << CurTok.lexeme << "\n";

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
  std::cout << "Parsing Literal. Current token: " << CurTok.lexeme << "\n";

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
