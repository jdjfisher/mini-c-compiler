
#pragma once

// Third party imports
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

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
#include <utility>
#include <vector>
#include <exception>
#include <assert.h> 
#include <sstream>

// Application imports
#include "lexer.h"
#include "codegen.h"

// Namespaces
using namespace llvm;

// Aliases
using SymbolTable = std::map<std::string, AllocaInst*>;


extern LLVMContext context;
extern IRBuilder<> builder;
extern std::unique_ptr<Module> module;


//===----------------------------------------------------------------------===//
// AST nodes
//===----------------------------------------------------------------------===//

class Node 
{
  public:
    virtual ~Node() {}
    virtual std::string to_string(std::string indent = "") const;
};

class ExprNode : public Node 
{
  public:
    virtual Value* codegen(SymbolTable& symbols) = 0;
};

class BinOpNode : public ExprNode 
{
  private:
    std::unique_ptr<ExprNode> left;
    std::unique_ptr<ExprNode> right;
    TOKEN op;

  public:
    BinOpNode(
      std::unique_ptr<ExprNode> left, std::unique_ptr<ExprNode> right, TOKEN op
    ) : left(std::move(left)), right(std::move(right)), op(op)
    {}
    virtual Value* codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bin_op> " + op.lexeme + "\n"
        + left->to_string(indent + "  ") + right->to_string(indent + "  ");
    };
};

class AssignNode : public ExprNode
{
  private:
    TOKEN id;
    std::unique_ptr<ExprNode> e;

  public:
    AssignNode(TOKEN id, std::unique_ptr<ExprNode> e) : id(id), e(std::move(e)) {}
    virtual Value* codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<assign> " + id.lexeme + "\n" + e->to_string(indent + "  ");
    };
};

class DeclNode : public Node
{
  public:
    virtual void codegen() = 0;
};

class VarDeclNode : public DeclNode 
{
  private:
    TOKEN type;
    TOKEN id;

  public:
    VarDeclNode(TOKEN type, TOKEN id) : type(type), id(id) {}
    virtual void codegen() override;
    void codegen(SymbolTable& symbols);
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<var_decl> " + type.lexeme + " " + id.lexeme + "\n";
    };
};

class StmtNode : public Node
{
  public:
    virtual void codegen(std::map<std::string, AllocaInst *> &symbols) = 0;
};

class BlockStmtNode : public StmtNode 
{
  private:
    std::vector<std::unique_ptr<VarDeclNode>> decls;
    std::vector<std::unique_ptr<StmtNode>> stmts;

  public:
    BlockStmtNode(
      std::vector<std::unique_ptr<VarDeclNode>> decls,
      std::vector<std::unique_ptr<StmtNode>> stmts
    ) : decls(std::move(decls)), stmts(std::move(stmts))
    {}
    virtual void codegen(SymbolTable& symbols) override
    {
      for (const auto& d : decls)
        d->codegen(symbols);

      for (const auto& s : stmts)
        s->codegen(symbols);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<block_stmt>\n";

      for (const auto& d : decls)
        str += d->to_string(indent + "  ");

      for (const auto& s : stmts)
        str += s->to_string(indent + "  ");
      
      return str;   
    };
};

class ExprStmtNode : public StmtNode
{
  private:
    std::unique_ptr<ExprNode> e;

  public:
    ExprStmtNode(std::unique_ptr<ExprNode> e = nullptr) : e(std::move(e)) {}
    virtual void codegen(SymbolTable& symbols) override
    {
      e->codegen(symbols);
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<expr_stmt>\n";
      if (e) str += e->to_string(indent + "  ");
      return str;
    };
};

class ReturnStmtNode : public StmtNode 
{
  private:
    TOKEN tok;
    std::unique_ptr<ExprNode> e;

  public:
    ReturnStmtNode(TOKEN tok, std::unique_ptr<ExprNode> e = nullptr) : tok(tok), e(std::move(e)) {}
    virtual void codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<return_stmt>\n";
      if (e) str += e->to_string(indent + "  ");
      return str;
    };
};

class IfStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<BlockStmtNode> then;
    std::unique_ptr<BlockStmtNode> else_;

  public:
    IfStmtNode(
      std::unique_ptr<ExprNode> cond, 
      std::unique_ptr<BlockStmtNode> then, 
      std::unique_ptr<BlockStmtNode> else_
    ) : cond(std::move(cond)), then(std::move(then)), else_(std::move(else_)) 
    {}
    virtual void codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<if_stmt>\n";
      str += cond->to_string(indent + "  ");
      str += then->to_string(indent + "  ");
      str += else_->to_string(indent + "  ");
      return str;
    };
};

class WhileStmtNode : public StmtNode 
{
  private:
    std::unique_ptr<ExprNode> cond;
    std::unique_ptr<StmtNode> loop;

  public:
    WhileStmtNode(
      std::unique_ptr<ExprNode> cond, 
      std::unique_ptr<StmtNode> loop
    ) : cond(std::move(cond)), loop(std::move(loop)) 
    {}
    virtual void codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<while_stmt>\n";
      str += cond->to_string(indent + "  ");
      str += loop->to_string(indent + "  ");
      return str;
    };
};

class ParamNode : public Node 
{
  private:
    TOKEN type;
    TOKEN id;

  public:
    ParamNode(TOKEN type, TOKEN id) : type(type), id(id) {}
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<param> " + type.lexeme + " " + id.lexeme + "\n";
    };
    std::string getName() { return id.lexeme; };
    int getTypeLL() { return type.type; };
};

class FunSignNode : public Node 
{
  private:
    TOKEN type;
    TOKEN id;
    std::vector<std::unique_ptr<ParamNode>> params;

  public:
    FunSignNode(
      TOKEN type, 
      TOKEN id, 
      std::vector<std::unique_ptr<ParamNode>> params
    ) : type(type), id(id), params(std::move(params)) 
    {}
    TOKEN getId() const { return id; };
    Function* codegen();
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<fun_sign> " + type.lexeme + " " 
        + id.lexeme + "\n";
        
      indent += "  ";
      str += indent + "<params>\n";

      for (const auto& param : params)
        str += param->to_string(indent + "  ");

      return str;
    };
    std::string getName() { return id.lexeme; };
};

class FunDeclNode : public DeclNode 
{
  private:
    std::unique_ptr<FunSignNode> sign;
    std::unique_ptr<BlockStmtNode> body;

  public:
    FunDeclNode(
      std::unique_ptr<FunSignNode> sign, 
      std::unique_ptr<BlockStmtNode> body
    ) : sign(std::move(sign)), body(std::move(body)) 
    {}
    virtual void codegen() override;
    virtual std::string to_string(std::string indent = "") const override
    {
      return sign->to_string(indent) + body->to_string(indent + "  ");
    };
};


class ProgramNode : public Node 
{
  private:
    std::vector<std::unique_ptr<FunSignNode>> externs;
    std::vector<std::unique_ptr<DeclNode>> decls;

  public:
    ProgramNode(
      std::vector<std::unique_ptr<FunSignNode>> externs,
      std::vector<std::unique_ptr<DeclNode>> decls
    ) : externs(std::move(externs)), decls(std::move(decls)) 
    {}
    void codegen()
    {
      for (const auto& e : externs)
        e->codegen();

      for (const auto& d : decls)
        d->codegen();
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str = indent + "<program>\n";
      str += indent + "  <externs>\n";

      for (const auto& e : externs)
        str += e->to_string(indent + "    ");
      
      str += indent + "  <decls>\n";

      for (const auto& d : decls)
        str += d->to_string(indent + "    ");
      
      return str;
    };
};

class UnaryNode : public ExprNode 
{
  private:
    TOKEN op;
    std::unique_ptr<ExprNode> expr;

  public:
    UnaryNode(
      TOKEN op,
      std::unique_ptr<ExprNode> expr
    ) : op(op), expr(std::move(expr)) 
    {}
    virtual Value* codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<unary>" + op.lexeme + "\n" + expr->to_string(indent + "  ");
    };
};

class VariableNode : public ExprNode 
{
  private:
    TOKEN id;

  public:
    VariableNode(TOKEN id): id(id) {}
    virtual Value* codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<variable> " + id.lexeme + "\n";
    };
};

class CallNode : public ExprNode 
{
  private:
    TOKEN id;
    std::vector<std::unique_ptr<ExprNode>> args;

  public:
    CallNode(
      TOKEN id, 
      std::vector<std::unique_ptr<ExprNode>> args
    ) : id(id), args(std::move(args)) 
    {}
    virtual Value* codegen(SymbolTable& symbols) override;
    virtual std::string to_string(std::string indent = "") const override
    {
      std::string str =  indent + "<call> " + id.lexeme + "\n";

      for (const auto& arg : args)
        str += arg->to_string(indent + "  ");

      return str; 
    };
};

class FloatNode : public ExprNode 
{
  private:
    TOKEN tok;

  public:
    FloatNode(TOKEN tok) : tok(tok) {}
    virtual Value* codegen(SymbolTable& symbols) override
    {
      return getFloatLL(std::stof(tok.lexeme));
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<float> " + tok.lexeme + "\n";
    };
};

class IntNode : public ExprNode 
{
  private:
    TOKEN tok;

  public:
    IntNode(TOKEN tok) : tok(tok) {}
    virtual Value* codegen(SymbolTable& symbols) override
    {
      return getIntLL(std::stoi(tok.lexeme));
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<int> " + tok.lexeme + "\n";
    };
};

class BoolNode : public ExprNode 
{
  private:
    TOKEN tok;

  public:
    BoolNode(TOKEN tok) : tok(tok) {}
    virtual Value* codegen(SymbolTable& symbols) override
    {
      return getBoolLL(tok.lexeme == "true");
    };
    virtual std::string to_string(std::string indent = "") const override
    {
      return indent + "<bool> " + tok.lexeme + "\n";
    };
};
