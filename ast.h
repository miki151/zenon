#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "state.h"
#include "binary_operator.h"

struct Accu;

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Accu&) const {}
  virtual ~Node() {}
};

struct Expression : Node {
  using Node::Node;
  virtual Type getType(const State&) const = 0;
};

struct Constant : Expression {
  Constant(CodeLoc, ArithmeticType, string value);
  virtual Type getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  Type type;
  string value;
};

struct Variable : Expression {
  Variable(CodeLoc, string name);
  virtual Type getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  string name;
};

struct BinaryExpression : Expression {
  BinaryExpression(CodeLoc, BinaryOperator, unique_ptr<Expression>, unique_ptr<Expression>);
  virtual Type getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  BinaryOperator op;
  unique_ptr<Expression> e1, e2;
};

struct FunctionCall : Expression {
  FunctionCall(CodeLoc, string name);
  virtual Type getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  string name;
  vector<unique_ptr<Expression>> arguments;
};

struct Statement : Node {
  using Node::Node;
  virtual void check(State&) const = 0;
  virtual bool hasReturnStatement(const State&) const;
  virtual void codegen(Accu&) const = 0;
  virtual bool allowTopLevel() const { return false; }
};

struct VariableDeclaration : Statement {
  VariableDeclaration(CodeLoc, string type, string identifier);
  string type;
  string identifier;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
};

struct IfStatement : Statement {
  IfStatement(CodeLoc, unique_ptr<Expression> cond, unique_ptr<Statement> ifTrue,
      unique_ptr<Statement> ifFalse /* can be null*/);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> ifTrue, ifFalse;
  virtual bool hasReturnStatement(const State&) const override;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
};

struct StatementBlock : Statement {
  using Statement::Statement;
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement(const State&) const override;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
};

struct ReturnStatement : Statement {
  using Statement::Statement;
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement(const State&) const override;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
};

struct StructDeclaration : Statement {
  StructDeclaration(CodeLoc, string name);
  string name;
  struct Member {
    string type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Member> members;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
  virtual bool allowTopLevel() const override { return true; }
};

struct FunctionDefinition : Statement {
  FunctionDefinition(CodeLoc, string returnType, string name);
  string returnType;
  string name;
  vector<unique_ptr<VariableDeclaration>> parameters;
  unique_ptr<StatementBlock> body;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
  virtual bool allowTopLevel() const override { return true; }
};

struct AST {
  vector<unique_ptr<Statement>> elems;
};
