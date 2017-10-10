#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "state.h"

struct Accu;

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Accu&) const {}
  virtual ~Node() {}
};

struct Statement : Node {
  using Node::Node;
  virtual void check(State&) const = 0;
  virtual bool hasReturnStatement(const State&) const;
  virtual void codegen(Accu&) const = 0;
};

struct Expression : Statement {
  using Statement::Statement;
  virtual ArithmeticType getType(const State&) const = 0;
  virtual void check(State&) const override;
};

struct Constant : Expression {
  Constant(CodeLoc, ArithmeticType, string value);
  virtual ArithmeticType getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  ArithmeticType type;
  string value;
};

struct Variable : Expression {
  Variable(CodeLoc, string name);
  virtual ArithmeticType getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  string name;
};

struct BinaryExpression : Expression {
  BinaryExpression(CodeLoc, char op, unique_ptr<Expression>, unique_ptr<Expression>);
  virtual ArithmeticType getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  char op;
  unique_ptr<Expression> e1, e2;
};

struct FunctionCall : Expression {
  FunctionCall(CodeLoc, string name);
  virtual ArithmeticType getType(const State&) const override;
  virtual void codegen(Accu&) const override;
  string name;
  vector<unique_ptr<Expression>> arguments;
};

struct VariableDecl : Statement {
  VariableDecl(CodeLoc, string type, string identifier);
  string type;
  string identifier;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
};

struct Assignment : Statement {
  Assignment(CodeLoc, string variable, unique_ptr<Expression>);
  string variable;
  unique_ptr<Expression> expr;
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

struct TopLevelStatement : Node {
  using Node::Node;
  virtual void check(State&) const = 0;
  virtual void codegen(Accu&) const = 0;
};

struct FunctionDefinition : TopLevelStatement {
  FunctionDefinition(CodeLoc, string returnType, string name);
  string returnType;
  string name;
  vector<unique_ptr<VariableDecl>> parameters;
  unique_ptr<StatementBlock> body;
  virtual void check(State&) const override;
  virtual void codegen(Accu&) const override;
};

struct AST {
  vector<unique_ptr<TopLevelStatement>> elems;
};
