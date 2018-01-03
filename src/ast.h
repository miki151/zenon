#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "state.h"
#include "operator.h"
#include "identifier.h"
#include "function_call_type.h"

struct Accu;

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Accu&) const {}
  virtual ~Node() {}
};

struct Expression : Node {
  using Node::Node;
  virtual Type getType(const State&) = 0;
};

struct Constant : Expression {
  Constant(CodeLoc, ArithmeticType, string value);
  virtual Type getType(const State&) override;
  virtual void codegen(Accu&) const override;
  Type type;
  string value;
};

struct Variable : Expression {
  Variable(CodeLoc, IdentifierInfo);
  virtual Type getType(const State&) override;
  virtual void codegen(Accu&) const override;
  IdentifierInfo identifier;
};

struct MemberAccessType : Expression {
  MemberAccessType(CodeLoc, string name);
  virtual Type getType(const State&) override;
  virtual void codegen(Accu&) const override;
  string name;
};

struct BinaryExpression : Expression {
  BinaryExpression(CodeLoc, Operator, unique_ptr<Expression>, unique_ptr<Expression>);
  virtual Type getType(const State&) override;
  virtual void codegen(Accu&) const override;
  Operator op;
  unique_ptr<Expression> e1, e2;
};

struct UnaryExpression : Expression {
  UnaryExpression(CodeLoc, Operator, unique_ptr<Expression>);
  virtual Type getType(const State&) override;
  virtual void codegen(Accu&) const override;
  Operator op;
  unique_ptr<Expression> expr;
};

struct FunctionCall : Expression {
  FunctionCall(CodeLoc, IdentifierInfo);
  virtual Type getType(const State&) override;
  virtual void codegen(Accu&) const override;
  IdentifierInfo identifier;
  FunctionCallType callType;
  vector<unique_ptr<Expression>> arguments;
};

struct FunctionCallNamedArgs : Expression {
  FunctionCallNamedArgs(CodeLoc, IdentifierInfo);
  virtual Type getType(const State&) override;
  virtual void codegen(Accu&) const override;
  IdentifierInfo identifier;
  struct Argument {
    CodeLoc codeLoc;
    string name;
    unique_ptr<Expression> expr;
  };
  FunctionCallType callType;
  vector<Argument> arguments;
};

struct Statement : Node {
  using Node::Node;
  virtual void check(State&) = 0;
  virtual bool hasReturnStatement(const State&) const;
  virtual void codegen(Accu&) const = 0;
  virtual void declare(Accu&) const;
  enum class TopLevelAllowance {
    CANT,
    CAN,
    MUST
  };
  virtual TopLevelAllowance allowTopLevel() const { return TopLevelAllowance::CANT; }
};

struct VariableDeclaration : Statement {
  VariableDeclaration(CodeLoc, optional<IdentifierInfo> type, string identifier, unique_ptr<Expression> initExpr);
  optional<IdentifierInfo> type;
  optional<Type> realType;
  string identifier;
  unique_ptr<Expression> initExpr;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
};

struct IfStatement : Statement {
  IfStatement(CodeLoc, unique_ptr<Expression> cond, unique_ptr<Statement> ifTrue,
      unique_ptr<Statement> ifFalse /* can be null*/);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> ifTrue, ifFalse;
  virtual bool hasReturnStatement(const State&) const override;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
};

struct StatementBlock : Statement {
  using Statement::Statement;
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement(const State&) const override;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
};

struct ReturnStatement : Statement {
  using Statement::Statement;
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement(const State&) const override;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
};

struct ForLoopStatement : Statement {
  ForLoopStatement(CodeLoc l, unique_ptr<Statement> init, unique_ptr<Expression> cond, unique_ptr<Expression> iter,
      unique_ptr<Statement> body);
  unique_ptr<Statement> init;
  unique_ptr<Expression> cond;
  unique_ptr<Expression> iter;
  unique_ptr<Statement> body;
  virtual bool hasReturnStatement(const State&) const override;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
};

struct FunctionDefinition;

struct StructDefinition : Statement {
  StructDefinition(CodeLoc, string name);
  string name;
  struct Member {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Member> members;
  vector<string> templateParams;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
  virtual void declare(Accu&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct VariantDefinition : Statement {
  VariantDefinition(CodeLoc, string name);
  string name;
  struct Element {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Element> elements;
  vector<string> templateParams;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
  virtual void declare(Accu&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }

  private:
  void generate(Accu&, bool import) const;
};

struct EmbedStructDefinition : Statement {
  EmbedStructDefinition(CodeLoc, string name);
  string name;
  vector<string> templateParams;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct SwitchStatement : Statement {
  SwitchStatement(CodeLoc, unique_ptr<Expression>);
  struct CaseElem {
    CodeLoc codeloc;
    optional<IdentifierInfo> type;
    string id;
    unique_ptr<StatementBlock> block;
    bool declareVar;
  };
  string subtypesPrefix;
  vector<CaseElem> caseElems;
  unique_ptr<StatementBlock> defaultBlock;
  unique_ptr<Expression> expr;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
  virtual bool hasReturnStatement(const State&) const override;
};

struct FunctionDefinition : Statement {
  FunctionDefinition(CodeLoc, IdentifierInfo returnType, string name);
  IdentifierInfo returnType;
  string name;
  struct Parameter {
    CodeLoc codeLoc;
    IdentifierInfo type;
    string name;
  };
  vector<Parameter> parameters;
  unique_ptr<Statement> body;
  vector<string> templateParams;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
  virtual void declare(Accu&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }

  private:
  string getPrototype() const;
};

struct EmbedStatement : Statement {
  EmbedStatement(CodeLoc, string value);
  string value;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
  virtual void declare(Accu&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::CAN; }
  virtual bool hasReturnStatement(const State&) const override;
};

struct AST;

struct ImportStatement : Statement {
  ImportStatement(CodeLoc, string path, bool isPublic);
  string path;
  unique_ptr<AST> ast;
  bool isPublic;
  virtual void check(State&) override;
  virtual void codegen(Accu&) const override;
  virtual void declare(Accu&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct AST {
  vector<unique_ptr<Statement>> elems;
};
