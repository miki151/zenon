#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "context.h"
#include "operator.h"
#include "identifier.h"
#include "function_call_type.h"
#include "type.h"

struct Accu;

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  enum CodegenStage { IMPORT, DECLARE, DEFINE };
  virtual void codegen(Accu&, CodegenStage) const {}
  virtual ~Node() {}
};

struct Expression : Node {
  using Node::Node;
  virtual SType getType(const Context&) = 0;
  virtual nullable<SType> getDotOperatorType(Expression* left, const Context& callContext);
  virtual void codegenDotOperator(Accu&, CodegenStage, Expression* leftSide) const;
};

struct Constant : Expression {
  Constant(CodeLoc, SType, string value);
  virtual SType getType(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  SType type;
  string value;
};

struct EnumConstant : Expression {
  EnumConstant(CodeLoc, string enumName, string enumElement);
  virtual SType getType(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  string enumName;
  string enumElement;
};

struct Variable : Expression {
  Variable(CodeLoc, string);
  virtual SType getType(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual nullable<SType> getDotOperatorType(Expression* left, const Context& callContext) override;
  string identifier;
};

struct BinaryExpression : Expression {
  BinaryExpression(CodeLoc, Operator, unique_ptr<Expression>, unique_ptr<Expression>);
  virtual SType getType(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  Operator op;
  unique_ptr<Expression> e1, e2;
};

struct UnaryExpression : Expression {
  UnaryExpression(CodeLoc, Operator, unique_ptr<Expression>);
  virtual SType getType(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  Operator op;
  unique_ptr<Expression> expr;
};

struct FunctionCall : Expression {
  FunctionCall(CodeLoc, IdentifierInfo);
  virtual SType getType(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual nullable<SType> getDotOperatorType(Expression* left, const Context& callContext) override;
  virtual void codegenDotOperator(Accu&, CodegenStage, Expression* leftSide) const override;
  IdentifierInfo identifier;
  optional<FunctionType> functionType;
  vector<unique_ptr<Expression>> arguments;
  bool methodCall = false;
  bool extractPointer = false;
};

struct FunctionCallNamedArgs : Expression {
  FunctionCallNamedArgs(CodeLoc, IdentifierInfo);
  virtual SType getType(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual nullable<SType> getDotOperatorType(Expression* left, const Context& callContext) override;
  virtual void codegenDotOperator(Accu&, CodegenStage, Expression* leftSide) const override;
  struct ArgMatching {
    vector<SType> args;
    vector<CodeLoc> codeLocs;
  };
  WithErrorLine<ArgMatching> matchArgs(const Context& functionContext, const Context& callContext, bool skipFirst);
  IdentifierInfo identifier;
  struct Argument {
    CodeLoc codeLoc;
    string name;
    unique_ptr<Expression> expr;
  };
  optional<FunctionType> functionType;
  vector<Argument> arguments;
  bool methodCall = false;
  bool extractPointer = false;
};

struct Statement : Node {
  using Node::Node;
  virtual void addToContext(Context&);
  virtual void check(Context&) = 0;
  virtual bool hasReturnStatement(const Context&) const;
  virtual void codegen(Accu&, CodegenStage) const = 0;
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
  nullable<SType> realType;
  string identifier;
  unique_ptr<Expression> initExpr;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct IfStatement : Statement {
  IfStatement(CodeLoc, unique_ptr<Expression> cond, unique_ptr<Statement> ifTrue,
      unique_ptr<Statement> ifFalse /* can be null*/);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> ifTrue, ifFalse;
  virtual bool hasReturnStatement(const Context&) const override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct StatementBlock : Statement {
  using Statement::Statement;
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement(const Context&) const override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ReturnStatement : Statement {
  using Statement::Statement;
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement(const Context&) const override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ForLoopStatement : Statement {
  ForLoopStatement(CodeLoc l, unique_ptr<Statement> init, unique_ptr<Expression> cond, unique_ptr<Expression> iter,
      unique_ptr<Statement> body);
  unique_ptr<Statement> init;
  unique_ptr<Expression> cond;
  unique_ptr<Expression> iter;
  unique_ptr<Statement> body;
  virtual bool hasReturnStatement(const Context&) const override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct WhileLoopStatement : Statement {
  WhileLoopStatement(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Statement> body);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> body;
  virtual bool hasReturnStatement(const Context&) const override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct FunctionDefinition;

struct TemplateParameter {
  string name;
  CodeLoc codeLoc;
};

struct TemplateInfo {
  vector<TemplateParameter> params;
  vector<IdentifierInfo> requirements;
};

struct StructDefinition : Statement {
  StructDefinition(CodeLoc, string name);
  string name;
  struct Member {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Member> members;
  vector<unique_ptr<FunctionDefinition>> methods;
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  virtual void addToContext(Context&) override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  bool external = false;
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
  vector<unique_ptr<FunctionDefinition>> methods;
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  virtual void addToContext(Context&) override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct ConceptDefinition : Statement {
  ConceptDefinition(CodeLoc, string name);
  string name;
  struct Type {
    vector<unique_ptr<FunctionDefinition>> methods;
    string name;
    CodeLoc codeLoc;
  };
  vector<Type> types;
  TemplateInfo templateInfo;
  virtual void addToContext(Context&) override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct EnumDefinition : Statement {
  EnumDefinition(CodeLoc, string name);
  string name;
  vector<string> elements;
  vector<unique_ptr<FunctionDefinition>> methods;
  virtual void addToContext(Context&) override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct SwitchStatement : Statement {
  SwitchStatement(CodeLoc, unique_ptr<Expression>);
  struct CaseElem {
    CodeLoc codeloc;
    optional<IdentifierInfo> type;
    string id;
    unique_ptr<StatementBlock> block;
    enum VarType { VALUE, POINTER, NONE } varType = NONE;
  };
  string subtypesPrefix;
  vector<CaseElem> caseElems;
  unique_ptr<StatementBlock> defaultBlock;
  unique_ptr<Expression> expr;
  enum { ENUM, VARIANT} type;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual bool hasReturnStatement(const Context&) const override;

  private:
  void codegenEnum(Accu&) const;
  void codegenVariant(Accu&) const;
};

struct FunctionDefinition : Statement {
  FunctionDefinition(CodeLoc, IdentifierInfo returnType, FunctionName);
  IdentifierInfo returnType;
  FunctionName name;
  struct Parameter {
    CodeLoc codeLoc;
    IdentifierInfo type;
    string name;
  };
  vector<Parameter> parameters;
  unique_ptr<Statement> body;
  TemplateInfo templateInfo;
  optional<FunctionType> functionType;
  virtual void check(Context&) override;
  virtual void addToContext(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  void setFunctionType(const Context&);
  void checkFunction(Context&, bool templateStruct);
  void addSignature(Accu& accu, string structName) const;
};

struct EmbedStatement : Statement {
  EmbedStatement(CodeLoc, string value);
  string value;
  bool isPublic = false;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override;
  virtual bool hasReturnStatement(const Context&) const override;
};

struct AST;

struct ImportStatement : Statement {
  ImportStatement(CodeLoc, string path, bool isPublic);
  string path;
  unique_ptr<AST> ast;
  bool isPublic;
  virtual void addToContext(Context&) override;
  virtual void check(Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
};

struct AST {
  vector<unique_ptr<Statement>> elems;
};
