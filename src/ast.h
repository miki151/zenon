#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "context.h"
#include "operator.h"
#include "identifier.h"
#include "type.h"

struct Buffer;
struct Sections;
class MoveChecker;
class ImportCache;
class ASTCache;

struct Statement;

using ExprTransformFun = function<unique_ptr<Expression>(Expression*)>;
using StmtTransformFun = function<unique_ptr<Statement>(Statement*)>;
using ExprVisitFun = function<void(Expression*)>;
using StmtVisitFun = function<void(Statement*)>;
using FunctionCallVisitFun = function<void(SFunctionInfo)>;
using LambdasSet = unordered_set<const LambdaType*>;
using ConceptsSet = unordered_set<const ConceptType*>;

unique_ptr<Statement> identityStmt(Statement*);
unique_ptr<Expression> identityExpr(Expression*);

struct Sections;
struct Buffer;

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Buffer*, Sections*) const {}
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const {}
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const { return success; }
  virtual ~Node() {}
};

extern WithErrorLine<SType> getType(const Context&, unique_ptr<Expression>&, bool evaluateAtCompileTime = true);

struct EvalResult {
  SType value;
  bool isConstant;
};

struct Expression : Node {
  using Node::Node;
  using TransformFun = function<unique_ptr<Expression>(Expression*)>;
  virtual WithErrorLine<SType> getTypeImpl(const Context&) = 0;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const;
  virtual WithEvalError<EvalResult> eval(const Context&) const;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const = 0;
  unique_ptr<Expression> deepCopy() const;
};

struct Constant : Expression {
  Constant(CodeLoc, SType);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  SType value;
  optional<string> structMemberName;
  nullable<SType> refValue;
};

struct EnumConstant : Expression {
  EnumConstant(CodeLoc, string enumName, string enumElement);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  string enumName;
  nullable<SType> enumType;
  string enumElement;
};

struct Variable : Expression {
  Variable(IdentifierInfo);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  IdentifierInfo identifier;
  nullable<SType> getConstantValue(const Context&) const;
};

struct MemberAccessExpression : Expression {
  static unique_ptr<MemberAccessExpression> getPointerAccess(CodeLoc, unique_ptr<Expression> lhs, string);
  MemberAccessExpression(CodeLoc, unique_ptr<Expression> lhs, string);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> lhs;
  string identifier;
  bool isUnion = false;
  nullable<SFunctionInfo> destructorCall;
  bool isMainDestructor = false;
};

struct BinaryExpression : Expression {
  static unique_ptr<Expression> get(CodeLoc, Operator, unique_ptr<Expression>, unique_ptr<Expression>);
  static unique_ptr<Expression> get(CodeLoc, Operator, vector<unique_ptr<Expression>>);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  Operator op;
  vector<unique_ptr<Expression>> expr;
  nullable<SFunctionInfo> functionInfo;
  nullable<SFunctionInfo> destructorCall[2];
  struct Private {};
  BinaryExpression(Private, CodeLoc, Operator, vector<unique_ptr<Expression>>);
  JustError<ErrorLoc> considerDestructorCall(const Context&, int index, const SType& argType);
};


struct UnaryExpression : Expression {
  UnaryExpression(CodeLoc, Operator, unique_ptr<Expression>);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  Operator op;
  unique_ptr<Expression> expr;
  nullable<SFunctionInfo> functionInfo;
  nullable<SFunctionInfo> destructorCall;
};

struct TernaryExpression : Expression {
  TernaryExpression(CodeLoc, unique_ptr<Expression>, unique_ptr<Expression>, unique_ptr<Expression>);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> condExpr;
  unique_ptr<Expression> e1;
  unique_ptr<Expression> e2;
};

struct MoveExpression : Expression {
  MoveExpression(CodeLoc, string, bool hasDestructor = false);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  string identifier;
  nullable<SType> type;
  bool hasDestructor = false;
};

struct CountOfExpression : Expression {
  CountOfExpression(CodeLoc, string);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  string identifier;
  nullable<SType> type;
};

struct VariablePackElement : Expression {
  VariablePackElement(CodeLoc, string packName, unique_ptr<Expression> index);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  string packName;
  unique_ptr<Expression> index;
  optional<string> codegenName;
};

struct StatementExpression : Expression {
  StatementExpression(CodeLoc, vector<unique_ptr<Statement>> statements, unique_ptr<Expression> value);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  vector<unique_ptr<Statement>> statements;
  unique_ptr<Expression> value;
};

struct StatementBlock;

struct FunctionParameter {
  CodeLoc codeLoc;
  IdentifierInfo type;
  optional<string> name;
  bool isMutable;
  bool isVirtual;
};

struct LambdaExpression : Expression {
  LambdaExpression(CodeLoc, vector<FunctionParameter>, unique_ptr<StatementBlock>, optional<IdentifierInfo> returnType,
      LambdaCaptureInfo captureInfo);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  JustError<ErrorLoc> checkBodyMoves() const;
  WithErrorLine<vector<LambdaCapture>> setLambda(Context&);
  vector<FunctionParameter> parameters;
  unique_ptr<StatementBlock> block;
  optional<IdentifierInfo> returnType;
  nullable<shared_ptr<LambdaType>> type;
  LambdaCaptureInfo captureInfo;
  vector<SFunctionInfo> functionCalls;
};

struct ArrayLiteral : Expression {
  ArrayLiteral(CodeLoc);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  vector<unique_ptr<Expression>> contents;
  optional<IdentifierInfo> typeId;
  nullable<SType> type;
};

struct FatPointerConversion : Expression {
  FatPointerConversion(CodeLoc, vector<SFunctionInfo> functions, SType toType, SType argType, unique_ptr<Expression> arg,
      shared_ptr<ConceptType> conceptType);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  SType toType;
  SType argType;
  unique_ptr<Expression> arg;
  shared_ptr<ConceptType> conceptType;
  vector<SFunctionInfo> functions;
};

enum class MethodCallType { METHOD, FUNCTION_AS_METHOD, FUNCTION_AS_METHOD_WITH_POINTER };

struct FunctionCall : Expression {
  FunctionCall(IdentifierInfo, bool methodCall);
  FunctionCall(IdentifierInfo, unique_ptr<Expression> arg, bool methodCall);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  IdentifierInfo identifier;
  optional<vector<SType>> templateArgs;
  nullable<SFunctionInfo> functionInfo;
  vector<unique_ptr<Expression>> arguments;
  vector<optional<string>> argNames;
  optional<MethodCallType> callType;
  nullable<SFunctionInfo> destructorCall;
  bool methodCall = false;
  bool variadicArgs = false;
  bool variadicTemplateArgs = false;
  struct Private {};
  FunctionCall(CodeLoc, bool methodCall, Private);

  private:
  JustError<ErrorLoc> checkNamedArgs() const;
  JustError<ErrorLoc> checkVariadicCall(const Context&);
  JustError<ErrorLoc> considerSpecialCalls(const Context&);
};

struct AST;

struct AttributeInfo {
  string name;
  CodeLoc codeLoc;
};

using StatementEvalResult = vector<unique_ptr<Statement>>;

struct Statement : Node {
  using Node::Node;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext);
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool topLevelNotInImport = false) { return success; }
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override final;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const;
  unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const;
  NODISCARD virtual bool hasReturnStatement() const;
  virtual void addGeneratedConstructor(Context&, const AST&) const {}
  virtual void codegen(Buffer*, Sections*) const override {}
  virtual unique_ptr<Statement> replaceVar(string from, string to) const;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const = 0;
  virtual WithEvalError<StatementEvalResult> eval(Context&);
  virtual bool canHaveAttributes() const { return false; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*) { return success; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry* r, ASTCache&,
      const vector<string>& importDirs) {
    return registerTypes(primaryContext, r);
  }
  unique_ptr<Statement> deepCopy() const;
  enum class TopLevelAllowance {
    CANT,
    CAN,
    MUST
  };
  virtual TopLevelAllowance allowTopLevel() const { return TopLevelAllowance::CANT; }
  bool exported = false;
  vector<AttributeInfo> attributes;
};

struct VariableDeclaration : Statement {
  VariableDeclaration(CodeLoc, optional<IdentifierInfo> type, string identifier, unique_ptr<Expression> initExpr);
  optional<IdentifierInfo> type;
  nullable<SType> realType;
  string identifier;
  unique_ptr<Expression> initExpr;
  bool isMutable = false;
  unique_ptr<Statement> destructorCall;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual bool canHaveAttributes() const override { return true; }

  private:
  WithErrorLine<SType> getRealType(const Context&) const;
  WithErrorLine<bool> considerShadowing();
};

struct AliasDeclaration : Statement {
  AliasDeclaration(CodeLoc, string identifier, unique_ptr<Expression> initExpr);
  nullable<SType> realType;
  string identifier;
  unique_ptr<Expression> initExpr;
  unique_ptr<Statement> destructorCall;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;

  private:
  WithErrorLine<SType> getRealType(const Context&) const;
};

struct ExternConstantDeclaration : Statement {
  ExternConstantDeclaration(CodeLoc, IdentifierInfo type, string identifier);
  IdentifierInfo type;
  nullable<SType> realType;
  string identifier;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct IfStatement : Statement {
  IfStatement(CodeLoc,
      unique_ptr<VariableDeclaration> decl /*can be null*/,
      unique_ptr<Expression> cond /*can be null if decl is non-null*/,
      unique_ptr<Statement> ifTrue,
      unique_ptr<Statement> ifFalse /* can be null*/);
  unique_ptr<VariableDeclaration> declaration;
  unique_ptr<Expression> condition;
  unique_ptr<Statement> ifTrue, ifFalse;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
};

struct StatementBlock : Statement {
  using Statement::Statement;
  StatementBlock(CodeLoc, vector<unique_ptr<Statement>>);
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
};

struct ExternalStatement : Statement {
  ExternalStatement(Statement*);
  Statement* elem;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
};

struct UncheckedStatement : Statement {
  UncheckedStatement(CodeLoc, unique_ptr<Statement>);
  unique_ptr<Statement> elem;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
};

struct ReturnStatement : Statement {
  ReturnStatement(CodeLoc);
  ReturnStatement(CodeLoc, unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
};

struct BreakStatement : Statement {
  using Statement::Statement;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
};

struct ContinueStatement : Statement {
  using Statement::Statement;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual bool hasReturnStatement() const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
  bool canDiscard = false;
  bool noReturnExpr = false;
  bool isConstant = false;
};

unique_ptr<Statement> getForLoop(CodeLoc, unique_ptr<VariableDeclaration>, unique_ptr<Expression> cond,
      unique_ptr<Expression> iter, unique_ptr<Statement> body);

unique_ptr<Statement> getRangedLoop(CodeLoc, string iterator, unique_ptr<Expression> container,
      unique_ptr<Statement> body);

struct WhileLoopStatement : Statement {
  WhileLoopStatement(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Statement> body,
      unique_ptr<Statement> afterContinue);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> body;
  unique_ptr<Statement> afterContinue;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual WithEvalError<StatementEvalResult> eval(Context&) override;
};

struct FunctionDefinition;

struct TemplateParameter {
  string name;
  optional<string> type;
  CodeLoc codeLoc;
};

struct TemplateInfo {
  vector<TemplateParameter> params;
  bool variadic = false;
  struct ConceptRequirement {
    IdentifierInfo identifier;
    bool variadic = false;
  };
  using Requirement = variant<ConceptRequirement, shared_ptr<Expression>>;
  vector<Requirement> requirements;
};

struct AttributeDefinition : Statement {
  AttributeDefinition(CodeLoc, string name);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  string name;
};

struct StructDefinition : Statement {
  StructDefinition(CodeLoc, string name);
  string name;
  struct Member {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
    bool isConst;
  };
  vector<Member> members;
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void addGeneratedConstructor(Context&, const AST&) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual bool canHaveAttributes() const override { return true; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  bool external = false;
  bool checked = false;
};

struct UnionDefinition : Statement {
  UnionDefinition(CodeLoc, string name);
  string name;
  struct Element {
    IdentifierInfo type;
    string name;
    CodeLoc codeLoc;
  };
  vector<Element> elements;
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual bool canHaveAttributes() const override { return true; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  bool checked = false;
};

struct ConceptDefinition : Statement {
  ConceptDefinition(CodeLoc, string name);
  string name;
  vector<unique_ptr<FunctionDefinition>> functions;
  TemplateInfo templateInfo;
  struct FatPointerInfo {
    SType type;
    vector<SFunctionInfo> vTable;
  };
  void addFatPointer(FatPointerInfo);
  void addConceptType(shared_ptr<ConceptType>);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;

  private:
  vector<FatPointerInfo> fatPointers;
  vector<shared_ptr<ConceptType>> conceptInstances;
};

struct EnumDefinition : Statement {
  EnumDefinition(CodeLoc, string name);
  string name;
  vector<string> elements;
  bool external = false;
  bool registered = false;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct SwitchStatement : Statement {
  SwitchStatement(CodeLoc, unique_ptr<Expression>);
  struct CaseElem {
    CodeLoc codeloc;
    vector<string> ids;
    optional<string> declaredVar;
    unique_ptr<StatementBlock> block;
    CaseElem transform(const StmtTransformFun&, const ExprTransformFun&) const;
    void visit(const StmtVisitFun&, const ExprVisitFun&) const;
  };
  nullable<SType> targetType;
  vector<CaseElem> caseElems;
  unique_ptr<StatementBlock> defaultBlock;
  unique_ptr<Expression> expr;
  nullable<SFunctionInfo> destructorCall;
  enum { ENUM, UNION} type;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual bool hasReturnStatement() const override;

  private:
  void codegenEnum(Buffer*, Sections*) const;
  void codegenUnion(Buffer*, Sections*) const;
};

struct FunctionDefinition : Statement {
  FunctionDefinition(CodeLoc, IdentifierInfo returnType, FunctionId);
  IdentifierInfo returnType;
  FunctionId name;
  using Parameter = FunctionParameter;
  vector<Parameter> parameters;
  vector<unique_ptr<Statement>> destructorCalls;
  unique_ptr<StatementBlock> body, origBody;
  struct InstanceInfo {
    unique_ptr<StatementBlock> body;
    vector<unique_ptr<Statement>> destructorCalls;
    SFunctionInfo functionInfo;
    Context callContext;
  };
  vector<InstanceInfo> instances;
  TemplateInfo templateInfo;
  nullable<SFunctionInfo> functionInfo;
  optional<Context> definitionContext;
  bool wasChecked = false;
  bool external = false;
  bool isVirtual = false;
  bool isDefault = false;
  bool isVariadicParams = false;
  bool wasUsed = false;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext) override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual void codegenInstance(Buffer*, Sections*, FunctionInfo*) const;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  NODISCARD JustError<ErrorLoc> setFunctionSignature(const Context&, nullable<SConcept> concept = nullptr, bool builtInImport = false);
  void handlePointerParamsInOperator(Buffer*, Sections*, const StatementBlock*) const;
  void handlePointerReturnInOperator(Buffer*, Sections*, const StatementBlock*) const;
  void addStacktraceGenerator(Buffer*, Sections*, const StatementBlock*) const;
  NODISCARD JustError<ErrorLoc> generateVirtualDispatchBody(Context& bodyContext);
  WithErrorLine<unique_ptr<Expression>> getVirtualFunctionCallExpr(const Context&, const string& funName,
      const string& alternativeName, const SType& alternativeType, int virtualIndex, bool lvalueParam);
  WithErrorLine<unique_ptr<Expression>> getVirtualOperatorCallExpr(Context&, Operator,
      const string& alternativeName, const SType& alternativeType, int virtualIndex, int lvalueParam);
  NODISCARD JustError<ErrorLoc> checkAndGenerateCopyFunction(const Context&, const string&);
  NODISCARD JustError<ErrorLoc> checkAndGenerateDefaultConstructor(const Context&);
  NODISCARD JustError<ErrorLoc> addInstance(const Context& callContext, const SFunctionInfo&);
  NODISCARD JustError<ErrorLoc> generateDefaultBodies(Context&);
  NODISCARD JustError<ErrorLoc> checkBody(const Context& callContext, StatementBlock& myBody,
      const FunctionInfo& instanceInfo, vector<unique_ptr<Statement> >& destructorCalls) const;
  void addParamsToContext(Context&, const FunctionInfo&) const;
  NODISCARD JustError<ErrorLoc> checkForIncompleteTypes(const Context&);
  WithErrorLine<SType> getReturnType(const Context&) const;
  JustError<ErrorLoc> handleIsMemberParamsFunction();
};

struct EmbedStatement : Statement {
  EmbedStatement(CodeLoc, string value);
  string value;
  bool isTopLevel = false;
  vector<Context::SubstitutionInfo> replacements;
  bool returns = false;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual bool hasReturnStatement() const override;
};

struct MixinStatement : Statement {
  MixinStatement(CodeLoc, unique_ptr<Expression>);
  unique_ptr<Expression> value;
  unique_ptr<Statement> result;
  bool returns = false;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
};

struct StaticStatement : Statement {
  StaticStatement(CodeLoc, unique_ptr<Statement>);
  unique_ptr<Statement> value;
  vector<unique_ptr<Statement>> results;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual bool hasReturnStatement() const override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual bool canHaveAttributes() const override;
};

struct AST {
  vector<unique_ptr<Statement>> elems;
};

struct ImportStatement : Statement {
  ImportStatement(CodeLoc, string path, bool isBuiltIn);
  string path;
  optional<string> absolutePath;
  AST* ast = nullptr;
  const bool isBuiltIn;
  virtual JustError<ErrorLoc> addToContext(Context&, ImportCache& cache, const Context& primaryContext) override;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Buffer*, Sections*) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*, ASTCache&,
      const vector<string>& importDirs) override;
  virtual unique_ptr<Statement> transformImpl(const StmtTransformFun&, const ExprTransformFun&) const override;
};

struct ModuleInfo {
  string path;
  bool builtIn;
};

extern WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST&, Context& context, const Context& primary, ImportCache&, bool builtIn);
extern Context createPrimaryContext(TypeRegistry*);
extern WithErrorLine<SFunctionInfo> getCopyFunction(const Context&, CodeLoc callLoc, const SType&);
extern WithErrorLine<SFunctionInfo> getImplicitCopyFunction(const Context&, CodeLoc callLoc, const SType&);
extern WithError<vector<SFunctionInfo> > getRequiredFunctionsForConceptType(const Context& context,
    const Concept& concept, CodeLoc codeLoc);
