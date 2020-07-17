#pragma once

#include "stdafx.h"
#include "variant.h"
#include "code_loc.h"
#include "context.h"
#include "operator.h"
#include "identifier.h"
#include "type.h"

struct Accu;
class MoveChecker;
class ImportCache;
class ASTCache;

struct CodegenStage {
  static CodegenStage types();
  static CodegenStage define();
  static CodegenStage declare();
  CodegenStage setImport();
  bool isTypes;
  bool isDefine;
  bool isImport;

  bool operator < (const CodegenStage& o) const {
    return std::forward_as_tuple(isTypes, isDefine, isImport) < std::forward_as_tuple(o.isTypes, o.isDefine, o.isImport);
  }

  private:
  CodegenStage() {}
};

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

struct Node {
  Node(CodeLoc);
  CodeLoc codeLoc;
  virtual void codegen(Accu&, CodegenStage) const {}
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const {}
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const;
  virtual void addLambdas(LambdasSet&) const;
  virtual void addConceptTypes(ConceptsSet&) const;
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
  Constant(CodeLoc, SCompileTimeValue);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  SCompileTimeValue value;
  optional<string> structMemberName;
};

struct EnumConstant : Expression {
  EnumConstant(CodeLoc, string enumName, string enumElement);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
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
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  IdentifierInfo identifier;
};

struct MemberAccessExpression : Expression {
  static unique_ptr<MemberAccessExpression> getPointerAccess(CodeLoc, unique_ptr<Expression> lhs, string);
  MemberAccessExpression(CodeLoc, unique_ptr<Expression> lhs, string);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> lhs;
  string identifier;
  bool isUnion = false;
  nullable<SFunctionInfo> destructorCall;
};

struct MemberIndexExpression : Expression {
  MemberIndexExpression(CodeLoc, unique_ptr<Expression> lhs, unique_ptr<Expression> index);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  unique_ptr<Expression> lhs;
  unique_ptr<Expression> index;
  optional<string> memberName;
  bool isUnion = false;
  nullable<SFunctionInfo> destructorCall;
};

struct BinaryExpression : Expression {
  static unique_ptr<Expression> get(CodeLoc, Operator, unique_ptr<Expression>, unique_ptr<Expression>);
  static unique_ptr<Expression> get(CodeLoc, Operator, vector<unique_ptr<Expression>>);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
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
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
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
  virtual void codegen(Accu&, CodegenStage) const override;
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
  virtual void codegen(Accu&, CodegenStage) const override;
  string identifier;
  nullable<SType> type;
  bool hasDestructor = false;
};

struct CountOfExpression : Expression {
  CountOfExpression(CodeLoc, string);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  string identifier;
  nullable<SType> type;
};

struct VariablePackElement : Expression {
  VariablePackElement(CodeLoc, string packName, unique_ptr<Expression> index);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  string packName;
  unique_ptr<Expression> index;
  optional<string> codegenName;
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
  virtual void addLambdas(LambdasSet&) const override;
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
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
  virtual void codegen(Accu&, CodegenStage) const override;
  vector<unique_ptr<Expression>> contents;
  optional<IdentifierInfo> typeId;
  nullable<SType> type;
};

struct FatPointerConversion : Expression {
  FatPointerConversion(CodeLoc, IdentifierInfo toType, unique_ptr<Expression> arg);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  virtual void addConceptTypes(ConceptsSet&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  IdentifierInfo toType;
  unique_ptr<Expression> arg;
  nullable<SType> argType;
  nullable<shared_ptr<ConceptType>> conceptType;
  vector<SFunctionInfo> functions;
};

enum class MethodCallType { METHOD, FUNCTION_AS_METHOD, FUNCTION_AS_METHOD_WITH_POINTER };

struct FunctionCall : Expression {
  FunctionCall(CodeLoc, IdentifierInfo, bool methodCall);
  FunctionCall(CodeLoc, IdentifierInfo, unique_ptr<Expression> arg, bool methodCall);
  virtual WithErrorLine<SType> getTypeImpl(const Context&) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual WithEvalError<EvalResult> eval(const Context&) const override;
  virtual unique_ptr<Expression> replaceVar(string from, string to) const override;
  virtual unique_ptr<Expression> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override;
  optional<IdentifierInfo> identifier;
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
};

struct AST;

struct AttributeInfo {
  string name;
  CodeLoc codeLoc;
};

struct Statement : Node {
  using Node::Node;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&, ImportCache&, const Context& primaryContext);
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool topLevelNotInImport = false) { return success; }
  NODISCARD virtual JustError<ErrorLoc> checkMoves(MoveChecker&) const override final;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const;
  NODISCARD virtual bool hasReturnStatement(const Context&) const;
  virtual void addGeneratedConstructor(Context&, const AST&) const {}
  virtual void codegen(Accu&, CodegenStage) const override {}
  virtual unique_ptr<Statement> replaceVar(string from, string to) const;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const;
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
  bool isStatic = false;
  unique_ptr<Statement> destructorCall;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
};

struct ExternConstantDeclaration : Statement {
  ExternConstantDeclaration(CodeLoc, IdentifierInfo type, string identifier);
  IdentifierInfo type;
  nullable<SType> realType;
  string identifier;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
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
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
};

struct StatementBlock : Statement {
  using Statement::Statement;
  StatementBlock(CodeLoc, vector<unique_ptr<Statement>>);
  vector<unique_ptr<Statement>> elems;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;  
};

struct ExternalStatement : Statement {
  ExternalStatement(Statement*);
  Statement* elem;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
};

struct UncheckedStatement : Statement {
  UncheckedStatement(CodeLoc, unique_ptr<Statement>);
  unique_ptr<Statement> elem;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
};

struct ReturnStatement : Statement {
  using Statement::Statement;
  ReturnStatement(CodeLoc, unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  virtual bool hasReturnStatement(const Context&) const override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct BreakStatement : Statement {
  using Statement::Statement;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ContinueStatement : Statement {
  using Statement::Statement;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct ExpressionStatement : Statement {
  ExpressionStatement(unique_ptr<Expression>);
  unique_ptr<Expression> expr;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual bool hasReturnStatement(const Context&) const override;
  bool canDiscard = false;
  bool noReturnExpr = false;
};

struct ForLoopStatement : Statement {
  ForLoopStatement(CodeLoc l, unique_ptr<Statement> init, unique_ptr<Expression> cond, unique_ptr<Expression> iter,
      unique_ptr<Statement> body);
  unique_ptr<Statement> init;
  unique_ptr<Expression> cond;
  unique_ptr<Expression> iter;
  unique_ptr<Statement> body;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct StaticForLoopStatement : Statement {
  StaticForLoopStatement(CodeLoc l, string counter, unique_ptr<Expression> init, unique_ptr<Expression> cond, unique_ptr<Expression> iter,
      unique_ptr<Statement> body);
  string counter;
  unique_ptr<Expression> init;
  unique_ptr<Expression> cond;
  unique_ptr<Expression> iter;
  unique_ptr<Statement> body;
  vector<unique_ptr<Statement>> unrolled;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  JustError<ErrorLoc> checkExpressions(const Context&) const;
  WithErrorLine<vector<unique_ptr<Statement>>> getUnrolled(const Context&, SType counterType) const;
};

struct RangedLoopStatement : Statement {
  RangedLoopStatement(CodeLoc l, unique_ptr<VariableDeclaration> init, unique_ptr<Expression> container,
      unique_ptr<Statement> body);
  unique_ptr<VariableDeclaration> init;
  unique_ptr<Expression> container;
  unique_ptr<Expression> condition;
  unique_ptr<Expression> increment;
  unique_ptr<Statement> body;
  optional<string> containerName;
  unique_ptr<VariableDeclaration> containerEnd;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
};

struct WhileLoopStatement : Statement {
  WhileLoopStatement(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Statement> body);
  unique_ptr<Expression> cond;
  unique_ptr<Statement> body;
  int loopId = 0;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  NODISCARD virtual JustError<ErrorLoc> checkMovesImpl(MoveChecker&) const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
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
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  string name;
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
  TemplateInfo templateInfo;
  nullable<shared_ptr<StructType>> type;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void addGeneratedConstructor(Context&, const AST&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual bool canHaveAttributes() const override { return true; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;
  bool external = false;
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
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual JustError<ErrorLoc> registerTypes(const Context& primaryContext, TypeRegistry*) override;
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
  void addFatPointer(FatPointerInfo, shared_ptr<ConceptType>);
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }

  private:
  vector<FatPointerInfo> fatPointers;
  vector<shared_ptr<ConceptType>> conceptInstances;
};

struct EnumDefinition : Statement {
  EnumDefinition(CodeLoc, string name);
  string name;
  vector<string> elements;
  bool external = false;
  NODISCARD virtual JustError<ErrorLoc> addToContext(Context&) override;
  NODISCARD virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*) override;
};

struct SwitchStatement : Statement {
  SwitchStatement(CodeLoc, unique_ptr<Expression>);
  struct CaseElem {
    CodeLoc codeloc;
    // returns null if there is no type in the element or error if there was an error getting the type
    WithErrorLine<nullable<SType>> getType(const Context&);
    optional<IdentifierInfo> type;
    vector<string> ids;
    optional<string> declaredVar;
    unique_ptr<StatementBlock> block;
    enum VarType { VALUE, POINTER, NONE } varType = NONE;
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
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual void visit(const StmtVisitFun&, const ExprVisitFun&) const override;
  virtual void addFunctionCalls(const FunctionCallVisitFun&) const override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual bool hasReturnStatement(const Context&) const override;

  private:
  void codegenEnum(Accu&) const;
  void codegenUnion(Accu&) const;
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
    vector<SFunctionInfo> requirements;
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
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  NODISCARD JustError<ErrorLoc> setFunctionType(const Context&, nullable<SConcept> concept = nullptr, bool builtInImport = false);
  void handlePointerParamsInOperator(Accu&, const StatementBlock*) const;
  void handlePointerReturnInOperator(Accu&, const StatementBlock*) const;
  void addStacktraceGenerator(Accu&, const StatementBlock*) const;
  NODISCARD JustError<ErrorLoc> generateVirtualDispatchBody(Context& bodyContext);
  WithErrorLine<unique_ptr<Expression>> getVirtualFunctionCallExpr(const Context&, const string& funName,
      const string& alternativeName, const SType& alternativeType, int virtualIndex);
  WithErrorLine<unique_ptr<Expression>> getVirtualOperatorCallExpr(Context&, Operator,
      const string& alternativeName, const SType& alternativeType, int virtualIndex);
  NODISCARD JustError<ErrorLoc> checkAndGenerateCopyFunction(const Context&, const string&);
  NODISCARD JustError<ErrorLoc> checkAndGenerateDefaultConstructor(const Context&);
  NODISCARD JustError<ErrorLoc> addInstance(const Context* callContext, const SFunctionInfo&);
  NODISCARD JustError<ErrorLoc> generateDefaultBodies(Context&);
  NODISCARD JustError<ErrorLoc> checkBody(const vector<SFunctionInfo>& requirements, StatementBlock& myBody,
      const FunctionInfo& instanceInfo, vector<unique_ptr<Statement> >& destructorCalls) const;
  void addParamsToContext(Context&, const FunctionInfo&) const;
  NODISCARD JustError<ErrorLoc> checkForIncompleteTypes(const Context&);
  WithErrorLine<SType> getReturnType(const Context&) const;
};

struct EmbedStatement : Statement {
  EmbedStatement(CodeLoc, string value);
  string value;
  bool isTopLevel = false;
  vector<Context::SubstitutionInfo> replacements;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override;
  virtual unique_ptr<Statement> transform(const StmtTransformFun&, const ExprTransformFun&) const override;
  virtual bool hasReturnStatement(const Context&) const override;
};

struct AST;

struct ImportStatement : Statement {
  ImportStatement(CodeLoc, string path, bool isBuiltIn);
  string path;
  optional<string> absolutePath;
  AST* ast = nullptr;
  const bool isBuiltIn;
  virtual JustError<ErrorLoc> addToContext(Context&, ImportCache& cache, const Context& primaryContext) override;
  virtual JustError<ErrorLoc> check(Context&, bool = false) override;
  virtual void codegen(Accu&, CodegenStage) const override;
  virtual TopLevelAllowance allowTopLevel() const override { return TopLevelAllowance::MUST; }
  virtual JustError<ErrorLoc> registerTypes(const Context&, TypeRegistry*, ASTCache&,
      const vector<string>& importDirs) override;
};

struct AST {
  vector<unique_ptr<Statement>> elems;
};

struct ModuleInfo {
  string path;
  bool builtIn;
};

extern WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST&, Context& context, const Context& primary, ImportCache&, bool builtIn);
extern Context createPrimaryContext(TypeRegistry*);
extern WithErrorLine<SFunctionInfo> getCopyFunction(const Context&, CodeLoc callLoc, const SType&);
extern WithErrorLine<SFunctionInfo> getImplicitCopyFunction(const Context&, CodeLoc callLoc, const SType&);
