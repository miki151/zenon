#include "stdafx.h"
#include "ast.h"
#include "type.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"
#include "code_loc.h"
#include "type_registry.h"
#include "move_checker.h"
#include "import_cache.h"
#include "ast_cache.h"

Node::Node(CodeLoc l) : codeLoc(l) {}

unique_ptr<Expression> Expression::replaceVar(string from, string to) const {
  return transform(
      [&](Statement* expr) { return expr->replaceVar(from, to); },
      [&](Expression* expr) { return expr->replaceVar(from, to); });
}

void Node::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  visit(
      [&](Statement* expr) { expr->addFunctionCalls(fun); },
      [&](Expression* expr) { expr->addFunctionCalls(fun); });
}

void Node::addLambdas(LambdasSet& lambdas) const {
  visit(
      [&](Statement* expr) { expr->addLambdas(lambdas); },
      [&](Expression* expr) { expr->addLambdas(lambdas); });
}

void Node::addConceptTypes(ConceptsSet& types) const {
  visit(
      [&](Statement* expr) { expr->addConceptTypes(types); },
      [&](Expression* expr) { expr->addConceptTypes(types); });
}

unique_ptr<Statement> identityStmt(Statement* expr) {
  return expr->transform(&identityStmt, &identityExpr);
}

unique_ptr<Expression> identityExpr(Expression* expr) {
  return expr->transform(&identityStmt, &identityExpr);
}

unique_ptr<Expression> Expression::deepCopy() const {
  return transform(&identityStmt, &identityExpr);
}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<VariableDeclaration> d, unique_ptr<Expression> c,
    unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), declaration(std::move(d)), condition(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, SType v) : Expression(l), value(v) {
  INFO << "Created constant " << v->getName() << " of type " << v->getType();
}

Variable::Variable(IdentifierInfo s) : Expression(s.codeLoc), identifier(std::move(s)) {
}

FunctionCall::FunctionCall(IdentifierInfo id, bool methodCall) : Expression(id.codeLoc), identifier(std::move(id)),
    methodCall(methodCall), variadicTemplateArgs(identifier.parts.back().variadic) {
  INFO << "Function call " << id.prettyString();;
}

FunctionCall::FunctionCall(IdentifierInfo id, unique_ptr<Expression> arg, bool methodCall)
    : FunctionCall(id, methodCall) {
  arguments.push_back(std::move(arg));
}

VariableDeclaration::VariableDeclaration(CodeLoc l, optional<IdentifierInfo> t, string id, unique_ptr<Expression> ini)
    : Statement(l), type(t), identifier(id), initExpr(std::move(ini)) {
  string type = "auto";
  if (t)
    type = t->prettyString();
  INFO << "Declared variable " << quote(id) << " of type " << quote(type);
}

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, FunctionId name)
  : Statement(l), returnType(std::move(r)), name(name) {}

WithErrorLine<SType> Constant::getTypeImpl(const Context&) {
  return value->getType();
}

WithEvalError<EvalResult> Constant::eval(const Context&) const {
  return EvalResult{ value, true};
}

unique_ptr<Expression> Constant::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<Constant>(codeLoc, value);
}

WithErrorLine<SType> Variable::getTypeImpl(const Context& context) {
  optional<string> varError;
  if (auto id = identifier.asBasicIdentifier()) {
    if (auto varType = context.getTypeOfVariable(*id))
      return *varType;
    else
      varError = varType.get_error();
  }
  if (auto t = getConstantValue(context))
    return t->getType();
  return codeLoc.getError(varError.value_or("Identifier not found: " + identifier.prettyString()));
}

WithEvalError<EvalResult> Variable::eval(const Context& context) const {
  if (auto res = getConstantValue(context))
    return EvalResult { res.get(), true};
  return EvalError::noEval();
}

unique_ptr<Expression> Variable::replaceVar(string from, string to) const {
  if (identifier.asBasicIdentifier() == from)
    return unique<Variable>(IdentifierInfo(to, codeLoc));
  else
    return deepCopy();
}

unique_ptr<Expression> Variable::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return unique<Variable>(identifier);
}

JustError<ErrorLoc> Variable::checkMoves(MoveChecker& checker) const {
  if (auto id = identifier.asBasicIdentifier())
    TRY(checker.getUsageError(*id).addCodeLoc(codeLoc));
  return success;
}

nullable<SType> Variable::getConstantValue(const Context& context) const {
  if (auto id = identifier.asBasicIdentifier())
    if (!!context.getTypeOfVariable(*id))
      return nullptr;
  if (auto t = context.getTypeFromString(identifier))
    return t.get();
  if (auto id = identifier.asBasicIdentifier()) {
    auto overloads = context.getFunctions(*id);
    if (!overloads.empty())
      return SType(CompileTimeValue::get(FunctionType::get(*id, std::move(overloads))));
  }
  return nullptr;
}

template <typename Comp>
static bool exactArgs(const vector<SType>& argTypes, const FunctionSignature& f, Comp comp) {
  if (f.params.size() != argTypes.size() || !f.templateParams.empty())
    return false;
  for (int i = 0; i < f.params.size(); ++i)
    if (!comp(argTypes[i], f.params[i]))
      return false;
  return true;
}

template <typename Comp>
static bool exactFirstArg(const vector<SType>& argTypes, const FunctionSignature& overload, Comp comp) {
  return !argTypes.empty() && !overload.params.empty() && comp(argTypes[0], overload.params[0]);
}

static bool fromConcept(const vector<SType>&, const SFunctionInfo& f) {
  return !!f->type.concept;
}

static bool userDefinedConstructor(const vector<SType>&, const SFunctionInfo& f) {
  return !f->type.generatedConstructor;
}

static void filterOverloads(const Context& context, vector<SFunctionInfo>& overloads, const vector<SType>& argTypes) {
  auto filter = [&] (auto fun, const char* method) {
    vector<SFunctionInfo> better;
    for (auto& overload : overloads)
      if (fun(argTypes, overload)) {
        better.push_back(overload);
        //cout << overload.toString() << " chosen by " << method << endl;
      }
    if (!better.empty())
      overloads = better;
  };
  auto isExactArg = [] (SType arg, SType param) {
    return param == arg;
  };
  auto isExactValueArg = [] (SType arg, SType param) {
    return param == arg->removeReference();
  };
  auto isExactReferenceArg = [] (SType arg, SType param) {
    bool byConstRef = param.dynamicCast<ReferenceType>() &&
        !arg.dynamicCast<MutableReferenceType>() &&
        param->removeReference() == arg->removeReference();
    return byConstRef;
  };
  auto isConstToMutableReferenceArg = [] (SType arg, SType param) {
    bool byConstRef = param.dynamicCast<ReferenceType>() &&
        arg.dynamicCast<MutableReferenceType>() &&
        param->removeReference() == arg->removeReference();
    return byConstRef;
  };
  auto isSpecialized = [&] (const auto& args, const auto& overload) {
    for (auto& other : overloads)
      if (other != overload && context.isGeneralizationWithoutReturnType(
          overload->getParent()->getWithoutRequirements(), other->getParent()->getWithoutRequirements()))
        return false;
    return true;
  };
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isExactArg);}, "all args exact");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isExactArg);}, "first arg exact");
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isExactValueArg);}, "all args exact");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isExactValueArg);}, "first arg exact");
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isExactReferenceArg);}, "all args exact");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isExactReferenceArg);}, "first arg exact");
  filter([&](const auto& args, const auto& overload) { return exactArgs(args, overload->type, isConstToMutableReferenceArg);}, "all args exact");
  filter([&](const auto& args, const auto& overload) { return exactFirstArg(args, overload->type, isConstToMutableReferenceArg);}, "first arg exact");
  // filter out functions that have concept type parameters or return value
  filter([](const vector<SType>&, const SFunctionInfo& f){ return !f->isConceptTypeFunction(); }, "concept type");
  // sometimes a function is both in the global context and in the concept, so prefer the one in the concept
  filter(&fromConcept, "non concept");
  filter(&userDefinedConstructor, "user defined constructor");
  // try to choose a more specialized template, eg. f<T>(X<T>) instead of f<T>(T).
  filter(isSpecialized, "specialized");
}


static WithErrorLine<SFunctionInfo> handleOperatorOverloads(const Context& context, CodeLoc codeLoc, Operator op,
    vector<SType> types, vector<CodeLoc> argLocs, vector<unique_ptr<Expression>>& expr) {
  vector<SFunctionInfo> overloads;
  if (auto fun = context.getBuiltinOperator(op, types))
    overloads.push_back(fun.get());
  vector<string> errors;
  for (auto fun : context.getOperatorType(op))
    if (auto inst = instantiateFunction(context, fun, codeLoc, {}, types, argLocs))
      overloads.push_back(inst.get());
    else
      errors.push_back("Candidate: " + fun->prettyString() + ": " + inst.get_error().error);
  filterOverloads(context, overloads, types);
  if (overloads.size() == 1) {
    generateConversions(context, overloads[0]->type.params, types, expr);
    return overloads[0];
  } else {
      string error = (overloads.empty() ? "No overload" : "Multiple overloads") + " found for operator: "s +
          quote(getString(op)) + " with argument types: " + joinTypeList(types);
      for (auto& f : overloads)
        error += "\nCandidate: " + f->prettyString();
      for (auto& f : errors)
        error += "\n" + f;
      return codeLoc.getError(error);
  }
}

static WithErrorLine<SFunctionInfo> getFunction(const Context&,
    CodeLoc, IdentifierInfo, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>&);

static WithErrorLine<SFunctionInfo> getFunction(const Context&,
    CodeLoc, IdentifierInfo, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>&, vector<unique_ptr<Expression>>&);

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr) {
  return unique<BinaryExpression>(Private{}, loc, op, std::move(expr));
}

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, unique_ptr<Expression> a,
    unique_ptr<Expression> b) {
  return get(loc, op, makeVec<unique_ptr<Expression>>(std::move(a), std::move(b)));
}

BinaryExpression::BinaryExpression(BinaryExpression::Private, CodeLoc loc, Operator op,
    vector<unique_ptr<Expression>> expr) : Expression(loc), op(op), expr(std::move(expr)) {}

static vector<unique_ptr<Expression>> transformValueOrArg(vector<unique_ptr<Expression>> expr, CodeLoc codeLoc,
    bool usePointer) {
  auto block = unique<StatementBlock>(codeLoc);
  if (usePointer)
    expr[1] = unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr[1]));
  block->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(expr[1])));
  expr[1] = unique<LambdaExpression>(codeLoc, vector<FunctionParameter>(), std::move(block), none,
      LambdaCaptureInfo{{}, {}, LambdaCaptureType::REFERENCE });
  return expr;
}

static unique_ptr<Expression> getDestructAndGetCall(CodeLoc codeLoc, unique_ptr<Expression> expr) {
  return unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE,
      (unique<FunctionCall>(IdentifierInfo("destruct_and_get", codeLoc),
          unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false)));
}

static unique_ptr<Expression> getDestructorCall(CodeLoc codeLoc, unique_ptr<Expression> expr) {
  return unique<FunctionCall>(IdentifierInfo("destruct", codeLoc),
      unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false);
}

static unique_ptr<Statement> getDestructorStatement(CodeLoc codeLoc, const string& identifier) {
  return unique<ExpressionStatement>(getDestructorCall(codeLoc, unique<Variable>(IdentifierInfo(identifier, codeLoc))));
}

WithErrorLine<SType> BinaryExpression::getTypeImpl(const Context& context) {
  if (!functionInfo && op == Operator::VALUE_OR) {
    // We have to use a copy to check type of rhs, otherwise the implicit lambda might not be checked
    // again and implicit captures won't be extracted
    auto tmpExpr = expr[1]->deepCopy();
    auto type = TRY(getType(context, tmpExpr));
    expr = transformValueOrArg(std::move(expr), codeLoc,
        !!type.dynamicCast<ReferenceType>() || !!type.dynamicCast<MutableReferenceType>());
  }
  vector<SType> exprTypes;
  for (auto& elem : expr)
    exprTypes.push_back(TRY(getType(context, elem)));
  if (op == Operator::SUBSCRIPT && exprTypes[0].dynamicCast<VariablePack>() && !expr[1]->eval(context))
    return expr[1]->codeLoc.getError("Unable to evaluate variable pack index at compile-time");
  functionInfo = TRY(handleOperatorOverloads(context, codeLoc, op, exprTypes,
      ::transform(expr, [&](auto& e) { return e->codeLoc;}), expr));
  if (op == Operator::ASSIGNMENT && exprTypes[0]->removeReference()->hasDestructor()) {
    expr[0] = getDestructAndGetCall(codeLoc, std::move(expr[0]));
    CHECK(!!getType(context, expr[0]));
  }
  TRY(functionInfo->addInstance(context));
  TRY(considerDestructorCall(context, 0, exprTypes[0]));
  TRY(considerDestructorCall(context, 1, exprTypes[1]));
  return functionInfo->type.retVal;
}

WithErrorLine<SFunctionInfo> getDestructor(const Context& context, const SType& type, CodeLoc codeLoc) {
  return getFunction(context, codeLoc, IdentifierInfo("destruct"s, codeLoc), {},
      {PointerType::get(type)}, {codeLoc});
}

JustError<ErrorLoc> BinaryExpression::considerDestructorCall(const Context& context, int index, const SType& argType) {
  if (argType->hasDestructor() && !argType->isReference() && functionInfo->type.params[index].dynamicCast<ReferenceType>()) {
    destructorCall[index] = TRY(getDestructor(context, argType, codeLoc));
    TRY(destructorCall[index]->addInstance(context));
  }
  return success;
}

WithEvalError<EvalResult> BinaryExpression::eval(const Context& context) const {
  auto value1 = TRY(expr[0]->eval(context));
  auto value2 = TRY(expr[1]->eval(context));
  return EvalResult{TRY(::eval(op, {value1.value, value2.value})), value1.isConstant && value2.isConstant};
}

unique_ptr<Expression> BinaryExpression::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return get(codeLoc, op, fun(expr[0].get()), fun(expr[1].get()));
}

void BinaryExpression::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  for (auto& e : expr)
    f2(e.get());
}

void BinaryExpression::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  this->Expression::addFunctionCalls(fun);
  fun(functionInfo.get());
  for (int i = 0; i < 2; ++i)
    if (!!destructorCall[i])
      fun(destructorCall[i].get());
}

JustError<ErrorLoc> BinaryExpression::checkMoves(MoveChecker& checker) const {
  for (auto& e : expr)
    TRY(e->checkMoves(checker));
  return success;
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

WithErrorLine<SType> UnaryExpression::getTypeImpl(const Context& context) {
  nullable<SType> ret;
  auto right = TRY(getType(context, expr));
  ErrorLoc error { codeLoc, "Can't apply operator: " + quote(getString(op)) + " to type: " + quote(right->getName())};
  auto exprTmp = makeVec(std::move(expr));
  functionInfo = TRY(handleOperatorOverloads(context, codeLoc, op, {TRY(exprTmp[0]->getTypeImpl(context))}, {exprTmp[0]->codeLoc}, exprTmp));
  expr = std::move(exprTmp[0]);
  TRY(functionInfo->addInstance(context));
  if (right->hasDestructor() && !right->isReference() &&
      (functionInfo->type.params[0].dynamicCast<ReferenceType>() || op == Operator::GET_ADDRESS)) {
    destructorCall = TRY(getDestructor(context, right, codeLoc));
    TRY(destructorCall->addInstance(context));
  }
  return functionInfo->type.retVal;
}

WithEvalError<EvalResult> UnaryExpression::eval(const Context& context) const {
  auto value = TRY(expr->eval(context));
  return EvalResult{TRY(::eval(op, {value.value})), value.isConstant};
}

unique_ptr<Expression> UnaryExpression::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return unique<UnaryExpression>(codeLoc, op, fun(expr.get()));
}

void UnaryExpression::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(expr.get());
}

void UnaryExpression::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  this->Expression::addFunctionCalls(fun);
  fun(functionInfo.get());
  if (destructorCall)
    fun(destructorCall.get());
}

JustError<ErrorLoc> UnaryExpression::checkMoves(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

JustError<ErrorLoc> StatementBlock::check(Context& context, bool) {
  auto bodyContext = context.getChild();
  for (auto& s : elems)
    TRY(s->check(bodyContext));
  return success;
}

JustError<ErrorLoc> StatementBlock::checkMovesImpl(MoveChecker& checker) const {
  checker.startBlock();
  OnExit onExit([&]{ checker.endBlock();});
  for (auto& elem : elems)
    TRY(elem->checkMoves(checker));
  return success;
}

unique_ptr<Statement> StatementBlock::transform(const StmtTransformFun& fun, const ExprTransformFun&) const {
  auto ret = unique<StatementBlock>(codeLoc);
  for (auto& elem : elems)
    ret->elems.push_back(fun(elem.get()));
  return ret;
}

void StatementBlock::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  for (auto& elem : elems)
    f1(elem.get());
}

JustError<ErrorLoc> IfStatement::check(Context& context, bool) {
  auto ifContext = context.getChild();
  if (declaration)
    TRY(declaration->check(ifContext));
  auto negate = [&] (unique_ptr<Expression> expr) {
    auto codeLoc = expr->codeLoc;
    return unique<UnaryExpression>(codeLoc, Operator::LOGICAL_NOT, std::move(expr));
  };
  if (!condition)
    condition = negate(negate(unique<Variable>(IdentifierInfo(declaration->identifier, declaration->codeLoc))));
  auto condType = TRY(getType(ifContext, condition));
  if (!ifContext.canConvert(condType, BuiltinType::BOOL)) {
    condition = negate(negate(std::move(condition)));
    condType = TRY(getType(ifContext, condition));
  }
  if (!ifContext.canConvert(condType, BuiltinType::BOOL)) {
    return codeLoc.getError(
        "Expected a type convertible to bool or with overloaded operator " +
        quote("!") + " inside if statement, got " + quote(condType.get()->getName()));
  }
  auto trueContext = ifContext.getChild();
  trueContext.setIsInBranch();
  TRY(ifTrue->check(trueContext));
  if (ifFalse) {
    auto falseContext = ifContext.getChild();
    falseContext.setIsInBranch();
    TRY(ifFalse->check(falseContext));
  }
  return success;
}

JustError<ErrorLoc> IfStatement::checkMovesImpl(MoveChecker& checker) const {
  if (declaration)
    TRY(declaration->checkMoves(checker));
  if (condition)
    TRY(condition->checkMoves(checker));
  checker.startBlock();
  OnExit onExit([&]{ checker.endBlock();});
  checker.newAlternative();
  TRY(ifTrue->checkMoves(checker));
  if (ifFalse) {
    checker.newAlternative();
    TRY(ifFalse->checkMoves(checker));
  }
  return success;
}

unique_ptr<Statement> IfStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  return unique<IfStatement>(codeLoc,
      declaration ? cast<VariableDeclaration>(fun(declaration.get())) : nullptr,
      condition ? exprFun(condition.get()) : nullptr,
      fun(ifTrue.get()),
      ifFalse ? fun(ifFalse.get()) : nullptr);
}

void IfStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  if (declaration)
    f1(declaration.get());
  if (condition)
    f2(condition.get());
  f1(ifTrue.get());
  if (ifFalse)
    f1(ifFalse.get());
}

WithEvalError<StatementEvalResult> IfStatement::eval(Context& context) const {
  if (declaration)
    return EvalError::withError(getString(Keyword::STATIC) + " if with a declaration is not supported.");
  StatementEvalResult res;
  auto ifContext = context.getChild();
  auto value1 = TRY(condition->eval(ifContext)).value;
  if (value1->getType() != BuiltinType::BOOL)
    return EvalError::withError("Expected a compile-time value of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(value1->getName()));
  auto value = value1.dynamicCast<CompileTimeValue>();
  if (auto b = value->value.getValueMaybe<bool>()) {
    if (*b)
      res.push_back(ifTrue->deepCopy());
    else if (ifFalse)
      res.push_back(ifFalse->deepCopy());
  } else {
    res.push_back(ifTrue->deepCopy());
    if (ifFalse)
      res.push_back(ifFalse->deepCopy());
  }
  for (auto& elem : res)
    if (auto res = elem->check(ifContext); !res)
      return EvalError::withError(res.get_error().toString());
  return std::move(res);
}

static JustError<string> getVariableInitializationError(const char* action, const Context& context, const SType& varType,
    const SType& exprType, unique_ptr<Expression>& expr) {
  if (auto res = context.canConvert(exprType, varType, expr); !res)
    return "Can't "s + action + " of type "
       + quote(varType->getName()) + " using a value of type " + quote(exprType->getName()) + ".\n" + res.get_error();
  return success;
}

WithErrorLine<SType> VariableDeclaration::getRealType(const Context& context) const {
  if (type)
    return context.getTypeFromString(*type);
  else
  if (initExpr)
    return TRY(initExpr->getTypeImpl(context))->removeReference();
  else
    return codeLoc.getError("Initializing expression needed to infer variable type");
}

JustError<ErrorLoc> VariableDeclaration::check(Context& context, bool) {
  TRY(context.checkNameConflictExcludingFunctions(identifier, "Variable").addCodeLoc(codeLoc));
  if (!realType)
    realType = TRY(getRealType(context));
  if (!realType->canDeclareVariable())
    return codeLoc.getError("Can't declare variable of type " + quote(realType->getName()));
  TRY(realType.get()->getSizeError(context).addCodeLoc(codeLoc));
  if (!initExpr)
    return codeLoc.getError("Variable requires initialization");
  auto exprType = TRY(getType(context, initExpr));
  TRY(getVariableInitializationError("initialize variable", context, realType.get(), exprType, initExpr).addCodeLoc(initExpr->codeLoc));
  TRY(getType(context, initExpr));
  auto varType = isMutable ? SType(MutableReferenceType::get(realType.get())) : SType(ReferenceType::get(realType.get()));
  context.addVariable(identifier, std::move(varType), codeLoc);
  if (realType->hasDestructor()) {
    destructorCall = getDestructorStatement(codeLoc, identifier);
    TRY(destructorCall->check(context));
  }
  return success;
}

JustError<ErrorLoc> VariableDeclaration::checkMovesImpl(MoveChecker& checker) const {
  if (initExpr)
    TRY(initExpr->checkMoves(checker));
  checker.addVariable(identifier);
  return success;
}

WithEvalError<StatementEvalResult> VariableDeclaration::eval(Context& context) const {
  if (auto res = context.checkNameConflictExcludingFunctions(identifier, "Variable"); !res)
    return EvalError::withError(res.get_error());
  auto actualType = TRY(getRealType(context).toEvalError());
  auto result = TRY(TRY(initExpr->eval(context)).value->convertTo(actualType).addCodeLoc(codeLoc).toEvalError());
  if (isMutable)
    result = CompileTimeValue::getReference(result);
  context.addType(identifier, std::move(result));
  return StatementEvalResult{};
}

unique_ptr<Statement> VariableDeclaration::transform(const StmtTransformFun&,
    const ExprTransformFun& exprFun) const {
  auto ret = unique<VariableDeclaration>(codeLoc, none, identifier, initExpr ? exprFun(initExpr.get()) : nullptr);
  ret->isMutable = isMutable;
  ret->type = type;
  return ret;
}

void VariableDeclaration::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(initExpr.get());
  if (destructorCall)
    f1(destructorCall.get());
}

AliasDeclaration::AliasDeclaration(CodeLoc l, string id, unique_ptr<Expression> ini)
    : Statement(l), identifier(id), initExpr(std::move(ini)) {
}

JustError<ErrorLoc> AliasDeclaration::check(Context& context, bool) {
  TRY(context.checkNameConflictExcludingFunctions(identifier, "Variable").addCodeLoc(codeLoc));
  realType = TRY(getType(context, initExpr));
  context.addVariable(identifier, realType->isReference() ? realType.get() : ReferenceType::get(realType.get()), codeLoc);
  if (realType->hasDestructor()) {
    destructorCall = getDestructorStatement(codeLoc, identifier);
    TRY(destructorCall->check(context));
  }
  return success;
}

JustError<ErrorLoc> AliasDeclaration::checkMovesImpl(MoveChecker& checker) const {
  TRY(initExpr->checkMoves(checker));
  return success;
}

unique_ptr<Statement> AliasDeclaration::transform(const StmtTransformFun&,
    const ExprTransformFun& exprFun) const {
  auto ret = unique<AliasDeclaration>(codeLoc, identifier, exprFun(initExpr.get()));
  return ret;
}

void AliasDeclaration::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(initExpr.get());
  if (destructorCall)
    f1(destructorCall.get());
}

JustError<ErrorLoc> ReturnStatement::check(Context& context, bool) {
  auto returnType = TRY([&]() -> WithErrorLine<SType> {
    if (expr)
      return getType(context, expr);
    else
      return SType(BuiltinType::VOID);
  }());
  TRY(context.getReturnTypeChecker()->addReturnStatement(context, returnType, expr).addCodeLoc(codeLoc));
  return success;
}

JustError<ErrorLoc> ReturnStatement::checkMovesImpl(MoveChecker& checker) const {
  if (expr)
    TRY(expr->checkMoves(checker));
  checker.returnStatement();
  return success;
}

unique_ptr<Statement> ReturnStatement::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return unique<ReturnStatement>(codeLoc, expr ? fun(expr.get()) : nullptr);
}

void ReturnStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  if (expr)
    f2(expr.get());
}

JustError<ErrorLoc> Statement::addToContext(Context&) {
  return success;
}

JustError<ErrorLoc> Statement::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  return addToContext(context);
}

JustError<ErrorLoc> Statement::checkMoves(MoveChecker& checker) const {
  checker.clearStatementUsages();
  auto ret = checkMovesImpl(checker);
  checker.clearStatementUsages();
  return ret;
}

unique_ptr<Statement> Statement::replaceVar(string from, string to) const {
  return transform(
      [&](Statement* s) { return s->replaceVar(from, to); },
  [&](Expression* s) { return s->replaceVar(from, to); });
}

WithEvalError<StatementEvalResult> Statement::eval(Context& context) const {
  return EvalError::noEval();
}

JustError<ErrorLoc> Statement::checkMovesImpl(MoveChecker&) const {
  return success;
}

bool Statement::hasReturnStatement() const {
  return false;
}

unique_ptr<Statement> Statement::deepCopy() const {
  auto ret = transform(&identityStmt, &identityExpr);
  ret->exported = exported;
  return ret;
}

bool IfStatement::hasReturnStatement() const {
  return ifTrue->hasReturnStatement() && ifFalse && ifFalse->hasReturnStatement();
}

StatementBlock::StatementBlock(CodeLoc l, vector<unique_ptr<Statement>> e) : Statement(l), elems(std::move(e)) {
}

bool StatementBlock::hasReturnStatement() const {
  for (auto& s : elems)
    if (s->hasReturnStatement())
      return true;
  return false;
}

ReturnStatement::ReturnStatement(CodeLoc codeLoc)
    : Statement(codeLoc), expr(unique<Variable>(IdentifierInfo("void_value", codeLoc))) {
}

ReturnStatement::ReturnStatement(CodeLoc codeLoc, unique_ptr<Expression> expr)
    : Statement(codeLoc), expr(std::move(expr)) {}

bool ReturnStatement::hasReturnStatement() const {
  return true;
}

static WithErrorLine<vector<SType>> translateConceptParams(const Context& context,
    const TemplateInfo::ConceptRequirement& requirement) {
  auto& reqId = requirement.identifier;
  auto& requirementArgs = reqId.parts[0].templateArguments;
  auto concept = context.getConcept(reqId.parts[0].name).get();
  vector<SType> translatedParams;
  bool missingTypePack = requirement.variadic;
  for (int i = 0; i < requirementArgs.size(); ++i) {
    if (auto arg = requirementArgs[i].getReferenceMaybe<IdentifierInfo>()) {
      optional<bool> requireTypePack = reqId.parts[0].variadic && i == requirementArgs.size() - 1;
      // If requirement is variadic then we are ok with both type and type pack argument.
      // We check below that at least one type pack argument is present.
      if (requirement.variadic)
        requireTypePack = none;
      auto origParam = TRY(context.getTypeFromString(*arg, requireTypePack));
      if (auto p = context.getUnexpandedTypePack())
        if (p->second == origParam)
          missingTypePack = false;
      if (auto templateParam = origParam.dynamicCast<TemplateParameterType>()) {
        // Support is_enum concept
        auto& conceptParams = concept->getParams();
        if (conceptParams[min<int>(i, conceptParams.size() -1 )]->getType() != BuiltinType::ANY_TYPE)
          templateParam->type = conceptParams[min<int>(i, conceptParams.size() -1 )]->getType();
      }
      translatedParams.push_back(std::move(origParam));
    } else
      return reqId.codeLoc.getError("Expected a type argument");
  }
  if (missingTypePack)
    return requirement.identifier.codeLoc.getError("Variadic parameter requires at least one type pack argument");
  return translatedParams;
}

static WithErrorLine<TemplateRequirement> applyRequirement(Context& from,
    const TemplateInfo::ConceptRequirement& requirement, bool variadicRequiements) {
  auto& reqId = requirement.identifier;
  if (requirement.variadic && reqId.parts[0].variadic)
    return reqId.codeLoc.getError("Requirement can't be both variadic and involving a variadic concept");
  if (auto concept = from.getConcept(reqId.parts[0].name)) {
    if (!concept->isVariadic() && reqId.parts[0].variadic)
      return reqId.codeLoc.getError("Concept " + quote(concept->getName()) + " is not variadic");
    auto& requirementArgs = reqId.parts[0].templateArguments;
    if ((!concept->isVariadic() && requirementArgs.size() != concept->getParams().size()) ||
        (concept->isVariadic() && requirementArgs.size() < concept->getParams().size() - 1))
      return reqId.codeLoc.getError(
          "Wrong number of template arguments to concept " + quote(concept->getName()) + ": " + quote(reqId.parts[0].toString()));
    ErrorBuffer errors;
    auto translated = concept->translate(TRY(translateConceptParams(from, requirement)), variadicRequiements, errors);
    if (!errors.empty())
      return reqId.codeLoc.getError(errors[0]);
    from.merge(translated->getContext());
    return TemplateRequirement(std::move(translated), requirement.variadic);
  } else
    return reqId.codeLoc.getError("Unknown concept: " + reqId.parts[0].name);
}

static WithErrorLine<TemplateRequirement> applyRequirement(Context& from,
    const shared_ptr<Expression>& expr1, bool variadicRequirements) {
  auto expr = expr1->deepCopy();
  auto tmpExpr = expr->deepCopy();
  TRY(getType(from, tmpExpr));
  auto value = TRY(expr->eval(from).addNoEvalError(
      expr1->codeLoc.getError("Unable to evaluate expression at compile-time")));
  if (value.value->getType() != BuiltinType::BOOL)
    return expr->codeLoc.getError("Expected expression of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(value.value->getType()->getName()));
  return TemplateRequirement(shared_ptr<Expression>(std::move(expr)), false);
}

NODISCARD static WithErrorLine<vector<TemplateRequirement>> applyRequirements(Context& from,
    const TemplateInfo& requirements) {
  vector<TemplateRequirement> ret;
  for (auto& req : requirements.requirements)
    ret.push_back(TRY(req.visit([&](auto& req) {
      return applyRequirement(from, req, requirements.variadic);
    })));
  return ret;
}

static bool paramsAreGoodForOperator(const vector<SType>& params) {
  for (auto& p : params)
    if (p->removeReference()->getType() == BuiltinType::STRUCT_TYPE ||
        p->removeReference()->getType() == BuiltinType::UNION_TYPE)
      return true;
  return false;
}

WithErrorLine<SType> FunctionDefinition::getReturnType(const Context& context) const {
  if (returnType.asBasicIdentifier() == "noreturn"s)
    return (SType) BuiltinType::NORETURN;
  else
    return context.getTypeFromString(this->returnType);
}

static WithErrorLine<vector<SType>> getTemplateParams(const TemplateInfo& info, const Context& context) {
  vector<SType> ret;
  auto paramsContext = context.getChild();
  for (auto& param : info.params) {
    if (param.type) {
      if (auto type = paramsContext.getType(*param.type)) {
        if (!type->canBeValueTemplateParam())
          return param.codeLoc.getError("Value template parameter cannot have type " + quote(*param.type));
        ret.push_back(CompileTimeValue::getTemplateValue(type.get(), param.name));
      } else
        return param.codeLoc.getError("Type not found: " + quote(*param.type));
    } else {
      TRY(paramsContext.checkNameConflict(param.name, "template parameter").addCodeLoc(param.codeLoc));
      ret.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
      paramsContext.addType(param.name, ret.back());
    }
  }
  return ret;
}

unique_ptr<Statement> FunctionDefinition::transform(const StmtTransformFun& f1, const ExprTransformFun&) const {
  auto ret = unique<FunctionDefinition>(codeLoc, returnType, name);
  ret->parameters = parameters;
  if (body)
    ret->body = cast<StatementBlock>(f1(body.get()));
  ret->templateInfo = templateInfo;
  ret->external = external;
  ret->isVirtual = isVirtual;
  ret->isDefault = isDefault;
  ret->isVariadicParams = isVariadicParams;
  return ret;
}

JustError<ErrorLoc> FunctionDefinition::setFunctionSignature(const Context& context, nullable<SConcept> concept,
    bool builtInImport) {
  if (functionInfo)
    return success;
  for (int i = 0; i < parameters.size(); ++i)
    if (!parameters[i].name)
      parameters[i].name = "parameter" + to_string(i);
  if (auto s = name.getReferenceMaybe<string>())
    TRY(context.checkNameConflictExcludingFunctions(*s, "Function").addCodeLoc(codeLoc));
  else
  if (auto op = name.getValueMaybe<Operator>()) {
    if (!canOverload(*op, int(parameters.size())))
      return codeLoc.getError("Can't overload operator " + quote(getString(*op)) +
          " with " + to_string(parameters.size()) + " arguments.");
  }
  Context contextWithTemplateParams = context.getChild();
  auto templateTypes = TRY(getTemplateParams(templateInfo, context));
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    auto& param = templateInfo.params[i];
    contextWithTemplateParams.addType(param.name, templateTypes[i]);
    if (i == templateInfo.params.size() - 1 && templateInfo.variadic)
      contextWithTemplateParams.addUnexpandedTypePack(param.name, templateTypes[i]);
  }
  auto requirements = TRY(applyRequirements(contextWithTemplateParams, templateInfo));
  definitionContext.emplace(contextWithTemplateParams.getChild());
  auto returnType = TRY(getReturnType(contextWithTemplateParams));
  if (name.contains<Operator>())
    returnType = convertPointerToReference(returnType);
  vector<SType> params;
  set<string> paramNames;
  for (int i = 0; i < parameters.size(); ++i) {
    auto& p = parameters[i];
    auto type = TRY(contextWithTemplateParams.getTypeFromString(p.type, isVariadicParams && i == parameters.size() - 1));
    if (type == BuiltinType::VOID)
      return p.codeLoc.getError("Function parameter may not have " + quote(type->getName()) + " type");
    if (name.contains<Operator>())
      type = convertPointerToReference(type);
    params.push_back(std::move(type));
    if (p.name) {
      if (paramNames.count(*p.name))
        return p.codeLoc.getError("Duplicate function parameter name: " + quote(*p.name));
      paramNames.insert(*p.name);
    }
  }
  if (!builtInImport && !concept && name.contains<Operator>() && !paramsAreGoodForOperator(params))
    return codeLoc.getError("Operator parameters must include at least one user-defined type");
  FunctionSignature functionType(returnType, params, templateTypes);
  functionType.concept = concept;
  functionType.requirements = requirements;
  functionType.variadicTemplate = templateInfo.variadic;
  functionType.variadicParams = isVariadicParams;
  if (name.contains<ConstructorTag>() && external)
    functionType.generatedConstructor = true;
  if (external)
    functionType.setBuiltin();
  CHECK(!functionInfo);
  functionInfo = FunctionInfo::getDefined(name, std::move(functionType), this);
  if (functionInfo->isMainFunction()) {
    auto expectedParam = SliceType::get(BuiltinType::STRING);
    if (!functionInfo->type.params.empty() && (functionInfo->type.params.size() > 1
        || functionInfo->type.params[0] != expectedParam))
      return codeLoc.getError("The main() function should take no arguments or take a single argument of type "
          + quote(expectedParam->getName()));
    if (functionInfo->type.retVal != BuiltinType::INT)
      return codeLoc.getError("The main() function should return a value of type " + quote(BuiltinType::INT->getName()));
  }
  return success;
}

static FunctionId translateDestructorId(const FunctionId& id) {
  if (id == "destruct"s)
    return "destruct_full"s;
  if (id == "destruct_impl_dont_call"s)
    return "destruct"s;
  return id;
}

static IdentifierInfo translateDestructorId(IdentifierInfo id) {
  if (id.parts[0].name == "destruct")
    id.parts[0].name = "destruct_full";
  if (id.parts[0].name == "destruct_impl_dont_call")
    id.parts[0].name = "destruct";
  return id;
}


static WithError<SFunctionInfo> getFunction(const Context& context,
    CodeLoc codeLoc, FunctionId id, vector<SFunctionInfo> candidates,
    vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc) {
  vector<SFunctionInfo> overloads;
  string errors;
  for (auto& overload : candidates) {
    if (auto f = instantiateFunction(context, overload, codeLoc, templateArgs, argTypes, argLoc)) {
      overloads.push_back(f.get());
    } else
      errors += "\nCandidate: "s + overload->prettyString() + ": " + f.get_error().error;
  }
  if (id == "destruct"s) {
    if (!argTypes.empty() && argTypes[0]->removeReference().dynamicCast<PointerType>()) {
      if (auto s = argTypes[0]->removePointer().dynamicCast<ConceptType>())
        return s->getConceptFor(argTypes[0]->removePointer())->getContext().getFunctions("destruct"s)[0];
      if (auto s = argTypes[0]->removePointer().dynamicCast<StructType>())
        if (s->destructor) {
          CHECK(templateArgs.empty());
          CHECK(s->destructor->type.params[0] == PointerType::get(s));
          return TRY(instantiateFunction(context, s->destructor->getParent(), codeLoc, {}, s->destructor->type.params,
              {codeLoc}).withoutCodeLoc());
        }
      if (auto s = argTypes[0]->removePointer().dynamicCast<LambdaType>())
        if (s->destructor)
          return FunctionInfo::getImplicit("destruct"s, FunctionSignature(BuiltinType::VOID, {PointerType::get(s)}, {}));
    }
  } else
  if (id == "invoke"s && !argTypes.empty() && argTypes[0]->isPointer())
    if (auto lambda = argTypes[0]->removePointer().dynamicCast<LambdaType>()) {
      if (auto f = instantiateFunction(context, lambda->functionInfo.get(), codeLoc, templateArgs, argTypes, argLoc)) {
        if (!contains(overloads, *f))
          overloads.push_back(*f);
      } else
        errors += "\nCandidate: "s + lambda->functionInfo->prettyString() + ": " + f.get_error().error;
    }
  if (overloads.empty())
    return errors;
  filterOverloads(context, overloads, argTypes);
  CHECK(!overloads.empty());
  if (overloads.size() == 1)
    return overloads[0];
  else
    return "Multiple function overloads found:\n" +
        combine(transform(overloads, [](const auto& o) { return o->prettyString();}), "\n");
}

static WithErrorLine<SFunctionInfo> getFunction(const Context& context,
    CodeLoc codeLoc, IdentifierInfo id, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc) {
  auto candidates = TRY(context.getFunctionTemplate(translateDestructorId(id)).addCodeLoc(codeLoc));
  auto error =  codeLoc.getError("Couldn't find function " + id.prettyString() +
      " matching arguments: (" + joinTypeList(argTypes) + ")");
  if (candidates.empty())
    return error;
  auto functionId = candidates[0]->id;
  if (auto res = getFunction(context, codeLoc, functionId, std::move(candidates), std::move(templateArgs),
      argTypes, argLoc))
    return *res;
  else {
    error.error += res.get_error();
    return error;
  }
}

static WithErrorLine<SFunctionInfo> getFunction(const Context& context,
    CodeLoc codeLoc, IdentifierInfo id, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc, vector<unique_ptr<Expression>>& expr) {
  auto fun = TRY(getFunction(context, codeLoc, std::move(id), std::move(templateArgs), std::move(argTypes), std::move(argLoc)));
  generateConversions(context, fun->type.params, argTypes, expr);
  return fun;
}

WithErrorLine<SFunctionInfo> getCopyFunction(const Context& context, CodeLoc callLoc, const SType& t) {
  return getFunction(context, callLoc, IdentifierInfo("copy", callLoc), {}, {PointerType::get(t)}, {callLoc});
}

WithErrorLine<SFunctionInfo> getImplicitCopyFunction(const Context& context, CodeLoc callLoc, const SType& t) {
  return getFunction(context, callLoc, IdentifierInfo("implicit_copy", callLoc), {}, {PointerType::get(t)}, {callLoc});
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualFunctionCallExpr(const Context& context,
    const string& funName, const string& alternativeName, const SType& alternativeType, int virtualIndex,
    bool lvalueParam) {
  auto functionCall = unique<FunctionCall>(IdentifierInfo(funName, codeLoc), false);
  vector<SType> args;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      args.push_back(functionInfo->type.params[i]);
    } else {
      if (lvalueParam) {
        functionCall->arguments.push_back(unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
            unique<Variable>(IdentifierInfo(alternativeName, codeLoc))));
        args.push_back(convertReferenceToPointer(alternativeType));
      } else {
        functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
        args.push_back(alternativeType);
      }
    }
  TRY(getFunction(context, codeLoc, IdentifierInfo(funName, codeLoc), {}, args,
      vector<CodeLoc>(args.size(), codeLoc)));
  return unique_ptr<Expression>(std::move(functionCall));
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualOperatorCallExpr(Context& context,
    Operator op, const string& alternativeName, const SType& alternativeType, int virtualIndex, int lvalueParam) {
  vector<unique_ptr<Expression>> arguments;
  vector<SType> argTypes;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      argTypes.push_back(functionInfo->type.params[i]);
    } else {
      if (lvalueParam) {
        arguments.push_back(unique<Variable>(IdentifierInfo(alternativeName, codeLoc)));
      } else
        arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
      argTypes.push_back(alternativeType);
    }
  TRY(handleOperatorOverloads(context, codeLoc, op, argTypes, vector<CodeLoc>(argTypes.size(), codeLoc), arguments));
  if (parameters.size() == 1)
    return unique_ptr<Expression>(unique<UnaryExpression>(codeLoc, op, std::move(arguments[0])));
  else {
    CHECK(parameters.size() == 2);
    return unique_ptr<Expression>(BinaryExpression::get(codeLoc, op, std::move(arguments)));
  }
}

JustError<ErrorLoc> FunctionDefinition::generateVirtualDispatchBody(Context& bodyContext) {
  unique_ptr<StatementBlock> defaultBlock;
  if (body)
    defaultBlock = std::move(body);
  int virtualIndex = [&]() {
    for (int i = 0; i < parameters.size(); ++i)
      if (parameters[i].isVirtual)
        return i;
    fail();
  }();
  for (int i = 0; i < parameters.size(); ++i) {
    parameters[i].isMutable = true;
    if (i != virtualIndex) {
      // we have to change the parameter names, because they could clash with union member names
      auto newName = "v_param" + *parameters[i].name;
      if (defaultBlock)
        defaultBlock = cast<StatementBlock>(defaultBlock->replaceVar(*parameters[i].name, newName));
      parameters[i].name = newName;
    }
  }
  auto& virtualParam = parameters[virtualIndex];
  auto virtualType = bodyContext.getTypeFromString(virtualParam.type);
  unique_ptr<Expression> switchExpr;
  body = unique<StatementBlock>(codeLoc);
  auto unionType = virtualType->dynamicCast<StructType>();
  bool lvalueParam = false;
  if (!unionType) {
    unionType = virtualType.get()->removePointer().dynamicCast<StructType>();
    lvalueParam = true;
    switchExpr = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE,
        unique<Variable>(IdentifierInfo(*virtualParam.name, codeLoc)));
  } else
    switchExpr = unique<MoveExpression>(codeLoc, *virtualParam.name);
  if (!unionType || unionType->alternatives.empty())
    return codeLoc.getError("Virtual parameter must be of a union type or a pointer to one");
  auto switchStatementPtr = unique<SwitchStatement>(codeLoc, std::move(switchExpr));
  auto& switchStatement = *switchStatementPtr;
  body->elems.push_back(std::move(switchStatementPtr));
  for (auto& alternative : unionType->alternatives) {
    auto alternativeType = alternative.type;
    if (virtualType->dynamicCast<MutablePointerType>())
      alternativeType = MutableReferenceType::get(std::move(alternativeType));
    else if (virtualType->dynamicCast<PointerType>())
      alternativeType = ReferenceType::get(std::move(alternativeType));
    WithErrorLine<unique_ptr<Expression>> call = [&] {
      if (auto regularName = name.getReferenceMaybe<string>())
        return getVirtualFunctionCallExpr(bodyContext, *regularName, alternative.name, alternativeType, virtualIndex,
            lvalueParam);
      else if (auto op = name.getValueMaybe<Operator>())
        return getVirtualOperatorCallExpr(bodyContext, *op, alternative.name, alternativeType, virtualIndex, lvalueParam);
      else
        fail();
    }();
    if (!call) {
      if (defaultBlock)
        switchStatement.defaultBlock = std::move(defaultBlock);
      else if (!switchStatement.defaultBlock)
        return call.get_error();
      continue;
    }
    auto block = unique<StatementBlock>(codeLoc);
    block->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(*call)));
    switchStatement.caseElems.push_back(
        SwitchStatement::CaseElem {
          codeLoc,
          {alternative.name},
          alternative.name,
          std::move(block)
        });
  }
  return success;
}

JustError<ErrorLoc> FunctionDefinition::checkAndGenerateCopyFunction(const Context& context, const string& functionName) {
  if (!body && isDefault) {
    if (parameters.size() != 1)
      return codeLoc.getError("Expected exactly one parameter in copy function");
    auto type = TRY(context.getTypeFromString(parameters[0].type));
    if (type != PointerType::get(context.getTypeFromString(returnType).get()))
      return codeLoc.getError("Copy function parameter type must be the same as pointer to return type");
    auto structType = type->removePointer().dynamicCast<StructType>();
    if (!structType)
      return codeLoc.getError("Can only generate copy function for user-defined types");
    body = unique<StatementBlock>(codeLoc);
    if (structType->alternatives.empty()) {
      auto call = unique<FunctionCall>(returnType, false);
      for (auto elem : structType->members) {
        auto copiedParam = unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc));
        auto copyCall = unique<FunctionCall>(IdentifierInfo(functionName, codeLoc), false);
        copyCall->arguments.push_back(unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
            MemberAccessExpression::getPointerAccess(codeLoc, std::move(copiedParam), elem.name)));
        call->arguments.push_back(std::move(copyCall));
      }
      body->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(call)));
    } else {
      auto copiedParam = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE,
          unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc)));
      auto topSwitch = unique<SwitchStatement>(codeLoc, std::move(copiedParam));
      for (auto& alternative : structType->alternatives) {
        auto block = unique<StatementBlock>(codeLoc);
        auto constructorName = returnType;
        constructorName.parts.push_back(IdentifierInfo::IdentifierPart { alternative.name, {} });
        auto constructorCall = unique<FunctionCall>(constructorName, false);
        if (alternative.type != BuiltinType::VOID)
          constructorCall->arguments.push_back(unique<FunctionCall>(IdentifierInfo(functionName, codeLoc),
              unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
                  unique<Variable>(IdentifierInfo(alternative.name, codeLoc))), false));
        block->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(constructorCall)));
        topSwitch->caseElems.push_back(
            SwitchStatement::CaseElem {
              codeLoc,
              {alternative.name},
              alternative.name,
              std::move(block)
            }
        );
      }
      body->elems.push_back(std::move(topSwitch));
    }
  }
  return success;
}

JustError<ErrorLoc> FunctionDefinition::checkAndGenerateDefaultConstructor(const Context& context) {
  if (!body && isDefault) {
    auto type = TRY(context.getTypeFromString(returnType));
    auto structType = type.dynamicCast<StructType>();
    if (!structType || !structType->alternatives.empty())
      return codeLoc.getError("Cannot generate default constructor for non-struct types");
    if (parameters.size() != structType->members.size())
      return codeLoc.getError("Expected exactly as many parameters as members in type " + quote(structType->getName()));
    body = unique<StatementBlock>(codeLoc);
    IdentifierInfo id = returnType;
    id.parts.push_back(returnType.parts[0]);
    id.parts.back().templateArguments.clear();
    auto call = unique<FunctionCall>(std::move(id), false);
    for (int i = 0; i < structType->members.size(); ++i) {
      call->arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
    }
    body->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(call)));
  }
  return success;
}

static vector<unique_ptr<Statement>> getDestructorCalls(CodeLoc codeLoc, const vector<string>& ids, const vector<SType>& types) {
  vector<unique_ptr<Statement>> ret;
  CHECK(ids.size() == types.size());
  for (int i = 0; i < ids.size(); ++i) {
    if (types[i]->hasDestructor())
      ret.push_back(getDestructorStatement(codeLoc, ids[i]));
    else
      ret.push_back(nullptr);
  }
  return ret;
}

JustError<ErrorLoc> FunctionDefinition::addInstance(const Context& callContext, const SFunctionInfo& instance) {
  if (callContext.getTemplateParams())
    return success;
  wasUsed = true;
  auto callTopContext = callContext.getTopLevel();
  if (instance != functionInfo.get()) {
    if (body) {
      CHECK(functionInfo == instance->getParent());
      for (auto& other : instances)
        if (other.functionInfo->getWithoutRequirements() == instance->getWithoutRequirements())
          return success;
      int instanceIndex = instances.size();
      instances.push_back(InstanceInfo{unique_ptr<StatementBlock>(), {},
          instance, callTopContext.getChild()});
      if (wasChecked) {
        instances[instanceIndex].body = cast<StatementBlock>(origBody->deepCopy());
        vector<unique_ptr<Statement>> destructorCallsTmp;
        auto ret = checkBody(callTopContext, *instances[instanceIndex].body,
            *instances[instanceIndex].functionInfo, destructorCallsTmp);
        instances[instanceIndex].destructorCalls = std::move(destructorCallsTmp);
        return ret;
      }
    }
  } else
    CHECK(instance->type.templateParams.empty());
  return success;
}

static void addTemplateParams(Context& context, vector<SType> params, bool variadic) {
  for (int i = 0; i < params.size(); ++i) {
    auto& param = params[i];
    if (auto valueType = param.dynamicCast<CompileTimeValue>()) {
      auto templateValue = valueType->value.getReferenceMaybe<CompileTimeValue::TemplateValue>();
      context.addType(templateValue->name,
          CompileTimeValue::getTemplateValue(templateValue->type, templateValue->name));
    } else {
      context.addType(param->getName(), param);
    }
    if (variadic && i == params.size() - 1)
      context.addUnexpandedTypePack(param->getName(), param);
  }
}

JustError<ErrorLoc> FunctionDefinition::generateDefaultBodies(Context& context) {
  auto getContext = [&] () -> WithErrorLine<Context> {
    Context bodyContext = context.getChild();
    addTemplateParams(bodyContext, functionInfo->type.templateParams, functionInfo->type.variadicTemplate);
    auto res = TRY(applyRequirements(bodyContext, templateInfo));
    return std::move(bodyContext);
  };
  if (isVirtual) {
    auto bodyContext = TRY(getContext());
    TRY(generateVirtualDispatchBody(bodyContext));
  } else
  if (name == "copy"s || name == "implicit_copy"s) {
    auto bodyContext = TRY(getContext());
    TRY(checkAndGenerateCopyFunction(bodyContext, name.get<string>()));
  } else
  if (name == ConstructorTag{}) {
    auto bodyContext = TRY(getContext());
    TRY(checkAndGenerateDefaultConstructor(bodyContext));
  } else
  if (isDefault)
    return codeLoc.getError("Cannot generate a default body for this function");
  return success;
}

void FunctionDefinition::addParamsToContext(Context& context, const FunctionInfo& instanceInfo) const {
  vector<SType> templateParams;
//  std::cout << "Checking " << instanceInfo.prettyString() << std::endl;
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    if (i >= instanceInfo.type.templateParams.size()) {
      context.addExpandedTypePack(templateInfo.params[i].name, {});
//      std::cout << "Adding empty expanded type pack " << templateInfo.params[i].name << std::endl;
    } else {
      auto& t = instanceInfo.type.templateParams[i];
      if (!t->getMangledName())
        templateParams.push_back(t);
      else if (i < templateInfo.params.size() - 1 || !templateInfo.variadic) {
//        std::cout << "Adding " << templateInfo.params[i].name << " = " << t->getName() << std::endl;
        context.addType(templateInfo.params[i].name, t);
        context.addSubstitution(Context::SubstitutionInfo{
            templateInfo.params[i].name, t->getCodegenName(), !!t.dynamicCast<CompileTimeValue>()});
      } else {
        auto types = instanceInfo.type.templateParams.getSubsequence(i);
//        std::cout << "Adding expanded type pack " << templateInfo.params[i].name << " " << joinTypeList(types) << std::endl;
        context.addExpandedTypePack(templateInfo.params[i].name, types);
      }
    }
  }
  if (!templateParams.empty())
    context.setTemplated(templateParams);
  else if (!templateInfo.params.empty())
    context.setTemplateInstance();
  for (int i = 0; i < parameters.size(); ++i) {
    if (i >= instanceInfo.type.params.size()) {
      if (parameters[i].name) {
        context.addExpandedVariablePack(*parameters[i].name, {});
//        std::cout << "Adding empty expanded type pack " << *parameters[i].name << std::endl;
      }
    } else {
      auto& param = parameters[i];
      auto getParamType = [&] (SType type) {
        if (name.contains<Operator>())
          type = convertReferenceToPointer(type);
        return param.isMutable ? SType(MutableReferenceType::get(type)) : SType(ReferenceType::get(type));
      };
      if (auto name = parameters[i].name) {
        if (!isVariadicParams || i < parameters.size() - 1) {
          auto type = getParamType(instanceInfo.type.params[i]);
          context.addVariable(*name, type, param.codeLoc);
//          std::cout << "Adding param " << *name << " of type " << type->getName() << std::endl;
        } else {
          auto types = instanceInfo.type.params.getSubsequence(parameters.size() - 1)
              .transform([&](auto& type) { return getParamType(type); });
          if (instanceInfo.type.variadicParams) {
            context.addUnexpandedVariablePack(*name, types.back());
            //std::cout << "Adding unexpanded variable pack " << *name << " of type " << types.back()->getName() << std::endl;
          } else {
            context.addExpandedVariablePack(*name, types);
            //std::cout << "Adding expanded variable pack " << *name << " of type " << joinTypeList(types) << std::endl;
          }
        }
      }
    }
  }
}

static void considerAddingVoidReturn(const Context& context, StatementBlock* block, const SType& retVal) {
  if (context.canConvert(BuiltinType::VOID, retVal)
      && (block->elems.empty() || !block->elems.back()->hasReturnStatement())) {
    block->elems.push_back(unique<ReturnStatement>(block->codeLoc));
    auto c = context.getChild();
    CHECK(!!block->elems.back()->check(c));
  }
}

JustError<ErrorLoc> FunctionDefinition::checkBody(const Context& callContext,
    StatementBlock& myBody, const FunctionInfo& instanceInfo, vector<unique_ptr<Statement>>& destructorCalls) const {
  auto bodyContext = callContext.getChild();
  bodyContext.merge(*definitionContext);
  addParamsToContext(bodyContext, instanceInfo);
  vector<SType> templateParams;
//  std::cout << "Checking " << instanceInfo.prettyString() << std::endl;
  for (auto& t : instanceInfo.type.templateParams)
    if (!t->getMangledName())
      templateParams.push_back(t);
  auto retVal = instanceInfo.type.retVal;
  if (name.contains<Operator>())
    retVal = convertReferenceToPointer(retVal);
  ReturnTypeChecker returnChecker(retVal);
  bodyContext.addReturnTypeChecker(&returnChecker);
  TRY(myBody.check(bodyContext));
  considerAddingVoidReturn(bodyContext, &myBody, retVal);
  if (templateParams.empty()) {
    vector<string> paramNames;
    for (int i = 0; i < instanceInfo.type.params.size(); ++i)
      paramNames.push_back(*instanceInfo.getParamName(i, this));
    destructorCalls = getDestructorCalls(codeLoc, std::move(paramNames), instanceInfo.type.params);
    for (auto& elem : destructorCalls)
      if (elem)
        TRY(elem->check(bodyContext));
  }
  if (retVal == BuiltinType::NORETURN && !myBody.hasReturnStatement())
    return codeLoc.getError("This function should never return");
  if (retVal != BuiltinType::VOID && !myBody.hasReturnStatement())
    return codeLoc.getError("Not all paths lead to a return statement in a function returning non-void");
  return success;
}

JustError<ErrorLoc> FunctionDefinition::checkForIncompleteTypes(const Context& context) {
  for (int i = 0; i < functionInfo->type.params.size(); ++i) {
    auto paramType = functionInfo->type.params[i];
    TRY(paramType->getSizeError(context).addCodeLoc(parameters[i].codeLoc));
  }
  TRY(functionInfo->type.retVal->getSizeError(context).addCodeLoc(returnType.codeLoc));
  return success;
}

static WithError<SType> getDestructedType(const vector<SType>& params) {
  if (params.size() != 1)
    return "Destructor function should take exactly one argument"s;
  auto& param = params[0];
  auto ret = param->removePointer();
  if (ret == param)
    return "Destructor function parameter should be a pointer to the destructed type"s;
  return ret;
}

JustError<ErrorLoc> FunctionDefinition::check(Context& context, bool notInImport) {
  TRY(generateDefaultBodies(context));
  // if origBody is present then body has already been checked
  // this can happen in exported functions, for example
  // it probably isn't necessary to recheck the body in these cases
  if (body && !origBody && (!templateInfo.params.empty() || notInImport)) {
    // save the original unchecked body to use by template instances
    origBody = cast<StatementBlock>(body->deepCopy());
    Context paramsContext = context.getChild();
    // use a temporary FunctionInfo with fresh template types to avoid clashes if the function
    // is calling itself with changed parameter order
    auto thisFunctionInfo = functionInfo.get();
    auto newParams = TRY(getTemplateParams(templateInfo, context));
    addTemplateParams(paramsContext, newParams, thisFunctionInfo->type.variadicTemplate);
    TRY(applyRequirements(paramsContext, templateInfo));
    ErrorBuffer errors;
    for (int i = 0; i < newParams.size(); ++i) {
      thisFunctionInfo = replaceInFunction(paramsContext, thisFunctionInfo,
        functionInfo->type.templateParams[i], newParams[i], errors);
    }
    if (!errors.empty())
      return codeLoc.getError(errors[0]);
    TRY(checkForIncompleteTypes(paramsContext));
    // For checking the template we use the Context that includes the template params.
    definitionContext.emplace(paramsContext.getChild());
    TRY(checkBody(*definitionContext, *body, *thisFunctionInfo, destructorCalls));
    // For checking instances we just use the top level context.
    definitionContext.emplace(context.getChild());
    wasChecked = true;
    for (int i = 0; i < instances.size(); ++i)
      if (!instances[i].body) {
        instances[i].body = cast<StatementBlock>(origBody->deepCopy());
        TRY(checkBody(instances[i].callContext, *instances[i].body,
            *instances[i].functionInfo, instances[i].destructorCalls));
      }
    MoveChecker moveChecker;
    for (auto& p : parameters)
      if (p.name)
        moveChecker.addVariable(*p.name);
    if (name == "destruct"s) {
      auto destructedType = TRY(getDestructedType(thisFunctionInfo->type.params).addCodeLoc(codeLoc));
      if (auto structType = destructedType.dynamicCast<StructType>())
        if (!structType->definition || structType->definition->file != codeLoc.file)
          return codeLoc.getError("Destructor function must be defined in the same file as the destructed type");
    }
    return body->checkMoves(moveChecker);
  }
  return success;
}

JustError<ErrorLoc> FunctionDefinition::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  TRY(setFunctionSignature(context, nullptr, cache.isCurrentlyBuiltIn()));
  TRY(context.addFunction(functionInfo.get()).addCodeLoc(codeLoc));
  if (name == "destruct"s) {
    auto destructedType = TRY(getDestructedType(functionInfo->type.params).addCodeLoc(codeLoc));
    if (auto structType = destructedType.dynamicCast<StructType>()) {
      if (!structType->alternatives.empty())
        return codeLoc.getError("User-defined destructors for union types are not supported");
      auto adjusted = functionInfo.get();
      if (functionInfo->type.templateParams.size() != structType->templateParams.size())
        return codeLoc.getError("Number of template parameters of destructor function must match destructed type");
      for (auto& param : functionInfo->type.templateParams)
        if (!structType->templateParams.contains(param))
          return codeLoc.getError("Template parameters of destructor function must match destructed type");
      ErrorBuffer errors;
      for (int i = 0; i < structType->parent->templateParams.size(); ++i)
        adjusted = replaceInFunction(context, adjusted, functionInfo->type.templateParams[i],
            structType->parent->templateParams[i], errors);
      if (!errors.empty())
        return codeLoc.getError("Error in the destructor definition for type " + quote(structType->getName()) + ":\n"
            + errors[0]);
      if (structType->parent->destructor && structType->parent->destructor != adjusted)
        return codeLoc.getError("Destructor function for type " + quote(destructedType->getName())
            + " already defined here: " + structType->destructor->getDefinition()->codeLoc.toString());
      structType->parent->destructor = adjusted;
    } else
    if (!cache.isCurrentlyBuiltIn())
      return codeLoc.getError("User-defined destructor is allowed only for struct types");
  }
  return success;
}

void FunctionDefinition::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  if (body && templateInfo.params.empty())
    f1(body.get());
  for (auto& call : destructorCalls)
    if (call)
      f1(call.get());
}

static void addBuiltInConcepts(Context& context) {
  auto addType = [&context](const char* name, SType type) {
    shared_ptr<Concept> concept = shared<Concept>(name, nullptr, Context(context.typeRegistry, true), false);
    concept->modParams().push_back(shared<TemplateParameterType>(type, "T", CodeLoc()));
    context.addConcept(name, concept);
  };
  addType("is_enum", BuiltinType::ENUM_TYPE);
  addType("is_struct", BuiltinType::STRUCT_TYPE);
  addType("is_union", BuiltinType::UNION_TYPE);
}

Context createPrimaryContext(TypeRegistry* typeRegistry) {
  Context context(typeRegistry, true);
  context.addVariable("void_value", BuiltinType::VOID, CodeLoc(), true);
  for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE, BuiltinType::BOOL,
       BuiltinType::VOID, BuiltinType::CHAR, BuiltinType::STRING, BuiltinType::NULL_TYPE})
    context.addType(type->getName(), type);
  CHECK(context.addImplicitFunction(Operator::PLUS, FunctionSignature(BuiltinType::STRING,
      {{BuiltinType::STRING}, {BuiltinType::STRING}}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS_UNARY, Operator::MINUS_UNARY})
    for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionSignature(type, {{type}}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT, Operator::DECREMENT})
    CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::VOID,
        {{MutableReferenceType::get(BuiltinType::INT)}}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS, Operator::MINUS, Operator::MULTIPLY, Operator::DIVIDE, Operator::MODULO})
    for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE})
      if (type != BuiltinType::DOUBLE || op != Operator::MODULO)
        CHECK(context.addImplicitFunction(op, FunctionSignature(type, {{type}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT_BY, Operator::DECREMENT_BY, Operator::MULTIPLY_BY, Operator::DIVIDE_BY})
    for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::VOID,
          {{MutableReferenceType::get(type)}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::LOGICAL_AND, Operator::LOGICAL_OR})
    CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::BOOL,
        {{BuiltinType::BOOL}, {BuiltinType::BOOL}}, {}).setBuiltin()));
  CHECK(context.addImplicitFunction(Operator::LOGICAL_NOT, FunctionSignature(BuiltinType::BOOL,
      {{BuiltinType::BOOL}}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS, Operator::NOT_EQUAL, Operator::LESS_THAN})
    for (auto type : {BuiltinType::INT, BuiltinType::STRING, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::BOOL,
          {ReferenceType::get(type), ReferenceType::get(type)}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS, Operator::NOT_EQUAL})
    for (auto type : {BuiltinType::BOOL, BuiltinType::CHAR})
      CHECK(context.addImplicitFunction(op, FunctionSignature(BuiltinType::BOOL,
          {ReferenceType::get(type), ReferenceType::get(type)}, {}).setBuiltin()));
  auto metaTypes = {BuiltinType::ANY_TYPE, BuiltinType::STRUCT_TYPE, BuiltinType::ENUM_TYPE, BuiltinType::UNION_TYPE};
  CHECK(context.addImplicitFunction(Operator::EQUALS, FunctionSignature(BuiltinType::BOOL, {{BuiltinType::ANY_TYPE}, {BuiltinType::ANY_TYPE}}, {}).setBuiltin()));
  addBuiltInConcepts(context);
  context.addBuiltInFunction("enum_count", BuiltinType::INT, {SType(BuiltinType::ENUM_TYPE)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto enumType = args[0].dynamicCast<EnumType>())
          return (SType) CompileTimeValue::get((int) enumType->elements.size());
        else
          fail();
      });
  context.addBuiltInFunction("struct_count", BuiltinType::INT, {SType(BuiltinType::STRUCT_TYPE)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          return (SType) CompileTimeValue::get((int) structType->members.size());
        else
          fail();
      });
  context.addBuiltInFunction("union_count", BuiltinType::INT, {SType(BuiltinType::UNION_TYPE)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          return (SType) CompileTimeValue::get((int) structType->alternatives.size());
        else
          fail();
      });
  context.addBuiltInFunction("to_string", BuiltinType::STRING, {SType(BuiltinType::ANYTHING)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        return (SType) CompileTimeValue::get(args[0]->getName());
      });
  context.addBuiltInFunction("get_member_name", BuiltinType::STRING, {SType(BuiltinType::STRUCT_TYPE), SType(BuiltinType::INT)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          if (auto value = args[1].dynamicCast<CompileTimeValue>())
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->members.size())
                return "Struct " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return (SType) CompileTimeValue::get(structType->members[*intValue].name);
            }
        fail();
      });
  context.addBuiltInFunction("is_const_member", BuiltinType::BOOL, {SType(BuiltinType::STRUCT_TYPE), SType(BuiltinType::INT)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          if (auto value = args[1].dynamicCast<CompileTimeValue>())
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->members.size())
                return "Struct " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return (SType) CompileTimeValue::get(structType->members[*intValue].isConst);
            }
        fail();
      });
  context.addBuiltInFunction("get_member_type", BuiltinType::ANY_TYPE, {SType(BuiltinType::STRUCT_TYPE), SType(BuiltinType::INT)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          if (auto value = args[1].dynamicCast<CompileTimeValue>())
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->members.size())
                return "Struct " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return structType->members[*intValue].type;
            }
        fail();
      });
  context.addBuiltInFunction("get_alternative_name", BuiltinType::STRING, {SType(BuiltinType::UNION_TYPE), SType(BuiltinType::INT)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          if (auto value = args[1].dynamicCast<CompileTimeValue>())
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->alternatives.size())
                return "Union " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return (SType) CompileTimeValue::get(structType->alternatives[*intValue].name);
            }
        fail();
      });
  context.addBuiltInFunction("get_alternative_type", BuiltinType::ANY_TYPE, {SType(BuiltinType::UNION_TYPE), SType(BuiltinType::INT)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          if (auto value = args[1].dynamicCast<CompileTimeValue>())
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->alternatives.size())
                return "Union " + quote(structType->getName()) + " member index out of range: "s + to_string(*intValue);
              return structType->alternatives[*intValue].type;
            }
        fail();
      });
  context.addBuiltInFunction("string_length", BuiltinType::INT, {SType(BuiltinType::STRING)},
      [](const Context&, vector<SType> args) -> WithError<SType> {
        if (auto value = args[0].dynamicCast<CompileTimeValue>())
          if (auto s = value->value.getReferenceMaybe<string>())
            return (SType) CompileTimeValue::get((int) s->size());
        fail();
      });
  context.addBuiltInFunction("known_size", BuiltinType::BOOL, {SType(BuiltinType::ANY_TYPE)},
      [](const Context& context, vector<SType> args) -> WithError<SType> {
        return (SType) CompileTimeValue::get(!!args[0]->getSizeError(context));
      });
  return context;
}

static JustError<ErrorLoc> addExportedContext(const Context& primaryContext, ImportCache& cache, AST& ast,
    const string& path, bool isBuiltIn) {
  cache.pushCurrentImport(path, isBuiltIn);
  auto importContext = primaryContext.getChild(true);
  for (auto& elem : ast.elems) {
    if (elem->exported) {
      TRY(elem->addToContext(importContext, cache, primaryContext));
      elem->addGeneratedConstructor(importContext, ast);
    }
  }
  for (auto& elem : ast.elems)
    if (elem->exported)
      TRY(elem->check(importContext));
  cache.popCurrentImport(isBuiltIn);
  cache.insert(path, std::move(importContext), isBuiltIn || cache.isCurrentlyBuiltIn());
  return success;
}

WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST& ast, Context& context,
    const Context& primaryContext, ImportCache& cache, bool isBuiltInModule) {
  TRY(addExportedContext(primaryContext, cache, ast, path, isBuiltInModule));
  context.merge(cache.getContext(path));
  for (auto& elem : ast.elems)
    if (!elem->exported) {
      TRY(elem->addToContext(context, cache, primaryContext));
      elem->addGeneratedConstructor(context, ast);
    }
  for (auto& elem : ast.elems)
    TRY(elem->check(context, true));
  return cache.getAllImports();
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

JustError<ErrorLoc> ExpressionStatement::check(Context& context, bool) {
  auto oldExpr = expr.get();
  auto res = TRY(getType(context, expr));
  isConstant = oldExpr != expr.get();
  noReturnExpr = res == BuiltinType::NORETURN;
  if (!canDiscard && res != BuiltinType::VOID && res != BuiltinType::NORETURN)
    return codeLoc.getError("Expression result of type " + quote(res->getName()) + " discarded");
  if (canDiscard && !isConstant) {
    expr = unique<FunctionCall>(IdentifierInfo("discard_impl", codeLoc), std::move(expr), false);
    CHECK(!!getType(context, expr));
  }
  return success;
}

unique_ptr<Statement> ExpressionStatement::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  auto ret = unique<ExpressionStatement>(fun(expr.get()));
  ret->canDiscard = canDiscard;
  ret->noReturnExpr = noReturnExpr;
  return ret;
}

void ExpressionStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(expr.get());
}

JustError<ErrorLoc> ExpressionStatement::checkMovesImpl(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

bool ExpressionStatement::hasReturnStatement() const {
  return noReturnExpr;
}

WithEvalError<StatementEvalResult> ExpressionStatement::eval(Context& context) const {
  TRY(expr->eval(context));
  return StatementEvalResult{};
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

JustError<ErrorLoc> FunctionCall::checkNamedArgs() const {
  int cnt = argNames.size();
  if (variadicArgs)
    --cnt;
  for (int i = 0; i < cnt; ++i) {
    optional<string> paramName;
    if (auto def = functionInfo->getParent()->getDefinition())
      paramName = functionInfo->getParamName(callType ? (i + 1) : i, def);
    if (argNames[i] && paramName && argNames[i] != paramName) {
      return arguments[i]->codeLoc.getError("Function argument " + quote(*argNames[i]) +
          " doesn't match parameter " + quote(*paramName) + " of function " +
          functionInfo->prettyString());
    }
  }
  return success;
}

JustError<ErrorLoc> FunctionCall::checkVariadicCall(const Context& callContext) {
  bool variadicCall = variadicArgs && !callContext.getExpandedVariablePack();
  bool variadicTemplateCall = variadicTemplateArgs && !callContext.getExpandedTypePack();
  if (!variadicCall && !variadicTemplateCall)
    return success;
  auto unExpandedTypePack = callContext.getUnexpandedTypePack();
  if (variadicTemplateCall && !unExpandedTypePack)
    return codeLoc.getError("No unexpanded template parameter pack found");
  nullable<SType> returnType;
  vector<SType> expanded;
  vector<SType> expandedArgs;
  unordered_set<FunctionInfo*> allCalls;
  auto typePack = unExpandedTypePack->second;
  while (true) {
    auto call = cast<FunctionCall>(deepCopy());
    auto context = callContext.getChild();
    if (variadicTemplateCall)
      context.addExpandedTypePack(templateArgs->back()->getName(), expanded);
    if (variadicCall)
      context.addExpandedVariablePack(callContext.getUnexpandedVariablePack()->first, expandedArgs);
    auto funContext = Context(context.typeRegistry, true);
    ErrorBuffer errors;
    for (auto& e : expanded)
      for (auto& fun : callContext.getAllFunctions())
        if (!!fun->type.concept) {
          auto replaced = fun;
          auto from = unExpandedTypePack->second;
            replaced = replaceInFunction(funContext, replaced, from, e, errors);
          if (errors.empty() && replaced != fun)
            ignore(funContext.addFunction(replaced));
          CHECK(errors.empty()); // not sure if any errors should appear here, so checking just in case
        }
    context.merge(funContext);
    auto type = TRY(call->getTypeImpl(context));
    if (!!returnType && returnType != type)
      return codeLoc.getError("Return type mismatch between called functions in variadic function call");
    returnType = type;
    expanded.push_back(shared<TemplateParameterType>(getExpandedParamName(
        typePack->getName(), expanded.size()), codeLoc));
    if (auto variablePack = callContext.getUnexpandedVariablePack())
      expandedArgs.push_back(variablePack->second->replace(callContext, typePack, expanded.back(), errors));
    CHECK(errors.empty());
    auto target = call->functionInfo->getParent();
    if (allCalls.count(target.get()))
      break;
    allCalls.insert(target.get());
  }
  return success;
}

JustError<ErrorLoc> FunctionCall::considerSpecialCalls(const Context& context) {
  if (identifier.parts.empty())
    return success;
  auto var = identifier.parts[0].name;
  auto type = [&]() -> nullable<SType>{
    if (auto t = context.getTypeOfVariable(var))
      return *t;
    if (auto t = context.getTypeFromString(identifier))
      if (!(*t)->getType()->isMetaType())
        return (*t)->getType();
    return nullptr;
  }();
  if (!methodCall) {
    if (type) {
      if (auto functionType = type->removePointer().dynamicCast<FunctionType>()) {
        identifier.parts[0].name = functionType->name;
      } else {
        auto tmp = std::move(arguments);
        arguments.clear();
        arguments.push_back(unique<Variable>(identifier));
        methodCall = true;
        arguments.append(std::move(tmp));
        identifier.parts[0].name = "invoke";
      }
    }
  } else if (!arguments.empty()) {
    auto argType = TRY(getType(context, arguments[0]));
    if (var == "invoke" && !arguments.empty()) {
      if (auto functionType = argType->removePointer().dynamicCast<FunctionType>()) {
        identifier = IdentifierInfo(functionType->name, codeLoc);
        arguments.removeIndex(0);
      }
    } else
    if (argType->getTypeOfMember(var)) {
      arguments[0] = unique<MemberAccessExpression>(codeLoc, std::move(arguments[0]), var);
      identifier.parts[0].name = "invoke";
    }
  }
  return success;
}

WithErrorLine<SType> FunctionCall::getTypeImpl(const Context& callContext) {
  optional<ErrorLoc> error;
  if (!functionInfo) {
    templateArgs = TRY(callContext.getTypeList(identifier.parts.back().templateArguments, variadicTemplateArgs));
    TRY(considerSpecialCalls(callContext));
    vector<SType> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = 0; i < arguments.size(); ++i) {
      auto context = callContext.getChild();
      if (variadicArgs && i == arguments.size() - 1) {
        if (auto types = callContext.getExpandedVariablePack().addCodeLoc(arguments[i]->codeLoc)) {
          auto origArg = arguments[i]->deepCopy();
          arguments.pop_back();
          for (int j = 0; j < types->second.size(); ++j) {
            auto varName = getExpandedParamName(types->first, j);
            arguments.push_back(origArg->replaceVar(types->first, varName));
            auto argContext = callContext.getChild();
            argContext.addVariable(varName, ReferenceType::get(types->second[j]), arguments.back()->codeLoc);
            argTypes.push_back(TRY(getType(argContext, arguments.back())));
            argLocs.push_back(arguments.back()->codeLoc);
          }
        }
        break;
      } else {
        argTypes.push_back(TRY(getType(context, arguments[i])));
        argLocs.push_back(arguments[i]->codeLoc);
      }
    }
    auto tryMethodCall = [&](MethodCallType thisCallType, const Type& origType) -> JustError<ErrorLoc> {
      auto res = getFunction(callContext, codeLoc, identifier, *templateArgs, argTypes, argLocs, arguments);
      if (res)
        callType = thisCallType;
      if (callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER) {
        if (!origType.isReference() && argTypes[0]->removePointer()->hasDestructor()) {
          destructorCall = TRY(getDestructor(callContext, argTypes[0]->removePointer(), codeLoc));
          TRY(destructorCall->addInstance(callContext));
        }
      }
      res.unpack(functionInfo, error);
      return success;
    };
    if (methodCall) {
      auto leftType = argTypes[0];
      if (!leftType->removeReference().dynamicCast<PointerType>() &&
          !leftType->removeReference().dynamicCast<MutablePointerType>())
        TRY(tryMethodCall(MethodCallType::FUNCTION_AS_METHOD, *leftType));
      if (!functionInfo) {
        argTypes[0] = leftType.dynamicCast<MutableReferenceType>()
            ? SType(MutablePointerType::get(leftType->removeReference()))
            : SType(PointerType::get(leftType->removeReference()));
        TRY(tryMethodCall(MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER, *leftType));
      }
    } else
      getFunction(callContext, codeLoc, identifier, *templateArgs, argTypes, argLocs, arguments)
          .unpack(functionInfo, error);
  }
  if (functionInfo) {
    TRY(checkVariadicCall(callContext));
    TRY(functionInfo->type.retVal->getSizeError(callContext).addCodeLoc(codeLoc));
    if (!callContext.getExpandedVariablePack())
      TRY(checkNamedArgs());
    if (auto res = functionInfo->addInstance(callContext); !res)
      return codeLoc.getError("When instantiating template " + functionInfo->getParent()->prettyString() + " as " + functionInfo->prettyString()  + ":\n"
        + res.get_error().loc.toString() + ": " + res.get_error().error);
    return functionInfo->type.retVal;
  }
  return *error;
}

WithEvalError<EvalResult> FunctionCall::eval(const Context& context) const {
  if (auto name = identifier.asBasicIdentifier()) {
    vector<SType> args;
    vector<CodeLoc> locs;
    for (auto& e : arguments) {
      locs.push_back(e->codeLoc);
      args.push_back(TRY(e->eval(context)).value->removeValueReference());
    }
    return EvalResult{TRY(context.invokeFunction(*name, codeLoc, std::move(args), std::move(locs))), true};
  }
  return EvalError::noEval();
}

unique_ptr<Expression> FunctionCall::replaceVar(string from, string to) const {
  auto ret = cast<FunctionCall>(transform(
      [from, to](Statement* expr) { return expr->replaceVar(from, to); },
      [from, to](Expression* expr) { return expr->replaceVar(from, to); }));
  ret->argNames = ::transform(argNames, [&](auto& n) -> optional<string> { if (n == from) return to; else return n; });
  return ret;
}

unique_ptr<Expression> FunctionCall::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  auto ret = unique<FunctionCall>(codeLoc, methodCall, Private{});
  ret->identifier = identifier;
  for (auto& arg : arguments)
    ret->arguments.push_back(fun(arg.get()));
  ret->templateArgs = templateArgs;
  ret->argNames = argNames;
  ret->variadicArgs = variadicArgs;
  ret->variadicTemplateArgs = variadicTemplateArgs;
  return ret;
}

void FunctionCall::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  for (auto& arg : arguments)
    f2(arg.get());
}

void FunctionCall::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  this->Expression::addFunctionCalls(fun);
  fun(functionInfo.get());
  if (!!destructorCall)
    fun(destructorCall.get());
}

JustError<ErrorLoc> FunctionCall::checkMoves(MoveChecker& checker) const {
  for (auto& e : arguments)
    TRY(e->checkMoves(checker));
  return success;
}

FunctionCall::FunctionCall(CodeLoc l, bool methodCall, Private) : Expression(l), methodCall(methodCall) {}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

JustError<ErrorLoc> SwitchStatement::check(Context& context, bool) {
  auto type = TRY(getType(context, expr));
  if (type->hasDestructor()) {
    destructorCall = TRY(getDestructor(context, type, codeLoc));
    TRY(destructorCall->addInstance(context));
  }
  if (!context.isFullyDefined(type->removePointer().get()))
    return codeLoc.getError("Type " + quote(type->getName()) + " is incomplete in this context");
  return type->handleSwitchStatement(*this, context, Type::ArgumentType::VALUE);
}

JustError<ErrorLoc> SwitchStatement::checkMovesImpl(MoveChecker& checker) const {
  TRY(expr->checkMoves(checker));
  checker.startBlock();
  OnExit onExit([&]{ checker.endBlock();});
  for (auto& elem : caseElems) {
    checker.newAlternative();
    checker.startBlock();
    OnExit onExit2([&]{ checker.endBlock();});
    if (elem.declaredVar)
      checker.addVariable(*elem.declaredVar);
    TRY(elem.block->checkMoves(checker));
  }
  if (defaultBlock) {
    checker.newAlternative();
    TRY(defaultBlock->checkMoves(checker));
  }
  return success;
}

unique_ptr<Statement> SwitchStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  auto ret = unique<SwitchStatement>(codeLoc, exprFun(expr.get()));
  ret->targetType = targetType;
  if (defaultBlock)
    ret->defaultBlock = cast<StatementBlock>(fun(defaultBlock.get()));
  for (auto& elem : caseElems)
    ret->caseElems.push_back(elem.transform(fun, exprFun));
  return ret;
}

void SwitchStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(expr.get());
  for (auto& elem : caseElems)
    elem.visit(f1, f2);
  if (defaultBlock)
    f1(defaultBlock.get());
}

void SwitchStatement::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  Statement::addFunctionCalls(fun);
  if (destructorCall)
    fun(destructorCall.get());
}

bool SwitchStatement::hasReturnStatement() const {
  for (auto& elem : caseElems)
    if (!elem.block->hasReturnStatement())
      return false;
  if (defaultBlock && !defaultBlock->hasReturnStatement())
    return false;
  return true;
}

UnionDefinition::UnionDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

JustError<ErrorLoc> UnionDefinition::registerTypes(const Context& primaryContext, TypeRegistry* r) {
  if (!type) {
    auto ret = r->addStruct(name, false, codeLoc).addCodeLoc(codeLoc);
    if (ret) {
      type = r->getStruct(name);
      type->templateParams = TRY(getTemplateParams(templateInfo, primaryContext));
    }
    return ret;
  }
  return success;
}

unique_ptr<Statement> UnionDefinition::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<UnionDefinition>(*this);
}

static WithErrorLine<set<shared_ptr<AttributeType>>> getAttributeTypes(const Context& context,
    const vector<AttributeInfo>& attributes) {
  set<shared_ptr<AttributeType>> ret;
  for (auto& attr : attributes)
    if (auto t = context.getType(attr.name)) {
      if (auto attrType = t.get().dynamicCast<AttributeType>())
        ret.insert(attrType);
      else
        return attr.codeLoc.getError(quote(attr.name) + " is not an attribute");
    } else
      return attr.codeLoc.getError("Attribute not found: " + quote(attr.name));
  return ret;
}

JustError<ErrorLoc> UnionDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflict(name, "type").addCodeLoc(codeLoc));
  context.addType(name, type.get());
  for (auto& attr : TRY(getAttributeTypes(context, attributes)))
    context.setAttribute(type.get(), attr, type->templateParams);
  type->definition = codeLoc;
  auto membersContext = context.getChild();
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  type->requirements = TRY(applyRequirements(membersContext, templateInfo));
  unordered_set<string> subtypeNames;
  if (elements.size() > type->alternatives.size())
    for (auto& subtype : elements) {
      if (subtypeNames.count(subtype.name))
        return subtype.codeLoc.getError("Duplicate union member: " + quote(subtype.name));
      subtypeNames.insert(subtype.name);
      type->alternatives.push_back({subtype.name, TRY(membersContext.getTypeFromString(subtype.type)), false});
      vector<SType> params;
      auto subtypeInfo = TRY(membersContext.getTypeFromString(subtype.type));
      if (subtypeInfo != BuiltinType::VOID)
        params.push_back(subtypeInfo);
      auto constructor = FunctionSignature(type.get(), params, {});
      constructor.parentType = type.get();
      CHECK(type->staticContext.addImplicitFunction(subtype.name, constructor));
    }
  return success;
}

JustError<ErrorLoc> UnionDefinition::check(Context& context, bool) {
  auto bodyContext = context.getChild();
  for (auto& param : type->templateParams)
    bodyContext.addType(param->getName(), param);
  CHECK(!!applyRequirements(bodyContext, templateInfo));
  type->updateInstantations(context);
  return success;
}

JustError<ErrorLoc> StructDefinition::registerTypes(const Context& primaryContext, TypeRegistry* r) {
  if (!type) {
    auto ret = r->addStruct(name, external, codeLoc).addCodeLoc(codeLoc);
    if (ret) {
      type = r->getStruct(name);
      type->templateParams = TRY(getTemplateParams(templateInfo, primaryContext));
    }
    return ret;
  }
  return success;
}

unique_ptr<Statement> StructDefinition::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<StructDefinition>(*this);
}

JustError<ErrorLoc> StructDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflict(name, "type").addCodeLoc(codeLoc));
  context.addType(name, type.get());
  for (auto& attr : TRY(getAttributeTypes(context, attributes)))
    context.setAttribute(type.get(), attr, type->templateParams);
  auto membersContext = context.getChild();
  addTemplateParams(membersContext, type->templateParams, false);
  type->requirements = TRY(applyRequirements(membersContext, templateInfo));
  if (members.size() > type->members.size())
    for (auto& member : members)
      type->members.push_back({member.name, TRY(membersContext.getTypeFromString(member.type)), member.isConst});
  for (auto& member : members)
    TRY(type->members.back().type->getSizeError(membersContext).addCodeLoc(member.codeLoc));
  for (int i = 0; i < members.size(); ++i)
    for (int j = i + 1; j < members.size(); ++j)
      if (members[i].name == members[j].name)
        return members[j].codeLoc.getError("Duplicate member: " + quote(members[j].name));
  context.setStructMembers(type.get(), type->members.transform([](auto& elem) { return elem.type; }),
      type->templateParams);
  return success;
}

JustError<ErrorLoc> StructDefinition::check(Context& context, bool notInImport) {
  auto methodBodyContext = context.getChild();
  addTemplateParams(methodBodyContext, type->templateParams, false);
  CHECK(!!applyRequirements(methodBodyContext, templateInfo));
  type->updateInstantations(context);
  TRY(type->getSizeError(context).addCodeLoc(codeLoc));
  if (exported && type->destructor && !type->destructor->getDefinition()->exported)
    return type->destructor->getDefinition()->codeLoc.getError(
        "Destuctor function of an exported type must also be exported");
  return success;
}

void StructDefinition::addGeneratedConstructor(Context& context, const AST& ast) const {
  bool hasUserDefinedConstructors = false;
  for (auto& elem : ast.elems)
    if (auto functionDef = dynamic_cast<const FunctionDefinition*>(elem.get()))
      if (functionDef->name.contains<ConstructorTag>() && functionDef->returnType.parts[0].name == name)
        hasUserDefinedConstructors = true;
  if (!external && type->getStaticContext().getAllFunctions().empty()) {
    vector<SType> constructorParams;
    for (auto& member : type->members)
      constructorParams.push_back(member.type);
    auto fun = FunctionSignature(type.get(), std::move(constructorParams), type->templateParams);
    fun.generatedConstructor = true;
    if (!hasUserDefinedConstructors)
      CHECK(context.addImplicitFunction(ConstructorTag{}, fun));
    fun.templateParams.clear();
    fun.parentType = type.get();
    CHECK(type->getStaticContext().addImplicitFunction(ConstructorTag{}, fun));
    type->getStaticContext().addType(name, type.get());
  }
}

MoveExpression::MoveExpression(CodeLoc l, string id, bool hasDestructor)
    : Expression(l), identifier(id), hasDestructor(hasDestructor) {}

WithErrorLine<SType> MoveExpression::getTypeImpl(const Context& context) {
  if (!type) {
    if (auto ret = context.getTypeOfVariable(identifier)) {
      if (context.isNonMovable(identifier))
        return codeLoc.getError("Can't move " + quote(identifier) + ", because the switch condition is an l-value");
      if (context.isCapturedVariable(identifier))
        return codeLoc.getError("Can't move from a captured value");
      if (!ret.get_value().dynamicCast<MutableReferenceType>() &&
          !ret.get_value().dynamicCast<ReferenceType>())
        return codeLoc.getError("Can't move from " + quote(ret.get_value()->getName()));
      type = ret.get_value()->removeReference();
    } else
      return codeLoc.getError(ret.get_error());
  }
  hasDestructor = type->hasDestructor();
  return type.get();
}

unique_ptr<Expression> MoveExpression::replaceVar(string from, string to) const {
  if (from == identifier)
    return unique<MoveExpression>(codeLoc, to, hasDestructor);
  else
    return deepCopy();
}

unique_ptr<Expression> MoveExpression::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<MoveExpression>(codeLoc, identifier, hasDestructor);
}

JustError<ErrorLoc> MoveExpression::checkMoves(MoveChecker& checker) const {
  return checker.moveVariable(codeLoc, identifier).addCodeLoc(codeLoc);
}

EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

JustError<ErrorLoc> EmbedStatement::check(Context& context, bool) {
  replacements = context.getSubstitutions();
  return success;
}

Statement::TopLevelAllowance EmbedStatement::allowTopLevel() const {
  return returns ? TopLevelAllowance::CANT : TopLevelAllowance::CAN;
}

unique_ptr<Statement> EmbedStatement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  auto ret = unique<EmbedStatement>(codeLoc, value);
  ret->returns = returns;
  ret->isTopLevel = isTopLevel;
  return std::move(ret);
}

bool EmbedStatement::hasReturnStatement() const {
  return returns;
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<VariableDeclaration> i, unique_ptr<Expression> c,
                                   unique_ptr<Expression> it, unique_ptr<Statement> b)
  : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

JustError<ErrorLoc> ForLoopStatement::check(Context& context, bool) {
  auto bodyContext = context.getChild();
  auto mutInit = cast<VariableDeclaration>(init->deepCopy());
  TRY(init->check(bodyContext));
  auto loopContext = context.getChild();
  mutInit->isMutable = true;
  TRY(mutInit->check(loopContext));
  auto condType = TRY(getType(loopContext, cond));
  if (condType != BuiltinType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  TRY(getType(loopContext, iter));
  loopId = bodyContext.setIsInLoop();
  return body->check(bodyContext);
}

JustError<ErrorLoc> ForLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  TRY(init->checkMoves(checker));
  checker.startLoop(loopId);
  TRY(cond->checkMoves(checker));
  TRY(iter->checkMoves(checker));
  if (auto res = body->checkMoves(checker); !res) {
    TRY(checker.endLoop(loopId));
    return res.get_error();
  }
  return checker.endLoop(loopId);
}

unique_ptr<Statement> ForLoopStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  return unique<ForLoopStatement>(codeLoc,
      cast<VariableDeclaration>(fun(init.get())),
      exprFun(cond.get()),
      exprFun(iter.get()),
      fun(body.get()));
}

void ForLoopStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f1(init.get());
  f2(cond.get());
  f2(iter.get());
  f1(body.get());
}

WithEvalError<StatementEvalResult> ForLoopStatement::eval(Context& context) const {
  StatementEvalResult res;
  auto forContext = context.getChild();
  bool wasMutable = init->isMutable;
  init->isMutable = true;
  CHECK(TRY(init->eval(forContext)).empty());
  const int maxIterations = 500;
  int countIter = 0;
  while (1) {
    auto bodyContext = forContext.getChild();
    auto condValue = TRY(cond->eval(bodyContext)).value;
    if (condValue->getType() != BuiltinType::BOOL)
    return EvalError::withError("Expected a compile-time value of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(condValue->getName()));
    auto value = condValue.dynamicCast<CompileTimeValue>();
    bool onePass = false;
    if (auto b = value->value.getValueMaybe<bool>()) {
      if (!*b)
        break;
    } else
      onePass = true;
    auto thisBody = body->deepCopy();
    auto bodyContext2 = bodyContext.getChild();
    if (!wasMutable)
      bodyContext2.addType(init->identifier, bodyContext2.getType(init->identifier)->removeValueReference());
    if (auto res = thisBody->check(bodyContext2); !res)
      return EvalError::withError(res.get_error().toString());
    res.push_back(unique<StatementBlock>(codeLoc, makeVec(std::move(thisBody))));
    TRY(iter->eval(bodyContext));
    if (onePass)
      break;
    if (++countIter > maxIterations)
      return EvalError::withError("Static loop reached maximum number of iterations (" + to_string(maxIterations) + ")");
  }
  return std::move(res);
}

WhileLoopStatement::WhileLoopStatement(CodeLoc l, unique_ptr<Expression> c, unique_ptr<Statement> b)
  : Statement(l), cond(std::move(c)), body(std::move(b)) {}

JustError<ErrorLoc> WhileLoopStatement::check(Context& context, bool) {
  auto bodyContext = context.getChild();
  auto condType = TRY(getType(bodyContext, cond));
  if (condType != BuiltinType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  loopId = bodyContext.setIsInLoop();
  return body->check(bodyContext);
}

JustError<ErrorLoc> WhileLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  checker.startLoop(loopId);
  TRY(cond->checkMoves(checker));
  if (auto res = body->checkMoves(checker); !res) {
    TRY(checker.endLoop(loopId));
    return res.get_error();
  }
  return checker.endLoop(loopId);
}

unique_ptr<Statement> WhileLoopStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  return unique<WhileLoopStatement>(codeLoc,
      exprFun(cond.get()),
      fun(body.get()));
}

void WhileLoopStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(cond.get());
  f1(body.get());
}

WithEvalError<StatementEvalResult> WhileLoopStatement::eval(Context& context) const {
  StatementEvalResult res;
  auto forContext = context.getChild();
  const int maxIterations = 500;
  int countIter = 0;
  while (1) {
    auto bodyContext = forContext.getChild();
    auto condValue = TRY(cond->eval(bodyContext)).value;
    if (condValue->getType() != BuiltinType::BOOL)
    return EvalError::withError("Expected a compile-time value of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(condValue->getName()));
    auto value = condValue.dynamicCast<CompileTimeValue>();
    bool onePass = false;
    if (auto b = value->value.getValueMaybe<bool>()) {
      if (!*b)
        break;
    } else
      onePass = true;
    auto thisBody = body->deepCopy();
    auto bodyContext2 = bodyContext.getChild();
    if (auto res = thisBody->check(bodyContext2); !res)
      return EvalError::withError(res.get_error().toString());
    res.push_back(unique<StatementBlock>(codeLoc, makeVec(std::move(thisBody))));
    if (onePass)
      break;
    if (++countIter > maxIterations)
      return EvalError::withError("Static loop reached maximum number of iterations (" + to_string(maxIterations) + ")");
  }
  return std::move(res);
}

ImportStatement::ImportStatement(CodeLoc l, string p, bool isBuiltIn)
    : Statement(l), path(p), isBuiltIn(isBuiltIn) {
}

JustError<ErrorLoc> ImportStatement::registerTypes(const Context& context, TypeRegistry* r, ASTCache& cache,
    const vector<string>& importDirs) {
  if (ast)
    return success;
  for (auto importDir : concat({getParentPath(codeLoc.file)}, importDirs)) {
    auto importPath = fs::path(importDir) / path;
    if (auto content = readFromFile(importPath.c_str())) {
      absolutePath = string(fs::canonical(importPath));
      bool firstTime = !cache.hasASTInUnit(*absolutePath);
      ast = TRY(cache.getAST(*absolutePath));
      if (firstTime)
        for (auto& elem : ast->elems)
          TRY(elem->registerTypes(context, r, cache, importDirs));
      break;
    }
  }
  if (!ast)
    return codeLoc.getError("Couldn't resolve import path: " + path);
  return success;
}

unique_ptr<Statement> ImportStatement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<ImportStatement>(codeLoc, path, isBuiltIn);
}

JustError<ErrorLoc> ImportStatement::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  if (cache.isCurrentlyBuiltIn() && isBuiltIn) {
    ast = nullptr;
    return success;
  }
  if (cache.isCurrentlyImported(*absolutePath))
    return codeLoc.getError("Exported import cycle: " + combine(cache.getCurrentImports(), ", "));
  if (!cache.contains(*absolutePath)) {
    TRY(addExportedContext(primaryContext, cache, *ast, *absolutePath, isBuiltIn));
  } else
    INFO << "Import " << *absolutePath << " already cached";
  context.merge(cache.getContext(*absolutePath));
  return success;
}

JustError<ErrorLoc> ImportStatement::check(Context& context, bool) {
  return success;
}

WithEvalError<EvalResult> Expression::eval(const Context&) const {
  return EvalError::noEval();
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

JustError<ErrorLoc> EnumDefinition::registerTypes(const Context&, TypeRegistry* r) {
  return r->addEnum(name, external, codeLoc).addCodeLoc(codeLoc);
}

unique_ptr<Statement> EnumDefinition::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  auto ret = unique<EnumDefinition>(codeLoc, name);
  ret->elements = elements;
  return ret;
}

JustError<ErrorLoc> EnumDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflict(name, "type").addCodeLoc(codeLoc));
  auto type = context.typeRegistry->getEnum(name).get();
  if (elements.empty())
    return codeLoc.getError("Enum requires at least one element");
  type->definition = codeLoc;
  type->elements = elements;
  type->external = external;
  unordered_set<string> occurences;
  for (auto& e : elements)
    if (occurences.count(e))
      return codeLoc.getError("Duplicate enum element: " + quote(e));
  context.addType(name, std::move(type));
  return success;
}

JustError<ErrorLoc> EnumDefinition::check(Context&, bool) {
  return success;
}

EnumConstant::EnumConstant(CodeLoc l, string name, string element) : Expression(l), enumName(name), enumElement(element) {
}

WithErrorLine<SType> EnumConstant::getTypeImpl(const Context& context) {
  auto type = TRY(context.getTypeFromString(IdentifierInfo(enumName, codeLoc)));
  if (auto enumType = type.dynamicCast<EnumType>()) {
    if (!context.isFullyDefined(enumType.get()))
      return codeLoc.getError("Enum type " + quote(enumType->getName()) + " elements are not known in this context");
    if (!contains(enumType->elements, enumElement))
      return codeLoc.getError(quote(enumElement) + " is not an element of enum " + quote(enumName));
  } else
    return codeLoc.getError(quote(type.get()->getName()) + " is not an enum type");
  return type;
}

WithEvalError<EvalResult> EnumConstant::eval(const Context& context) const {
  if (auto type = context.getTypeFromString(IdentifierInfo(enumName, codeLoc))) {
    if (auto enumType = type->dynamicCast<EnumType>()) {
      for (int i = 0; i < enumType->elements.size(); ++i)
        if (enumType->elements[i] == enumElement)
          return EvalResult{ CompileTimeValue::get(CompileTimeValue::EnumValue{enumType, i}), true};
    }
  }
  FATAL << "Unrecognized enum element - should have been discovered by the type checker";
  fail();
}

unique_ptr<Expression> EnumConstant::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<EnumConstant>(codeLoc, enumName, enumElement);
}

ConceptDefinition::ConceptDefinition(CodeLoc l, string name) : Statement(l), name(name) {
}

void ConceptDefinition::addFatPointer(ConceptDefinition::FatPointerInfo info, shared_ptr<ConceptType> conceptType) {
  if (!conceptInstances.contains(conceptType))
    conceptInstances.push_back(conceptType);
  for (auto& elem : fatPointers)
    if (elem.type == info.type)
      return;
  fatPointers.push_back(info);
}

JustError<ErrorLoc> ConceptDefinition::addToContext(Context& context) {
  auto concept = shared<Concept>(name, this, Context(context.typeRegistry, true), templateInfo.variadic);
  auto declarationsContext = context.getChild();
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    auto& param = templateInfo.params[i];
    if (param.type)
      return param.codeLoc.getError("Concept value template parameters are not supported.");
    concept->modParams().push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    declarationsContext.addType(param.name, concept->modParams().back());
    if (templateInfo.variadic && i == templateInfo.params.size() - 1)
      declarationsContext.addUnexpandedTypePack(templateInfo.params[i].name, concept->modParams().back());
  }
  for (auto& function : functions) {
    if (function->isVirtual)
      return function->codeLoc.getError("Virtual functions are not allowed here");
    TRY(function->setFunctionSignature(declarationsContext, concept));
    TRY(function->check(declarationsContext));
    TRY(concept->modContext().addFunction(function->functionInfo.get()).addCodeLoc(function->codeLoc));
  }
  context.addConcept(name, concept);
  return success;
}

JustError<ErrorLoc> ConceptDefinition::check(Context& context, bool) {
  return success;
}

unique_ptr<Statement> ConceptDefinition::transform(const StmtTransformFun& f1, const ExprTransformFun&) const {
  auto ret = unique<ConceptDefinition>(codeLoc, name);
  for (auto& f : functions)
    ret->functions.push_back(cast<FunctionDefinition>(f1(f.get())));
  ret->templateInfo = templateInfo;
  return ret;
}

CodegenStage CodegenStage::types() {
  CodegenStage ret;
  ret.isTypes = true;
  ret.isDefine = false;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::define() {
  CodegenStage ret;
  ret.isDefine = true;
  ret.isTypes = false;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::declare() {
  CodegenStage ret;
  ret.isTypes = false;
  ret.isDefine = false;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::setImport() {
  isImport = true;
  return *this;
}

JustError<ErrorLoc> BreakStatement::check(Context& context, bool) {
  if (auto id = context.getLoopId()) {
    loopId = *id;
    return success;
  } else
    return codeLoc.getError("Break statement outside of a loop");
}

unique_ptr<Statement> BreakStatement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  auto ret = unique<BreakStatement>(codeLoc);
  ret->loopId = loopId;
  return ret;
}

JustError<ErrorLoc> BreakStatement::checkMovesImpl(MoveChecker& checker) const {
  checker.breakStatement(loopId);
  return success;
}

JustError<ErrorLoc> ContinueStatement::check(Context& context, bool) {
  if (!context.getLoopId())
    return codeLoc.getError("Continue statement outside of a loop");
  return success;
}

unique_ptr<Statement> ContinueStatement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<ContinueStatement>(codeLoc);
}

JustError<ErrorLoc> ContinueStatement::checkMovesImpl(MoveChecker& checker) const {
  return checker.continueStatement().addCodeLoc(codeLoc);
}

ArrayLiteral::ArrayLiteral(CodeLoc codeLoc) : Expression(codeLoc) {
}

WithErrorLine<SType> ArrayLiteral::getTypeImpl(const Context& context) {
  auto typeTmp = TRY(typeId ? context.getTypeFromString(*typeId, false) : getType(context, contents[0]));
  auto ret = typeTmp->removeReference();
  for (int i = 0; i < contents.size(); ++i) {
    if (i > 0 || !typeId)
      typeTmp = TRY(getType(context, contents[i]));
    TRY(getVariableInitializationError("construct array", context, ret, typeTmp, contents[i]).addCodeLoc(contents[i]->codeLoc));
  }
  type = ret;
  return SType(ArrayType::get(ret, CompileTimeValue::get((int)contents.size())));
}

unique_ptr<Expression> ArrayLiteral::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  auto ret = unique<ArrayLiteral>(codeLoc);
  for (auto& elem : contents)
    ret->contents.push_back(fun(elem.get()));
  ret->typeId = typeId;
  return ret;
}

void ArrayLiteral::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  for (auto& elem : contents)
    f2(elem.get());
}

JustError<ErrorLoc> ArrayLiteral::checkMoves(MoveChecker& checker) const {
  for (auto& e : contents)
    TRY(e->checkMoves(checker));
  return success;
}

WithErrorLine<SType> getType(const Context& context, unique_ptr<Expression>& expr, bool evaluateAtCompileTime) {
  auto type = TRY(expr->getTypeImpl(context));
  if (evaluateAtCompileTime) {
    if (auto type = expr->eval(context)) {
      if (type->isConstant) {
          auto c = unique<Constant>(expr->codeLoc, type->value);
          auto refValue = type->value->removeValueReference();
          if (refValue != type->value)
            c->refValue = refValue;
          expr = std::move(c);
        }
    } else
    if (type.get_error().canEval)
      return expr->codeLoc.getError(type.get_error().error);
  }
  return type;
}

SwitchStatement::CaseElem SwitchStatement::CaseElem::transform(const StmtTransformFun& fun, const ExprTransformFun&) const {
  CaseElem ret;
  ret.codeloc = codeloc;
  ret.ids = ids;
  ret.block = cast<StatementBlock>(fun(block.get()));
  return ret;
}

void SwitchStatement::CaseElem::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f1(block.get());
}

ExternConstantDeclaration::ExternConstantDeclaration(CodeLoc l, IdentifierInfo type, string identifier)
  : Statement(l), type(type), identifier(identifier) {
}

JustError<ErrorLoc> ExternConstantDeclaration::addToContext(Context& context) {
  TRY(context.checkNameConflictExcludingFunctions(identifier, "Variable").addCodeLoc(codeLoc));
  realType = TRY(context.getTypeFromString(type));
  if (realType == BuiltinType::VOID)
    return codeLoc.getError("Can't declare constant of type " + quote(BuiltinType::VOID->getName()));
  INFO << "Adding extern constant " << identifier << " of type " << realType.get()->getName();
  context.addVariable(identifier, ReferenceType::get(realType.get()), codeLoc);
  return success;
}

unique_ptr<Statement> ExternConstantDeclaration::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<ExternConstantDeclaration>(codeLoc, type, identifier);
}

LambdaExpression::LambdaExpression(CodeLoc l, vector<FunctionParameter> params, unique_ptr<StatementBlock> block,
    optional<IdentifierInfo> returnType, LambdaCaptureInfo captureInfo)
    : Expression(l), parameters(std::move(params)), block(std::move(block)), returnType(std::move(returnType)),
      captureInfo(std::move(captureInfo)) {
}

static SType getCapturedType(SType input, LambdaCaptureType type) {
  switch (type) {
    case LambdaCaptureType::REFERENCE:
      return convertReferenceToPointer(input);
    case LambdaCaptureType::COPY:
    case LambdaCaptureType::IMPLICIT_COPY:
    case LambdaCaptureType::MOVE:
      return input->removeReference();
  }
}

WithErrorLine<vector<LambdaCapture>> LambdaExpression::setLambda(Context& context) {
  vector<LambdaCapture> ret;
  for (auto& capture : captureInfo.captures) {
    if (auto type = context.getTypeOfVariable(capture.name)) {
      auto underlying = type->get()->removeReference();
/*      if (capture.type == LambdaCaptureType::IMPLICIT_COPY && !context.canConvert(*type, underlying))
        return capture.codeLoc.getError("Variable " + capture.name + " of type " +
            quote(underlying->getName()) + " can't be captured by implicit copy");*/
      if (capture.type == LambdaCaptureType::IMPLICIT_COPY) {
        if (auto f = getImplicitCopyFunction(context, capture.codeLoc, underlying)) {
          TRY(f->get()->getDefinition()->addInstance(context, *f));
          functionCalls.push_back(*f);
        } else
          return capture.codeLoc.getError("No implicit copy function defined for type " +
              quote(underlying->getName())+ "\n" + f.get_error().error);
      }
      if (capture.type == LambdaCaptureType::COPY) {
        if (auto f = getCopyFunction(context, capture.codeLoc, underlying)) {
          TRY(f->get()->getDefinition()->addInstance(context, *f));
          functionCalls.push_back(*f);
        } else
          return capture.codeLoc.getError("No copy function defined for type " +
              quote(underlying->getName())+ "\n" + f.get_error().error);
      }
      ret.push_back(LambdaCapture{capture.name, getCapturedType(*type, capture.type), capture.type});
    } else
      return capture.codeLoc.getError(type.get_error());
  }
  context.setLambda(&captureInfo);
  return ret;
}

WithErrorLine<SType> LambdaExpression::getTypeImpl(const Context& context) {
  if (type)
    return SType(type.get());
  nullable<SType> retType;
  for (int i = 0; i < parameters.size(); ++i)
    if (!parameters[i].name)
      parameters[i].name = "parameter" + to_string(i);
  if (returnType)
    retType = TRY(context.getTypeFromString(*returnType));
  auto bodyContext = context.getChild();
  ReturnTypeChecker returnChecker(retType);
  bodyContext.addReturnTypeChecker(&returnChecker);
  auto captureTypes = TRY(setLambda(bodyContext));
  vector<SType> params;
  type = LambdaType::get(context.getTemplateParams().value_or(vector<SType>()));
  type->captures = captureTypes;
  for (auto& param : parameters)
    type->parameterNames.push_back(param.name);
  params.push_back(PointerType::get(type.get()));
  set<string> paramNames;
  for (auto& param : parameters) {
    auto type = TRY(bodyContext.getTypeFromString(param.type));
    if (type == BuiltinType::VOID)
      return param.codeLoc.getError("Function parameter may not have " + quote(type->getName()) + " type");
    auto varType = param.isMutable ? SType(MutableReferenceType::get(type)) : SType(ReferenceType::get(type));
    params.push_back(type);
    if (param.name) {
      bodyContext.addVariable(*param.name, varType, param.codeLoc);
      if (paramNames.count(*param.name))
        return param.codeLoc.getError("Duplicate function parameter name: " + quote(*param.name));
      paramNames.insert(*param.name);
    }
  }
  auto bodyContext2 = bodyContext.getChild();
  // Only used to prevent from modifying comptime mutable values. Make a new flag if needed.
  bodyContext2.setIsInBranch();
  TRY(block->check(bodyContext2));
  type->captures.append(captureInfo.implicitCaptures);
  if (!returnType)
    retType = returnChecker.getReturnType();
  considerAddingVoidReturn(bodyContext2, block.get(), retType.get());
  if (!type->functionInfo) {
    FunctionSignature functionType(retType.get(), params, {});
    auto functioInfo = FunctionInfo::getImplicit("invoke"s, std::move(functionType));
    type->functionInfo = std::move(functioInfo);
  }
  TRY(checkBodyMoves());
  if (!block->hasReturnStatement() && retType != BuiltinType::VOID)
    return block->codeLoc.getError("Not all paths lead to a return statement in a lambda expression returning non-void");
  type->body = cast<Statement>(unique<ExternalStatement>(block.get()));
  type->destructorCalls = getDestructorCalls(codeLoc,
      parameters.transform([](auto& p) { return *p.name;}),
      getSubsequence(type->functionInfo->type.params, 1));
  for (auto& elem : type->destructorCalls)
    if (elem)
      TRY(elem->check(bodyContext));
  vector<unique_ptr<Statement>> toDestruct;
  for (auto& capture : type->captures) {
    if (capture.captureType != LambdaCaptureType::REFERENCE && capture.type->hasDestructor()) {
      toDestruct.push_back(getDestructorStatement(codeLoc, capture.name));
      CHECK(toDestruct.back()->check(bodyContext));
      for (auto& elem : captureInfo.captures)
        if (elem.name == capture.name)
          elem.hasConstructor = true;
    }
    if (capture.captureType == LambdaCaptureType::IMPLICIT_COPY) {
      TRY(getImplicitCopyFunction(bodyContext, codeLoc, capture.type).get()->addInstance(bodyContext));
    }
  }
  if (!toDestruct.empty())
    type->destructor = unique<StatementBlock>(codeLoc, std::move(toDestruct));
  return SType(type.get());
}

JustError<ErrorLoc> LambdaExpression::checkBodyMoves() const {
  MoveChecker moveChecker;
  for (auto& p : parameters)
    if (p.name)
      moveChecker.addVariable(*p.name);
  return block->checkMoves(moveChecker);
}

JustError<ErrorLoc> LambdaExpression::checkMoves(MoveChecker& checker) const {
  for (auto& capture : captureInfo.captures) {
    if (capture.type == LambdaCaptureType::MOVE)
      TRY(checker.moveVariable(capture.codeLoc, capture.name).addCodeLoc(capture.codeLoc));
    else
      TRY(checker.getUsageError(capture.name).addCodeLoc(capture.codeLoc));
  }
  return success;
}

unique_ptr<Expression> LambdaExpression::transform(const StmtTransformFun& fun, const ExprTransformFun& exprFun) const {
  auto ret = unique<LambdaExpression>(codeLoc, parameters, cast<StatementBlock>(block->transform(fun, exprFun)), returnType, captureInfo);
  return ret;
}

void LambdaExpression::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f1(block.get());
  for (auto& elem : type->destructorCalls)
    if (elem)
      f1(elem.get());
  if (type->destructor)
    f1(type->destructor.get());
}

void LambdaExpression::addLambdas(LambdasSet& lambdas) const {
  Expression::addLambdas(lambdas);
  lambdas.insert(type.get().get());
}

void LambdaExpression::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  Expression::addFunctionCalls(fun);
  for (auto& f : functionCalls)
    fun(f);
}

CountOfExpression::CountOfExpression(CodeLoc l, string id) : Expression(l), identifier(std::move(id)) {
}

WithErrorLine<SType> CountOfExpression::getTypeImpl(const Context& context) {
  if (auto p = context.getExpandedVariablePack())
    if (p->first == identifier)
      return SType(BuiltinType::INT);
  if (auto p = context.getExpandedTypePack())
    if (p->first == identifier)
      return SType(BuiltinType::INT);
  if (auto p = context.getUnexpandedTypePack())
    if (p->first == identifier)
      return SType(BuiltinType::INT);
  if (auto p = context.getUnexpandedVariablePack())
    if (p->first == identifier)
      return SType(BuiltinType::INT);
  return codeLoc.getError("Operator countof requires a type or variable pack");
}

unique_ptr<Expression> CountOfExpression::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<CountOfExpression>(codeLoc, identifier);
}

WithEvalError<EvalResult> CountOfExpression::eval(const Context& context) const {
  if (auto t = context.getExpandedVariablePack()) {
    if (t->first == identifier)
      return EvalResult{CompileTimeValue::get(t->second.size()), true};
  }
  if (auto t = context.getExpandedTypePack())
    if (t->first == identifier)
      return EvalResult{CompileTimeValue::get(t->second.size()), true};
  return EvalResult{CompileTimeValue::getTemplateValue(BuiltinType::INT, "countof"), false};
}


VariablePackElement::VariablePackElement(CodeLoc l, string packName, unique_ptr<Expression> index)
    : Expression(l), packName(std::move(packName)), index(std::move(index)) {
}

WithErrorLine<SType> VariablePackElement::getTypeImpl(const Context& context) {
  auto indexValue1 = TRY(index->eval(context).addNoEvalError(
      index->codeLoc.getError("Unable to evaluate constant expression at compile-time")));
  auto indexValue = indexValue1.value->removeValueReference().dynamicCast<CompileTimeValue>();
  if (indexValue->getType() != BuiltinType::INT)
    return index->codeLoc.getError("Expected subscript index of type " + quote(BuiltinType::INT->getName())
        + ",  got " + quote(indexValue->getType()->getName()));
  const int intValue = indexValue->value.getValueMaybe<int>().value_or(0);
  auto getType = [&](const vector<SType>& types) -> WithErrorLine<SType> {
    if (intValue >= 0 && intValue < types.size())
      return types[intValue];
    return codeLoc.getError("Index out of range: " + to_string(intValue) + ". Expected value between 0 and "
        + to_string(types.size()));
  };
  if (auto pack = context.getExpandedVariablePack())
    if (pack->first == packName) {
      codegenName = getExpandedParamName(packName, intValue);
      return getType(pack->second);
    }
  if (auto pack = context.getUnexpandedVariablePack())
    if (pack->first == packName)
      return pack->second;
  return codeLoc.getError(quote(packName) + " is neither a type or variable pack name.");
}

unique_ptr<Expression> VariablePackElement::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return cast<Expression>(unique<VariablePackElement>(codeLoc, packName, index->transform(f1, f2)));
}

unique_ptr<MemberAccessExpression> MemberAccessExpression::getPointerAccess(CodeLoc l, unique_ptr<Expression> lhs, string id) {
  return unique<MemberAccessExpression>(l,
      unique<UnaryExpression>(l, Operator::POINTER_DEREFERENCE, std::move(lhs)),
      id);
}

MemberAccessExpression::MemberAccessExpression(CodeLoc l , unique_ptr<Expression> lhs, string id)
  : Expression(l), lhs(std::move(lhs)), identifier(id) { }

static JustError<ErrorLoc> initializeDestructor(const Context& context, const SType& type, const string& member,
    CodeLoc codeLoc, nullable<SFunctionInfo>& destructorCall, bool& mainDestructor) {
  if (auto structType = type.dynamicCast<StructType>()) {
    if (structType->destructor) {
      mainDestructor = true;
      destructorCall = TRY(getDestructor(context, structType, codeLoc));
      TRY(destructorCall->addInstance(context));
    } else
    if (structType->hasDestructor()) {
      for (int i = 0; i < structType->members.size(); ++i)
        if (structType->members[i].name == member) {
          destructorCall = TRY(getFunction(context, codeLoc, IdentifierInfo("destruct_except", codeLoc),
              {CompileTimeValue::get(i)}, {PointerType::get(type)}, {codeLoc}));
          TRY(destructorCall->addInstance(context));
        }
    }
  }
  return success;
}

WithErrorLine<SType> MemberAccessExpression::getTypeImpl(const Context& context) {
  auto leftType = TRY(lhs->getTypeImpl(context));
  if (auto res = leftType->getSizeError(context); !res)
    return codeLoc.getError(leftType->getName() + res.get_error());
  isUnion = leftType->removeReference()->getType() == BuiltinType::UNION_TYPE;
  auto ret = TRY(leftType->getTypeOfMember(identifier).addCodeLoc(codeLoc));
  TRY(initializeDestructor(context, leftType, identifier, codeLoc, destructorCall, isMainDestructor));
  return ret;
}

unique_ptr<Expression> MemberAccessExpression::transform(const StmtTransformFun& fun1, const ExprTransformFun& fun2) const {
  return unique<MemberAccessExpression>(codeLoc, lhs->transform(fun1, fun2), identifier);
}

void MemberAccessExpression::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(lhs.get());
}

void MemberAccessExpression::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  Expression::addFunctionCalls(fun);
  if (destructorCall)
    fun(destructorCall.get());
}

JustError<ErrorLoc> MemberAccessExpression::checkMoves(MoveChecker& checker) const {
  return lhs->checkMoves(checker);
}

TernaryExpression::TernaryExpression(CodeLoc l, unique_ptr<Expression> cond, unique_ptr<Expression> e1, unique_ptr<Expression> e2)
  : Expression(l), condExpr(std::move(cond)), e1(std::move(e1)), e2(std::move(e2)) {
}

WithErrorLine<SType> TernaryExpression::getTypeImpl(const Context& context) {
  auto condType = TRY(getType(context, condExpr));
  if (!context.canConvert(condType, BuiltinType::BOOL))
    return condExpr->codeLoc.getError("Expected expression of type " + quote(BuiltinType::BOOL->getName()) +
        ", got " + quote(condType->getName()));
  auto t1 = TRY(getType(context, e1));
  auto t2 = TRY(getType(context, e2));
  if (context.canConvert(t1, t2, e1))
    return t2;
  else
  if (context.canConvert(t2, t1, e2))
    return t1;
  else
    return e1->codeLoc.getError("Ternary operator operands have incompatible types: "
        + quote(t1->getName()) + " and " + quote(t2->getName()));
}

WithEvalError<EvalResult> TernaryExpression::eval(const Context& context) const {
  auto cond = TRY(condExpr->eval(context));
  if (auto value = cond.value.dynamicCast<CompileTimeValue>()) {
    auto res1 = TRY(e1->eval(context));
    auto res2 = TRY(e2->eval(context));
    if (auto boolValue = value->value.getValueMaybe<bool>())
      return *boolValue ? res1 : res2;
    return EvalResult{CompileTimeValue::getTemplateValue(res1.value->getType(), "ternary_result"), false};
  }
  return EvalError::noEval();
}

unique_ptr<Expression> TernaryExpression::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return unique<TernaryExpression>(codeLoc, f2(condExpr.get()), f2(e1.get()), f2(e2.get()));
}

void TernaryExpression::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(condExpr.get());
  f2(e1.get());
  f2(e2.get());
}

JustError<ErrorLoc> TernaryExpression::checkMoves(MoveChecker& checker) const {
  TRY(condExpr->checkMoves(checker));
  checker.startBlock();
  checker.newAlternative();
  TRY(e1->checkMoves(checker));
  checker.clearStatementUsages();
  checker.newAlternative();
  TRY(e2->checkMoves(checker));
  checker.endBlock();
  return success;
}

FatPointerConversion::FatPointerConversion(CodeLoc l, vector<SFunctionInfo> functions, SType toType, SType argType, unique_ptr<Expression> arg, shared_ptr<ConceptType> conceptType)
    : Expression(l), toType(toType), argType(argType), arg(std::move(arg)), conceptType(std::move(conceptType)), functions(std::move(functions)) {
}

WithError<vector<SFunctionInfo>> getRequiredFunctionsForConceptType(const Context& context,
    const Concept& concept, CodeLoc codeLoc) {
  vector<SFunctionInfo> ret;
  for (auto& fun : concept.getContext().getAllFunctions()) {
    auto candidates = context.getFunctions(translateDestructorId(fun->id)).filter(
        [](const SFunctionInfo& f) {
          return !f->isConceptTypeFunction();
        }
    );
    ret.push_back(TRY(getFunction(context, codeLoc, translateDestructorId(fun->id),
        std::move(candidates), {}, fun->type.params,
        vector<CodeLoc>(fun->type.params.size(), codeLoc))));
  }
  return ret;
}

WithErrorLine<SType> FatPointerConversion::getTypeImpl(const Context& context) {
  return toType;
}

unique_ptr<Expression> FatPointerConversion::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return f2(arg.get());
}

void FatPointerConversion::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f2(arg.get());
}

void FatPointerConversion::addFunctionCalls(const FunctionCallVisitFun& fun) const {
  for (auto& f : functions)
    fun(f);
}

void FatPointerConversion::addConceptTypes(ConceptsSet& types) const {
  Expression::addConceptTypes(types);
  types.insert(conceptType.get());
}

JustError<ErrorLoc> FatPointerConversion::checkMoves(MoveChecker& c) const {
  return arg->checkMoves(c);
}

UncheckedStatement::UncheckedStatement(CodeLoc l, unique_ptr<Statement> elem) : Statement(l), elem(std::move(elem)) {
}

bool UncheckedStatement::hasReturnStatement() const {
  return elem->hasReturnStatement();
}

JustError<ErrorLoc> UncheckedStatement::check(Context& context, bool) {
  if (!context.getTemplateParams())
    return elem->check(context);
  return success;
}

JustError<ErrorLoc> UncheckedStatement::checkMovesImpl(MoveChecker& checker) const {
  return elem->checkMovesImpl(checker);
}

unique_ptr<Statement> UncheckedStatement::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return unique<UncheckedStatement>(codeLoc, elem->transform(f1, f2));
}

void UncheckedStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f1(elem.get());
}

AttributeDefinition::AttributeDefinition(CodeLoc l, string name) : Statement(l), name(std::move(name)) {
}

JustError<ErrorLoc> AttributeDefinition::addToContext(Context& context) {
  TRY(context.checkNameConflictExcludingFunctions(name, "attribute").addCodeLoc(codeLoc));
  context.addType(name, AttributeType::get(name));
  return success;
}

JustError<ErrorLoc> AttributeDefinition::check(Context&, bool) {
  return success;
}

unique_ptr<Statement> AttributeDefinition::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<AttributeDefinition>(codeLoc, name);
}

ExternalStatement::ExternalStatement(Statement* s) : Statement(s->codeLoc), elem(s) {}

bool ExternalStatement::hasReturnStatement() const {
  return elem->hasReturnStatement();
}

JustError<ErrorLoc> ExternalStatement::check(Context& context, bool b) {
  return elem->check(context, b);
}

JustError<ErrorLoc> ExternalStatement::checkMovesImpl(MoveChecker& checker) const {
  return elem->checkMoves(checker);
}

unique_ptr<Statement> ExternalStatement::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return elem->transform(f1, f2);
}

void ExternalStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  f1(elem);
}

StatementExpression::StatementExpression(CodeLoc l, vector<unique_ptr<Statement>> s, unique_ptr<Expression> v)
    : Expression(l), statements(std::move(s)), value(std::move(v)) {
}

WithErrorLine<SType> StatementExpression::getTypeImpl(const Context& context) {
  auto bodyContext = context.getChild();
  for (auto& s : statements)
    TRY(s->check(bodyContext));
  return TRY(getType(bodyContext, value));
}

unique_ptr<Expression> StatementExpression::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return unique<StatementExpression>(codeLoc,
      statements.transform([&](auto& s) { return s->transform(f1, f2); }),
      value->transform(f1, f2));
}

void StatementExpression::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  for (auto& s : statements)
    f1(s.get());
  f2(value.get());
}

JustError<ErrorLoc> StatementExpression::checkMoves(MoveChecker& checker) const {
  checker.startBlock();
  OnExit onExit([&]{ checker.endBlock();});
  for (auto& s : statements)
    TRY(s->checkMoves(checker));
  TRY(value->checkMoves(checker));
  return success;
}

AST AST::clone() {
  return AST{ elems.transform([](auto& elem) { return elem->deepCopy(); } ) };
}

MixinStatement::MixinStatement(CodeLoc l, unique_ptr<Expression> value) : Statement(l), value(std::move(value)) {
}

JustError<ErrorLoc> MixinStatement::check(Context& context, bool) {
  if (context.getTemplateParams()) {
    returns = true;
    return success;
  }
  auto evalResult = TRY(value->eval(context)
      .addNoEvalError(value->codeLoc.getError("Unable to evaluate expression at compile-time"))).value;
  string str = *TRY(evalResult->convertTo(BuiltinType::STRING).addCodeLoc(value->codeLoc))
      .dynamicCast<CompileTimeValue>()->value.getReferenceMaybe<string>();
  istringstream iss("\"" + str + "\"");
  iss >> std::quoted(str);
  auto tokens = TRY(lex(str, value->codeLoc, "end of expression"));
  result = TRY(parseStatement(tokens, false));
  if (!result)
    return tokens.peek().codeLoc.getError("Expected a non-empty mixin parameter");
  if (!tokens.peek().contains<EofToken>())
    return tokens.peek().codeLoc.getError("Expected a single statement in mixin parameter");
  TRY(result->check(context, false));
  returns = result->hasReturnStatement();
  return success;
}


unique_ptr<Statement> MixinStatement::transform(const StmtTransformFun&, const ExprTransformFun& f) const {
  return unique<MixinStatement>(codeLoc, f(value.get()));
}

bool MixinStatement::hasReturnStatement() const {
  return returns;
}

JustError<ErrorLoc> MixinStatement::checkMovesImpl(MoveChecker& checker) const {
  if (result)
    return result->checkMoves(checker);
  return success;
}

void MixinStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  if (result)
    result->visit(f1, f2);
}

StaticStatement::StaticStatement(CodeLoc l, unique_ptr<Statement> value) : Statement(l), value(std::move(value)) {
}

JustError<ErrorLoc> StaticStatement::check(Context& context, bool) {
  results = TRY(value->eval(context)
      .addNoEvalError(value->codeLoc.getError("Unable to evaluate statement at compile-time")));
  return success;
}

unique_ptr<Statement> StaticStatement::transform(const StmtTransformFun& f, const ExprTransformFun&) const {
  return unique<StaticStatement>(codeLoc, f(value.get()));
}

bool StaticStatement::hasReturnStatement() const {
  for (auto& s : results)
    if (s->hasReturnStatement())
      return true;
  return false;
}

JustError<ErrorLoc> StaticStatement::checkMovesImpl(MoveChecker& checker) const {
  return value->checkMovesImpl(checker);
}

void StaticStatement::visit(const StmtVisitFun& f1, const ExprVisitFun& f2) const {
  for (auto& s : results)
    f1(s.get());
}

unique_ptr<Statement> getRangedLoop(CodeLoc l, string iterator, unique_ptr<Expression> container,
    unique_ptr<Statement> body) {
  auto ret = unique<StatementBlock>(l);
  static int cnt = 0;
  ++cnt;
  auto containerId = "container" + to_string(cnt);
  auto itEndId = "itEnd" + to_string(cnt);
  ret->elems.push_back(unique<AliasDeclaration>(l, containerId, std::move(container)));
  auto itDecl = unique<VariableDeclaration>(l, none, iterator,
      unique<FunctionCall>(IdentifierInfo("begin", l), unique<Variable>(IdentifierInfo(containerId, l)), true));
  itDecl->isMutable = true;
  ret->elems.push_back(std::move(itDecl));
  ret->elems.push_back(unique<VariableDeclaration>(l, none, itEndId,
      unique<FunctionCall>(IdentifierInfo("end", l), unique<Variable>(IdentifierInfo(containerId, l)), true)));
  auto whileBody = unique<StatementBlock>(l);
  whileBody->elems.push_back(std::move(body));
  auto incExpr = unique<ExpressionStatement>(unique<UnaryExpression>(l, Operator::INCREMENT,
      unique<Variable>(IdentifierInfo(iterator, l))));
  incExpr->canDiscard = true;
  whileBody->elems.push_back(std::move(incExpr));
  ret->elems.push_back(unique<WhileLoopStatement>(l, BinaryExpression::get(l, Operator::NOT_EQUAL,
      unique<Variable>(IdentifierInfo(iterator, l)), unique<Variable>(IdentifierInfo(itEndId, l))),
      std::move(whileBody)));
  return ret;
}
