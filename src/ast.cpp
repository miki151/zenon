#include "stdafx.h"
#include "ast.h"
#include "type.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"
#include "code_loc.h"
#include "identifier_type.h"
#include "type_registry.h"
#include "move_checker.h"

Node::Node(CodeLoc l) : codeLoc(l) {}

unique_ptr<Expression> Expression::replaceVar(string from, string to) const {
  return transform(
      [&](Statement* expr) { return expr->replaceVar(from, to); },
      [&](Expression* expr) { return expr->replaceVar(from, to); });
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

Constant::Constant(CodeLoc l, SCompileTimeValue v) : Expression(l), value(v) {
  INFO << "Created constant " << v->getName() << " of type " << v->getType();
}

Variable::Variable(IdentifierInfo s) : Expression(s.codeLoc), identifier(std::move(s)) {
}

FunctionCall::FunctionCall(CodeLoc l, IdentifierInfo id, bool methodCall) : Expression(l), identifier(std::move(id)),
    methodCall(methodCall), variadicTemplateArgs(identifier->parts.back().variadic) {
  INFO << "Function call " << id.prettyString();;
}

FunctionCall::FunctionCall(CodeLoc l, IdentifierInfo id, unique_ptr<Expression> arg, bool methodCall) : FunctionCall(l, id, methodCall) {
  arguments.push_back(std::move(arg));
}

unique_ptr<FunctionCall> FunctionCall::constructor(CodeLoc l, SType type) {
  auto ret = unique<FunctionCall>(l, false, Private{});
  vector<SType> templateArgs;
  if (auto structType = type.dynamicCast<StructType>()) {
    templateArgs = structType->templateParams;
    type = structType->parent.get();
  }
  ret->templateArgs = std::move(templateArgs);
  ret->identifierType = IdentifierType(type);
  return ret;
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
  if (auto t = context.getTypeFromString(identifier))
    return t.get()->getType();
  return codeLoc.getError(varError.value_or("Identifier not found: " + identifier.prettyString()));
}

WithEvalError<EvalResult> Variable::eval(const Context& context) const {
  auto res = context.getTypeFromString(identifier);
  if (res) {
    auto value = res.get().dynamicCast<CompileTimeValue>();
    bool isConstant = !value || !value->value.contains<CompileTimeValue::ReferenceValue>();
    return EvalResult { res.get(), isConstant};
  }
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

template <typename Comp>
static bool exactArgs(const vector<SType>& argTypes, const FunctionType& f, Comp comp) {
  if (f.params.size() != argTypes.size() || !f.templateParams.empty())
    return false;
  for (int i = 0; i < f.params.size(); ++i)
    if (!comp(argTypes[i], f.params[i]))
      return false;
  return true;
}

template <typename Comp>
static bool exactFirstArg(const vector<SType>& argTypes, const FunctionType& overload, Comp comp) {
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
      if (other != overload && context.isGeneralization(overload->getParent(), other->getParent()))
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
    CodeLoc, IdentifierType, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>&);

static WithErrorLine<SFunctionInfo> getFunction(const Context&,
    CodeLoc, IdentifierType, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>&, vector<unique_ptr<Expression>>&);

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr) {
  switch (op) {
    case Operator::NOT_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::EQUALS, std::move(expr)));
    case Operator::LESS_OR_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::MORE_THAN, std::move(expr)));
    case Operator::MORE_OR_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::LESS_THAN, std::move(expr)));
    default:
      return unique<BinaryExpression>(Private{}, loc, op, std::move(expr));
  }
}

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, unique_ptr<Expression> a, unique_ptr<Expression> b) {
  return get(loc, op, makeVec<unique_ptr<Expression>>(std::move(a), std::move(b)));
}

BinaryExpression::BinaryExpression(BinaryExpression::Private, CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr)
    : Expression(loc), op(op), expr(std::move(expr)) {}

static vector<unique_ptr<Expression>> transformValueOrArg(vector<unique_ptr<Expression>> expr, CodeLoc codeLoc, bool usePointer) {
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
      (unique<FunctionCall>(codeLoc, IdentifierInfo("destruct_and_get", codeLoc),
          unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false)));
}

static unique_ptr<Expression> getDestructorCall(CodeLoc codeLoc, unique_ptr<Expression> expr) {
  return unique<FunctionCall>(codeLoc, IdentifierInfo("destruct", codeLoc),
      unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false);
}

static unique_ptr<Statement> getDestructorStatement(CodeLoc codeLoc, const string& identifier) {
  return unique<ExpressionStatement>(getDestructorCall(codeLoc, unique<Variable>(IdentifierInfo(identifier, codeLoc))));
}

WithErrorLine<SType> BinaryExpression::getTypeImpl(const Context& context) {
  switch (op) {
    case Operator::NOT_EQUAL:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
      FATAL << "This operator should have been rewritten";
      fail();
    default: {
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
  }
}

WithErrorLine<SFunctionInfo> getDestructor(const Context& context, const SType& type) {
  return getFunction(context, CodeLoc(), "destruct"s, {}, {PointerType::get(type)}, {CodeLoc()});
}

JustError<ErrorLoc> BinaryExpression::considerDestructorCall(const Context& context, int index, const SType& argType) {
  if (argType->hasDestructor() && !argType->isReference() && functionInfo->type.params[index].dynamicCast<ReferenceType>()) {
    destructorCall[index] = TRY(getDestructor(context, argType));
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
    destructorCall = TRY(getFunction(context, codeLoc, "destruct"s, {}, {PointerType::get(right)}, {codeLoc}));
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

JustError<ErrorLoc> UnaryExpression::checkMoves(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

JustError<ErrorLoc> StatementBlock::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
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

JustError<ErrorLoc> IfStatement::check(Context& context, bool) {
  auto ifContext = Context::withParent(context);
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
  TRY(ifTrue->check(ifContext));
  if (ifFalse)
    TRY(ifFalse->check(ifContext));
  return success;
}

JustError<ErrorLoc> IfStatement::checkMovesImpl(MoveChecker& checker) const {
  if (declaration)
    TRY(declaration->checkMoves(checker));
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

static JustError<string> getVariableInitializationError(const char* action, const Context& context, const SType& varType,
    const SType& exprType, unique_ptr<Expression>& expr) {
  if (!context.canConvert(exprType, varType, expr))
    return "Can't "s + action + " of type "
       + quote(varType->getName()) + " using a value of type " + quote(exprType->getName());
  return success;
}

JustError<ErrorLoc> VariableDeclaration::check(Context& context, bool) {
  TRY(context.checkNameConflictExcludingFunctions(identifier, "Variable").addCodeLoc(codeLoc));
  if (!realType) {
    if (type)
      realType = TRY(context.getTypeFromString(*type));
    else
    if (initExpr)
      realType = TRY(getType(context, initExpr))->removeReference();
    else
      return codeLoc.getError("Initializing expression needed to infer variable type");
  }
  if (!realType->canDeclareVariable())
    return codeLoc.getError("Can't declare variable of type " + quote(realType->getName()));
  TRY(realType.get()->getSizeError(context).addCodeLoc(codeLoc));
  INFO << "Adding variable " << identifier << " of type " << realType.get()->getName();
  if (!initExpr)
    return codeLoc.getError("Variable requires initialization");
  auto exprType = TRY(getType(context, initExpr));
  if (isStatic) {
    if (isMutable)
      return codeLoc.getError("Static mutable variables are not supported");
    context.addType(identifier, TRY(initExpr->eval(context)
        .addNoEvalError(initExpr->codeLoc.getError("Unable to evaluate expression at compile-time"))).value);
  }
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

unique_ptr<Statement> VariableDeclaration::transform(const StmtTransformFun&,
    const ExprTransformFun& exprFun) const {
  auto ret = unique<VariableDeclaration>(codeLoc, none, identifier,
      initExpr ? exprFun(initExpr.get()) : nullptr);
  ret->isMutable = isMutable;
  ret->isStatic = isStatic;
  return ret;
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

JustError<ErrorLoc> Statement::checkMovesImpl(MoveChecker&) const {
  return success;
}

bool Statement::hasReturnStatement(const Context&) const {
  return false;
}

unique_ptr<Statement> Statement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  fail();
}

unique_ptr<Statement> Statement::deepCopy() const {
  return transform(&identityStmt, &identityExpr);
}

bool IfStatement::hasReturnStatement(const Context& context) const {
  return ifTrue->hasReturnStatement(context) && ifFalse && ifFalse->hasReturnStatement(context);
}

StatementBlock::StatementBlock(CodeLoc l, vector<unique_ptr<Statement>> e) : Statement(l), elems(std::move(e)) {
}

bool StatementBlock::hasReturnStatement(const Context& context) const {
  for (auto& s : elems)
    if (s->hasReturnStatement(context))
      return true;
  return false;
}

ReturnStatement::ReturnStatement(CodeLoc codeLoc, unique_ptr<Expression> expr)
    : Statement(codeLoc), expr(std::move(expr)) {}

bool ReturnStatement::hasReturnStatement(const Context&) const {
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
  auto paramsContext = Context::withParent(context);
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

JustError<ErrorLoc> FunctionDefinition::setFunctionType(const Context& context, nullable<SConcept> concept, bool builtInImport) {
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
  Context contextWithTemplateParams = Context::withParent(context);
  auto templateTypes = TRY(getTemplateParams(templateInfo, context));
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    auto& param = templateInfo.params[i];
    contextWithTemplateParams.addType(param.name, templateTypes[i], true);
    if (i == templateInfo.params.size() - 1 && templateInfo.variadic)
      contextWithTemplateParams.addUnexpandedTypePack(param.name, templateTypes[i]);
  }
  auto requirements = TRY(applyRequirements(contextWithTemplateParams, templateInfo));
  definitionContext.emplace(Context::withParent(contextWithTemplateParams));
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
  FunctionType functionType(returnType, params, templateTypes);
  functionType.concept = concept;
  functionType.requirements = requirements;
  functionType.variadicTemplate = templateInfo.variadic;
  functionType.variadicParams = isVariadicParams;
  if (name.contains<ConstructorTag>() && external)
    functionType.generatedConstructor = true;
  if (external)
    functionType.setBuiltin();
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

static IdentifierType translateDestructorId(const IdentifierType& id) {
  if (id.isSimpleString("destruct"))
    return IdentifierType("destruct_full");
  if (id.isSimpleString("destruct_impl_dont_call"))
    return IdentifierType("destruct");
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
          return s->destructor.get();
        }
      if (auto s = argTypes[0]->removePointer().dynamicCast<LambdaType>())
        if (s->destructor)
          return FunctionInfo::getImplicit("destruct"s, FunctionType(BuiltinType::VOID, {PointerType::get(s)}, {}));
    }
  } else
  if (id == "invoke"s && !argTypes.empty() && argTypes[0].dynamicCast<PointerType>())
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
    CodeLoc codeLoc, IdentifierType id, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc) {
  auto candidates = context.getFunctionTemplate(translateDestructorId(id));
  auto error =  codeLoc.getError("Couldn't find function " + id.prettyString() +
      " matching arguments: (" + joinTypeList(argTypes) + ")");
  if (candidates.empty())
    return error;
  auto functionId = candidates[0]->id;
  if (auto res = getFunction(context, codeLoc, functionId, std::move(candidates), std::move(templateArgs), argTypes, argLoc))
    return *res;
  else {
    error.error += res.get_error();
    return error;
  }
}

static WithErrorLine<SFunctionInfo> getFunction(const Context& context,
    CodeLoc codeLoc, IdentifierType id, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc, vector<unique_ptr<Expression>>& expr) {
  auto fun = TRY(getFunction(context, codeLoc, std::move(id), std::move(templateArgs), std::move(argTypes), std::move(argLoc)));
  generateConversions(context, fun->type.params, argTypes, expr);
  return fun;
}

WithErrorLine<SFunctionInfo> getCopyFunction(const Context& context, CodeLoc callLoc, const SType& t) {
  return getFunction(context, callLoc, IdentifierType("copy"), {}, {PointerType::get(t)}, {callLoc});
}

WithErrorLine<SFunctionInfo> getImplicitCopyFunction(const Context& context, CodeLoc callLoc, const SType& t) {
  return getFunction(context, callLoc, IdentifierType("implicit_copy"), {}, {PointerType::get(t)}, {callLoc});
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualFunctionCallExpr(const Context& context,
    const string& funName, const string& alternativeName, const SType& alternativeType, int virtualIndex) {
  auto functionCall = unique<FunctionCall>(codeLoc, IdentifierInfo(funName, codeLoc), false);
  vector<SType> args;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      args.push_back(functionInfo->type.params[i]);
    } else {
      functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
      args.push_back(alternativeType);
    }
  TRY(getFunction(context, codeLoc, IdentifierType(funName), {}, args,
      vector<CodeLoc>(args.size(), codeLoc)));
  return unique_ptr<Expression>(std::move(functionCall));
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualOperatorCallExpr(Context& context,
    Operator op, const string& alternativeName, const SType& alternativeType, int virtualIndex) {
  vector<unique_ptr<Expression>> arguments;
  vector<SType> argTypes;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      argTypes.push_back(functionInfo->type.params[i]);
    } else {
      arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
      argTypes.push_back(alternativeType);
      if (i == 0 && (alternativeType.dynamicCast<PointerType>() || alternativeType.dynamicCast<MutablePointerType>())) {
        arguments.back() = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(arguments.back()));
        argTypes.back() = argTypes.back()->removePointer();
      }
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
  unique_ptr<Expression> switchExpr = unique<MoveExpression>(codeLoc, *virtualParam.name);
  body = unique<StatementBlock>(codeLoc);
  auto unionType = virtualType->dynamicCast<StructType>();
  bool isPointerParam = false;
  if (!unionType) {
    unionType = virtualType.get()->removePointer().dynamicCast<StructType>();
    isPointerParam = true;
  }
  if (!unionType || unionType->alternatives.empty())
    return codeLoc.getError("Virtual parameter must be of a union type or a pointer to one");
  auto switchStatementPtr = unique<SwitchStatement>(codeLoc, std::move(switchExpr));
  auto& switchStatement = *switchStatementPtr;
  body->elems.push_back(std::move(switchStatementPtr));
  for (auto& alternative : unionType->alternatives) {
    auto alternativeType = alternative.type;
    if (virtualType->dynamicCast<MutablePointerType>())
      alternativeType = MutablePointerType::get(std::move(alternativeType));
    else if (virtualType->dynamicCast<PointerType>())
      alternativeType = PointerType::get(std::move(alternativeType));
    WithErrorLine<unique_ptr<Expression>> call = [&] {
      if (auto regularName = name.getReferenceMaybe<string>())
        return getVirtualFunctionCallExpr(bodyContext, *regularName, alternative.name, alternativeType, virtualIndex);
      else if (auto op = name.getValueMaybe<Operator>())
        return getVirtualOperatorCallExpr(bodyContext, *op, alternative.name, alternativeType, virtualIndex);
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
          none,
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
      auto call = unique<FunctionCall>(codeLoc, returnType, false);
      for (auto elem : structType->members) {
        auto copiedParam = unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc));
        auto copyCall = unique<FunctionCall>(codeLoc, IdentifierInfo(functionName, codeLoc), false);
        copyCall->arguments.push_back(unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
            MemberAccessExpression::getPointerAccess(codeLoc, std::move(copiedParam), elem.name)));
        call->arguments.push_back(std::move(copyCall));
      }
      body->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(call)));
    } else {
      auto copiedParam = unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc));
      auto topSwitch = unique<SwitchStatement>(codeLoc, std::move(copiedParam));
      for (auto& alternative : structType->alternatives) {
        auto block = unique<StatementBlock>(codeLoc);
        auto constructorName = returnType;
        constructorName.parts.push_back(IdentifierInfo::IdentifierPart { alternative.name, {} });
        auto constructorCall = unique<FunctionCall>(codeLoc, constructorName, false);
        if (alternative.type != BuiltinType::VOID)
          constructorCall->arguments.push_back(unique<FunctionCall>(codeLoc, IdentifierInfo(functionName, codeLoc),
              unique<Variable>(IdentifierInfo(alternative.name, codeLoc)), false));
        block->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(constructorCall)));
        topSwitch->caseElems.push_back(
            SwitchStatement::CaseElem {
              codeLoc,
              none,
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
    auto call = unique<FunctionCall>(codeLoc, std::move(id), false);
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

JustError<ErrorLoc> FunctionDefinition::addInstance(const Context* callContext, const SFunctionInfo& instance) {
  if (callContext && callContext->getTemplateParams())
    return success;
  wasUsed = true;
  vector<SFunctionInfo> requirements;
  for (auto& req : instance->type.requirements)
    if (auto concept = req.base.getValueMaybe<SConcept>())
      append(requirements, TRY((callContext ? callContext : &*definitionContext)
          ->getRequiredFunctions(**concept, {}).addCodeLoc(codeLoc)));
  for (auto& fun : requirements)
    if (!!fun->type.concept)
      return success;
  if (instance != functionInfo.get()) {
    if (body) {
      CHECK(functionInfo == instance->getParent());
      for (auto& other : instances)
        if (other.functionInfo->getWithoutRequirements() == instance->getWithoutRequirements())
          return success;
      int instanceIndex = instances.size();
      instances.push_back(InstanceInfo{unique_ptr<StatementBlock>(), {},
          instance, requirements});
      if (wasChecked) {
        instances[instanceIndex].body = cast<StatementBlock>(origBody->deepCopy());
        vector<unique_ptr<Statement>> destructorCallsTmp;
        auto ret = checkBody(requirements, *instances[instanceIndex].body,
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
      if (variadic && i == params.size() - 1)
        context.addUnexpandedTypePack(param->getName(), param);
    }
  }
}

JustError<ErrorLoc> FunctionDefinition::generateDefaultBodies(Context& context) {
  Context bodyContext = Context::withParent(context);
  addTemplateParams(bodyContext, functionInfo->type.templateParams, functionInfo->type.variadicTemplate);
  auto res = TRY(applyRequirements(bodyContext, templateInfo));
  if (isVirtual)
    TRY(generateVirtualDispatchBody(bodyContext));
  if (name == "copy"s || name == "implicit_copy"s)
    TRY(checkAndGenerateCopyFunction(bodyContext, name.get<string>()));
  else
  if (name == ConstructorTag{})
    TRY(checkAndGenerateDefaultConstructor(bodyContext));
  else
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
  context.setTemplated(templateParams);
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

JustError<ErrorLoc> FunctionDefinition::checkBody(const vector<SFunctionInfo>& requirements,
    StatementBlock& myBody, const FunctionInfo& instanceInfo, vector<unique_ptr<Statement>>& destructorCalls) const {
  auto bodyContext = Context::withParent(*definitionContext);
  for (auto& f : requirements) {
    if (!contains(bodyContext.getFunctions(f->id), f->getParent()))
      CHECK(bodyContext.addFunction(f));
  }
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
  if (templateParams.empty()) {
    vector<string> paramNames;
    for (int i = 0; i < instanceInfo.type.params.size(); ++i)
      paramNames.push_back(*instanceInfo.getParamName(i, this));
    destructorCalls = getDestructorCalls(codeLoc, std::move(paramNames), instanceInfo.type.params);
    for (auto& elem : destructorCalls)
      if (elem)
        TRY(elem->check(bodyContext));
  }
  if (retVal == BuiltinType::NORETURN && !myBody.hasReturnStatement(bodyContext))
    return codeLoc.getError("This function should never return");
  if (retVal != BuiltinType::VOID && !myBody.hasReturnStatement(bodyContext))
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
  if (body && (!templateInfo.params.empty() || notInImport)) {
    origBody = cast<StatementBlock>(body->deepCopy());
    Context paramsContext = Context::withParent(context);
    addTemplateParams(paramsContext, functionInfo->type.templateParams, functionInfo->type.variadicTemplate);
    auto res = TRY(applyRequirements(paramsContext, templateInfo));
    TRY(checkForIncompleteTypes(paramsContext));
    // For checking the template we use the Context that includes the template params.
    definitionContext.emplace(Context::withParent(paramsContext));
    TRY(checkBody({}, *body, *functionInfo, destructorCalls));
    // For checking instances we just use the top level context.
    definitionContext.emplace(Context::withParent(context));
    wasChecked = true;
    for (int i = 0; i < instances.size(); ++i)
      if (!instances[i].body) {
        instances[i].body = cast<StatementBlock>(origBody->deepCopy());
        TRY(checkBody(instances[i].requirements, *instances[i].body,
            *instances[i].functionInfo, instances[i].destructorCalls));
      }
    MoveChecker moveChecker;
    for (auto& p : parameters)
      if (p.name)
        moveChecker.addVariable(*p.name);
    if (name == "destruct"s) {
      auto destructedType = TRY(getDestructedType(functionInfo->type.params).addCodeLoc(codeLoc));
      if (auto structType = destructedType.dynamicCast<StructType>())
        if (!structType->definition || structType->definition->file != codeLoc.file)
          return codeLoc.getError("Destructor function must be defined in the same file as the destructed type");
    }
    return body->checkMoves(moveChecker);
  }
  return success;
}

JustError<ErrorLoc> FunctionDefinition::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  TRY(setFunctionType(context, nullptr, cache.isCurrentlyBuiltIn()));
  TRY(context.addFunction(functionInfo.get()).addCodeLoc(codeLoc));
  if (name == "destruct"s) {
    auto destructedType = TRY(getDestructedType(functionInfo->type.params).addCodeLoc(codeLoc));
    if (auto structType = destructedType.dynamicCast<StructType>()) {
      if (!structType->alternatives.empty())
        return codeLoc.getError("User-defined destructors for union types are not supported");
      if (structType->destructor)
        return codeLoc.getError("Destructor function for type " + quote(destructedType->getName()) + " already defined here: "
            + structType->destructor->getDefinition()->codeLoc.toString());
      auto adjusted = functionInfo.get();
      if (functionInfo->type.templateParams.size() != structType->templateParams.size())
        return codeLoc.getError("Number of template parameters of destructor function must match destructed type");
      for (auto& param : functionInfo->type.templateParams)
        if (!structType->templateParams.contains(param))
          return codeLoc.getError("Template parameters of destructor function must match destructed type");
      ErrorBuffer errors;
      for (int i = 0; i < structType->parent->templateParams.size(); ++i)
        adjusted = replaceInFunction(adjusted, functionInfo->type.templateParams[i], structType->parent->templateParams[i], errors);
      CHECK(errors.empty());
      structType->parent->destructor = adjusted;
    } else
    if (!cache.isCurrentlyBuiltIn())
      return codeLoc.getError("User-defined destructor is allowed only for struct types");
  }
  return success;
}

static void addBuiltInConcepts(Context& context) {
  auto addType = [&context](const char* name, SType type) {
    shared_ptr<Concept> concept = shared<Concept>(name, nullptr, Context(context.typeRegistry), false);
    concept->modParams().push_back(shared<TemplateParameterType>(type, "T", CodeLoc()));
    context.addConcept(name, concept);
  };
  addType("is_enum", BuiltinType::ENUM_TYPE);
  addType("is_struct", BuiltinType::STRUCT_TYPE);
  addType("is_union", BuiltinType::UNION_TYPE);
}

Context createPrimaryContext(TypeRegistry* typeRegistry) {
  Context context(typeRegistry);
  for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE, BuiltinType::BOOL,
       BuiltinType::VOID, BuiltinType::CHAR, BuiltinType::STRING, BuiltinType::NULL_TYPE})
    context.addType(type->getName(), type);
  CHECK(context.addImplicitFunction(Operator::PLUS, FunctionType(BuiltinType::STRING,
      {{BuiltinType::STRING}, {BuiltinType::STRING}}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS_UNARY, Operator::MINUS_UNARY})
    for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionType(type, {{type}}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT, Operator::DECREMENT})
    CHECK(context.addImplicitFunction(op, FunctionType(BuiltinType::VOID,
        {{MutableReferenceType::get(BuiltinType::INT)}}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS, Operator::MINUS, Operator::MULTIPLY, Operator::DIVIDE, Operator::MODULO})
    for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE})
      if (type != BuiltinType::DOUBLE || op != Operator::MODULO)
        CHECK(context.addImplicitFunction(op, FunctionType(type, {{type}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT_BY, Operator::DECREMENT_BY, Operator::MULTIPLY_BY, Operator::DIVIDE_BY})
    for (auto type : {BuiltinType::INT, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionType(BuiltinType::VOID,
          {{MutableReferenceType::get(type)}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::LOGICAL_AND, Operator::LOGICAL_OR})
    CHECK(context.addImplicitFunction(op, FunctionType(BuiltinType::BOOL,
        {{BuiltinType::BOOL}, {BuiltinType::BOOL}}, {}).setBuiltin()));
  CHECK(context.addImplicitFunction(Operator::LOGICAL_NOT, FunctionType(BuiltinType::BOOL,
      {{BuiltinType::BOOL}}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS, Operator::LESS_THAN, Operator::MORE_THAN})
    for (auto type : {BuiltinType::INT, BuiltinType::STRING, BuiltinType::DOUBLE})
      CHECK(context.addImplicitFunction(op, FunctionType(BuiltinType::BOOL, {{type}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS})
    for (auto type : {BuiltinType::BOOL, BuiltinType::CHAR})
      CHECK(context.addImplicitFunction(op, FunctionType(BuiltinType::BOOL, {{type}, {type}}, {}).setBuiltin()));
  auto metaTypes = {BuiltinType::ANY_TYPE, BuiltinType::STRUCT_TYPE, BuiltinType::ENUM_TYPE, BuiltinType::UNION_TYPE};
  CHECK(context.addImplicitFunction(Operator::EQUALS, FunctionType(BuiltinType::BOOL, {{BuiltinType::ANY_TYPE}, {BuiltinType::ANY_TYPE}}, {}).setBuiltin()));
  addBuiltInConcepts(context);
  context.addBuiltInFunction("enum_count", BuiltinType::INT, {SType(BuiltinType::ENUM_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto enumType = args[0].dynamicCast<EnumType>())
          return (SType) CompileTimeValue::get((int) enumType->elements.size());
        else
          fail();
      });
  context.addBuiltInFunction("struct_count", BuiltinType::INT, {SType(BuiltinType::STRUCT_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          return (SType) CompileTimeValue::get((int) structType->members.size());
        else
          fail();
      });
  context.addBuiltInFunction("union_count", BuiltinType::INT, {SType(BuiltinType::UNION_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          return (SType) CompileTimeValue::get((int) structType->alternatives.size());
        else
          fail();
      });
  context.addBuiltInFunction("get_name", BuiltinType::STRING, {SType(BuiltinType::ANY_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        return (SType) CompileTimeValue::get(args[0]->getName());
      });
  context.addBuiltInFunction("get_member_name", BuiltinType::STRING, {SType(BuiltinType::STRUCT_TYPE), SType(BuiltinType::INT)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          if (auto value = args[1].dynamicCast<CompileTimeValue>())
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->members.size())
                return "Struct member index out of range: "s + to_string(*intValue);
              return (SType) CompileTimeValue::get(structType->members[*intValue].name);
            }
        fail();
      });
  context.addBuiltInFunction("get_alternative_name", BuiltinType::STRING, {SType(BuiltinType::UNION_TYPE), SType(BuiltinType::INT)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          if (auto value = args[1].dynamicCast<CompileTimeValue>())
            if (auto intValue = value->value.getReferenceMaybe<int>()) {
              if (*intValue < 0 || *intValue >= structType->alternatives.size())
                return "Union member index out of range: "s + to_string(*intValue);
              return (SType) CompileTimeValue::get(structType->alternatives[*intValue].name);
            }
        fail();
      });
  context.addBuiltInFunction("string_length", BuiltinType::INT, {SType(BuiltinType::STRING)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto value = args[0].dynamicCast<CompileTimeValue>())
          if (auto s = value->value.getReferenceMaybe<string>())
            return (SType) CompileTimeValue::get((int) s->size());
        fail();
      });
  context.addBuiltInFunction("enum_strings", ArrayType::get(BuiltinType::STRING, CompileTimeValue::get(0)),
          {SType(BuiltinType::ENUM_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        auto enumType = args[0].dynamicCast<EnumType>();
        vector<SCompileTimeValue> values;
        for (auto& elem : enumType->elements)
          values.push_back(CompileTimeValue::get(elem));
        return (SType) CompileTimeValue::get(CompileTimeValue::ArrayValue{values, BuiltinType::STRING});
      });
  return context;
}

static void addBuiltInImport(AST& ast) {
  auto tmpVec = std::move(ast.elems);
  ast.elems = makeVec<unique_ptr<Statement>>(
      unique<ImportStatement>(CodeLoc{}, "std/builtin.znn", true)
  );
  for (auto& elem : ast.elems)
    elem->exported = true;
  for (auto& elem : tmpVec)
    ast.elems.push_back(std::move(elem));
}

static JustError<ErrorLoc> addExportedContext(const Context& primaryContext, ImportCache& cache, AST& ast,
    const string& path, bool isBuiltIn, const vector<string>& importDirs) {
  INFO << "Parsing import " << path;
  cache.pushCurrentImport(path, isBuiltIn);
  if (!isBuiltIn && !cache.isCurrentlyBuiltIn())
    addBuiltInImport(ast);
  auto importContext = Context::withParent(primaryContext);
  for (auto& elem : ast.elems) {
    if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
      import->setImportDirs(importDirs);
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

WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST& ast, Context& context, const Context& primaryContext,
    const vector<string>& importPaths, bool isBuiltInModule) {
  ImportCache cache(isBuiltInModule);
  TRY(addExportedContext(primaryContext, cache, ast, path, isBuiltInModule, importPaths));
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
  auto res = TRY(getType(context, expr));
  noReturnExpr = res == BuiltinType::NORETURN;
  if (!canDiscard && res != BuiltinType::VOID && res != BuiltinType::NORETURN)
    return codeLoc.getError("Expression result of type " + quote(res->getName()) + " discarded");
  if (canDiscard) {
    if (res == BuiltinType::VOID || res == BuiltinType::NORETURN)
      return codeLoc.getError("Void expression result unnecessarily marked as discarded");
    expr = unique<FunctionCall>(codeLoc, IdentifierInfo("discard_impl", codeLoc), std::move(expr), false);
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

JustError<ErrorLoc> ExpressionStatement::checkMovesImpl(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

bool ExpressionStatement::hasReturnStatement(const Context& context) const {
  return noReturnExpr;
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

JustError<ErrorLoc> FunctionCall::initializeTemplateArgsAndIdentifierType(const Context& context) {
  if (!templateArgs)
    templateArgs = TRY(context.getTypeList(identifier->parts.back().templateArguments, variadicTemplateArgs));
  if (!identifierType)
      identifierType = TRY(context.getIdentifierType(*identifier).addCodeLoc(identifier->codeLoc));
  return success;
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
  nullable<SType> returnType;
  vector<SType> expanded;
  vector<string> expandedVars;
  unordered_set<FunctionInfo*> allCalls;
  while (true) {
    auto call = cast<FunctionCall>(deepCopy());
    auto context = Context::withParent(callContext);
    if (variadicTemplateCall)
      context.addExpandedTypePack(templateArgs->back()->getName(), expanded);
    if (variadicCall)
      context.addExpandedVariablePack(callContext.getUnexpandedVariablePack()->first, expanded);
    for (auto& e : expanded)
    for (auto& fun : callContext.getAllFunctions())
      if (!!fun->type.concept) {
        ErrorBuffer errors;
        auto replaced = fun;
        auto from = callContext.getUnexpandedTypePack()->second;
//          std::cout << "Replacing in " << replaced->prettyString() << " from " << from->getName() << " to " << e->getName() << std::endl;
          replaced = replaceInFunction(replaced, from, e, errors);
        if (errors.empty() && replaced != fun)
          ignore(context.addFunction(replaced));
        CHECK(errors.empty()); // not sure if any errors should appear here, so checking just in case
      }
    auto type = TRY(call->getTypeImpl(context));
    if (!!returnType && returnType != type)
      return codeLoc.getError("Return type mismatch between called functions in variadic function call");
    returnType = type;
    expanded.push_back(shared<TemplateParameterType>(getExpandedParamName(
        callContext.getUnexpandedTypePack()->second->getName(), expanded.size()), codeLoc));
    //std::cout << "Calling " << call->functionInfo->prettyString() << std::endl;
    auto target = call->functionInfo->getParent();
    if (allCalls.count(target.get()))
      break;
    allCalls.insert(target.get());
  }
  return success;
}

/*// Check if we found a function that we can call with any arguments / template arguments expansion.
// The condition might have to be improved if some corner cases fail
expanded.push_back(shared<TemplateParameterType>(getExpandedParamName(
    callContext.getUnexpandedTypePack()->getName(), expanded.size()), codeLoc));
// Go through all functions coming from a concept and try to expand them from the current type pack into 'expanded'.
// Otherwise one of the types in 'expanded' might fail a requirement in the checked call
if (variablePack) {
  expandedVars.push_back(getExpandedParamName("SomeVar", expanded.size()));
  ErrorBuffer errors;
  context.addVariable(expandedVars.back(),
      variablePack->type->replace(callContext.getUnexpandedTypePack().get(), expanded.back(), errors), codeLoc);
  if (!errors.empty())
    return codeLoc.getError(errors[0]);
}*/

WithErrorLine<SType> FunctionCall::getTypeImpl(const Context& callContext) {
  TRY(initializeTemplateArgsAndIdentifierType(callContext));
  optional<ErrorLoc> error;
  if (!functionInfo) {
    vector<SType> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = 0; i < arguments.size(); ++i) {
      auto context = Context::withParent(callContext);
      if (variadicArgs && i == arguments.size() - 1) {
        if (auto types = callContext.getExpandedVariablePack().addCodeLoc(arguments[i]->codeLoc)) {
          auto origArg = arguments[i]->deepCopy();
          arguments.pop_back();
          for (int j = 0; j < types->second.size(); ++j) {
            auto varName = getExpandedParamName(types->first, j);
            arguments.push_back(origArg->replaceVar(types->first, varName));
            auto argContext = Context::withParent(callContext);
            argContext.addVariable(varName, ReferenceType::get(types->second[j]), arguments.back()->codeLoc);
            argTypes.push_back(TRY(getType(argContext, arguments.back())));
            argLocs.push_back(arguments.back()->codeLoc);
  //          std::cout << "Function argument " << argTypes.back()->getName() << std::endl;
          }
        }
        break;
      } else {
        argTypes.push_back(TRY(getType(context, arguments[i])));
        argLocs.push_back(arguments[i]->codeLoc);
      }
//      std::cout << "Function argument " << argTypes.back()->getName() << std::endl;
    }
    auto tryMethodCall = [&](MethodCallType thisCallType, const Type& origType) -> JustError<ErrorLoc> {
      auto res = getFunction(callContext, codeLoc, *identifierType, *templateArgs, argTypes, argLocs, arguments);
      if (res)
        callType = thisCallType;
      if (callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER) {
        if (!origType.isReference() && argTypes[0]->removePointer()->hasDestructor()) {
          destructorCall = TRY(getFunction(callContext, codeLoc, "destruct"s, {}, {argTypes[0]}, {codeLoc}));
          TRY(destructorCall->addInstance(callContext));
        }
      }
      if (res && functionInfo) {
        error = codeLoc.getError("Ambigous method call:\nCandidate: " + functionInfo->prettyString() +
            "\nCandidate: " + res.get()->prettyString());
        functionInfo = nullptr;
      } else
        res.unpack(functionInfo, error);
      return success;
    };
    if (methodCall) {
      auto leftType = argTypes[0];
      if (!leftType->removeReference().dynamicCast<PointerType>() &&
          !leftType->removeReference().dynamicCast<MutablePointerType>())
        TRY(tryMethodCall(MethodCallType::FUNCTION_AS_METHOD, *leftType));
      argTypes[0] = leftType.dynamicCast<MutableReferenceType>()
          ? SType(MutablePointerType::get(leftType->removeReference()))
          : SType(PointerType::get(leftType->removeReference()));
      TRY(tryMethodCall(MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER, *leftType));
    } else
      getFunction(callContext, codeLoc, *identifierType, *templateArgs, argTypes, argLocs, arguments)
          .unpack(functionInfo, error);
//    if (functionInfo)
//      std::cout << "Arguments \"" << joinTypeList(argTypes) << "\" " << functionInfo->prettyString() << std::endl;
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
  if (identifier)
    if (auto name = identifier->asBasicIdentifier()) {
      vector<SType> args;
      vector<CodeLoc> locs;
      for (auto& e : arguments) {
        locs.push_back(e->codeLoc);
        args.push_back(TRY(e->eval(context)).value);
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
  ret->identifierType = identifierType;
  ret->variadicArgs = variadicArgs;
  ret->variadicTemplateArgs = variadicTemplateArgs;
  return ret;
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
    destructorCall = TRY(getFunction(context, codeLoc, "destruct"s, {}, {PointerType::get(type)}, {codeLoc}));
    TRY(destructorCall->addInstance(context));
  }
  if (!context.isFullyDefined(type->removeReference().get()))
    return codeLoc.getError("Type " + quote(type->getName()) + " is incomplete in this context");
  if (type->isReference() && !type->isBuiltinCopyable(context, expr))
    return codeLoc.getError("Type " + quote(type->getName()) + " is not implicitly copyable. Consider moving or passing as pointer.");
  return type->removeReference()->handleSwitchStatement(*this, context, Type::SwitchArgument::VALUE);
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

bool SwitchStatement::hasReturnStatement(const Context& context) const {
  for (auto& elem : caseElems)
    if (!elem.block->hasReturnStatement(context))
      return false;
  if (defaultBlock && !defaultBlock->hasReturnStatement(context))
    return false;
  return true;
}

UnionDefinition::UnionDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

static WithErrorLine<shared_ptr<StructType>> getNewOrIncompleteStruct(Context& context, string name, CodeLoc codeLoc,
    const TemplateInfo& templateInfo, bool incomplete) {
  if (auto existing = context.getType(name)) {
    auto asStruct = existing.get().dynamicCast<StructType>();
    if (context.isFullyDefined(existing.get().get()) || !asStruct)
      // if it's not an incomplete struct type then this returns a conflict error
      return codeLoc.getError(context.checkNameConflict(name, "Type").get_error());
    return asStruct;
  } else {
    TRY(context.checkNameConflict(name, "Type").addCodeLoc(codeLoc));
    auto paramsContext = Context::withParent(context);
    auto returnType = context.typeRegistry->getStruct(name);
    for (auto& t : TRY(getTemplateParams(templateInfo, context)))
      returnType->templateParams.push_back(std::move(t));
    context.addType(name, returnType, !incomplete);
    return returnType;
  }
}

static JustError<string> getRedefinitionError(const string& typeName, const optional<CodeLoc>& definition) {
  if (definition)
    return "Type " + quote(typeName) + " has already been defined at " +
        definition->file + ", line " + to_string(definition->line);
  else
    return success;
}

JustError<ErrorLoc> UnionDefinition::addToContext(Context& context) {
  type = TRY(getNewOrIncompleteStruct(context, name, codeLoc, templateInfo, false));
  context.setFullyDefined(type.get().get(), true);
  TRY(getRedefinitionError(type->getName(), type->definition).addCodeLoc(codeLoc));
  type->definition = codeLoc;
  auto membersContext = Context::withParent(context);
  if (templateInfo.params.size() != type->templateParams.size())
    return codeLoc.getError("Number of template parameters differs from forward declaration");
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  type->requirements = TRY(applyRequirements(membersContext, templateInfo));
  unordered_set<string> subtypeNames;
  for (auto& subtype : elements) {
    if (subtypeNames.count(subtype.name))
      return subtype.codeLoc.getError("Duplicate union member: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    type->alternatives.push_back({subtype.name, TRY(membersContext.getTypeFromString(subtype.type))});
    vector<SType> params;
    auto subtypeInfo = TRY(membersContext.getTypeFromString(subtype.type));
    if (subtypeInfo != BuiltinType::VOID)
      params.push_back(subtypeInfo);
    auto constructor = FunctionType(type.get(), params, {});
    constructor.parentType = type.get();
    CHECK(type->staticContext.addImplicitFunction(subtype.name, constructor));
  }
  return success;
}

JustError<ErrorLoc> UnionDefinition::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  for (auto& param : type->templateParams)
    bodyContext.addType(param->getName(), param);
  CHECK(!!applyRequirements(bodyContext, templateInfo));
  type->updateInstantations();
  return success;
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

JustError<ErrorLoc> StructDefinition::addToContext(Context& context) {
  type = TRY(getNewOrIncompleteStruct(context, name, codeLoc, templateInfo, incomplete));
  if (!incomplete) {
    TRY(getRedefinitionError(type->getName(), type->definition).addCodeLoc(codeLoc));
    for (auto& attr : TRY(getAttributeTypes(context, attributes)))
      context.setAttribute(type.get(), attr);
    type->definition = codeLoc;
    context.setFullyDefined(type.get().get(), true);
    if (templateInfo.params.size() != type->templateParams.size())
      return codeLoc.getError("Number of template parameters of type " + quote(type->getName()) +
          " differs from forward declaration.");
    auto membersContext = Context::withParent(context);
    addTemplateParams(membersContext, type->templateParams, false);
    type->requirements = TRY(applyRequirements(membersContext, templateInfo));
    for (auto& member : members)
        type->members.push_back({member.name, TRY(membersContext.getTypeFromString(member.type))});
    for (auto& member : members)
      TRY(type->members.back().type->getSizeError(membersContext).addCodeLoc(member.codeLoc));
    for (int i = 0; i < members.size(); ++i)
      for (int j = i + 1; j < members.size(); ++j)
        if (members[i].name == members[j].name)
          return members[j].codeLoc.getError("Duplicate member: " + quote(members[j].name));
    type->external = external;
  } else
  if (!attributes.empty())
    return codeLoc.getError("Forward declaration can't contain any attributes.");
  return success;
}

JustError<ErrorLoc> StructDefinition::check(Context& context, bool notInImport) {
  auto methodBodyContext = Context::withParent(context);
  addTemplateParams(methodBodyContext, type->templateParams, false);
  CHECK(!!applyRequirements(methodBodyContext, templateInfo));
  type->updateInstantations();
  if (!incomplete)
    TRY(type->getSizeError(context).addCodeLoc(codeLoc));
  if (exported && type->destructor && !type->destructor->getDefinition()->exported)
    return type->destructor->getDefinition()->codeLoc.getError(
        "Destuctor function of an exported type must also be exported");
  return success;
}

void StructDefinition::addGeneratedConstructor(Context& context, const AST& ast) const {
  if (!incomplete) {
    bool hasUserDefinedConstructors = false;
    for (auto& elem : ast.elems)
      if (auto functionDef = dynamic_cast<const FunctionDefinition*>(elem.get()))
        if (functionDef->name.contains<ConstructorTag>() && functionDef->returnType.parts[0].name == name)
          hasUserDefinedConstructors = true;
    if (!external) {
      vector<SType> constructorParams;
      for (auto& member : type->members)
        constructorParams.push_back(member.type);
      auto fun = FunctionType(type.get(), std::move(constructorParams), type->templateParams);
      fun.generatedConstructor = true;
      if (!hasUserDefinedConstructors)
        CHECK(context.addImplicitFunction(ConstructorTag{}, fun));
      fun.templateParams.clear();
      fun.parentType = type.get();
      CHECK(type->getStaticContext().addImplicitFunction(ConstructorTag{}, fun));
      type->getStaticContext().addType(name, type.get());
    }
  }
}

MoveExpression::MoveExpression(CodeLoc l, string id, bool hasDestructor)
    : Expression(l), identifier(id), hasDestructor(hasDestructor) {}

WithErrorLine<SType> MoveExpression::getTypeImpl(const Context& context) {
  if (!type) {
    if (auto ret = context.getTypeOfVariable(identifier)) {
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
  return TopLevelAllowance::CAN;
}

unique_ptr<Statement> EmbedStatement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<EmbedStatement>(codeLoc, value);
}

bool EmbedStatement::hasReturnStatement(const Context&) const {
  return true;
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<Statement> i, unique_ptr<Expression> c,
                                   unique_ptr<Expression> it, unique_ptr<Statement> b)
  : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

JustError<ErrorLoc> ForLoopStatement::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  TRY(init->check(bodyContext));
  auto condType = TRY(getType(bodyContext, cond));
  if (condType != BuiltinType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  TRY(getType(bodyContext, iter));
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
      fun(init.get()),
      exprFun(cond.get()),
      exprFun(iter.get()),
      fun(body.get()));
}

StaticForLoopStatement::StaticForLoopStatement(CodeLoc l, string counter, unique_ptr<Expression> init, unique_ptr<Expression> cond,
    unique_ptr<Expression> iter, unique_ptr<Statement> body) : Statement(l), counter(std::move(counter)), init(std::move(init)),
  cond(std::move(cond)), iter(std::move(iter)), body(std::move(body)) {
}

JustError<ErrorLoc> StaticForLoopStatement::checkExpressions(const Context& bodyContext) const {
  auto context = Context::withParent(bodyContext);
  auto initVal = TRY(init->eval(context).addNoEvalError(init->codeLoc.getError("Unable to evaluate expression at compile time")));
  context.addType(counter, CompileTimeValue::getReference(initVal.value.dynamicCast<CompileTimeValue>()));
  auto condVal = TRY(cond->eval(context).addNoEvalError(cond->codeLoc.getError("Unable to evaluate expression at compile time")));
  if (condVal.value->getType()->removeReference() != BuiltinType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote(BuiltinType::BOOL->getName()));
  TRY(iter->eval(context).addNoEvalError(iter->codeLoc.getError("Unable to evaluate expression at compile time")));
  return success;
}

WithErrorLine<vector<unique_ptr<Statement>>> StaticForLoopStatement::getUnrolled(const Context& context,
    SType counterType) const {
  vector<unique_ptr<Statement>> ret;
  auto counterValue = CompileTimeValue::getReference(init->eval(context)->value.dynamicCast<CompileTimeValue>());
  const int maxIterations = 500;
  int countIter = 0;
  while (1) {
    auto evalContext = Context::withParent(context);
    auto iterContext = Context::withParent(context);
    iterContext.addType(counter, counterValue);
    auto currentCounter = counterValue->value.getValueMaybe<CompileTimeValue::ReferenceValue>()->value;
    evalContext.addType(counter, currentCounter);
    auto condValue = cond->eval(evalContext)->value;
    if (!*condValue.dynamicCast<CompileTimeValue>()->value.getValueMaybe<bool>())
      break;
    ErrorLocBuffer errors;
    ret.push_back(body->deepCopy());
    if (!errors.empty())
      return errors[0];
    TRY(ret.back()->check(evalContext));
    CHECK(iter->eval(iterContext)->value->getMangledName());
    if (++countIter >= maxIterations)
      return codeLoc.getError("Static loop reached maximum number of iterations (" + to_string(maxIterations) + ")");
  }
  return std::move(ret);
}

JustError<ErrorLoc> StaticForLoopStatement::check(Context& context, bool) {
  TRY(context.checkNameConflictExcludingFunctions(counter, "variable").addCodeLoc(codeLoc));
  auto bodyContext = Context::withParent(context);
  TRY(checkExpressions(bodyContext));
  auto initType = TRY(getType(bodyContext, init));
  bodyContext.addVariable(counter, MutableReferenceType::get(getType(bodyContext, init).get()), init->codeLoc);
  auto condType = TRY(getType(bodyContext, cond));
  if (condType != BuiltinType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  auto res = TRY(getType(bodyContext, iter));
  auto counterType = CompileTimeValue::getTemplateValue(initType, counter);
  bodyContext.addType(counter, counterType);
  auto bodyTemplateContext = Context::withParent(bodyContext);
  bodyTemplateContext.setTemplated({counterType});
  if (!bodyContext.getTemplateParams())
    unrolled = TRY(getUnrolled(context, counterType));
  else {
    auto bodyTmp = body->deepCopy();
    TRY(bodyTmp->check(bodyTemplateContext));
  }
  return success;
}

JustError<ErrorLoc> StaticForLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  static int loopId = 0;
  --loopId;
  checker.startLoop(loopId);
  if (auto res = body->checkMoves(checker); !res) {
    TRY(checker.endLoop(loopId));
    return res.get_error();
  }
  return checker.endLoop(loopId);
}

unique_ptr<Statement> StaticForLoopStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  return unique<StaticForLoopStatement>(codeLoc,
      counter,
      exprFun(init.get()),
      exprFun(cond.get()),
      exprFun(iter.get()),
      fun(body.get()));
}

WhileLoopStatement::WhileLoopStatement(CodeLoc l, unique_ptr<Expression> c, unique_ptr<Statement> b)
  : Statement(l), cond(std::move(c)), body(std::move(b)) {}

JustError<ErrorLoc> WhileLoopStatement::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
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

ImportStatement::ImportStatement(CodeLoc l, string p, bool isBuiltIn)
    : Statement(l), path(p), isBuiltIn(isBuiltIn) {
}

void ImportStatement::setImportDirs(const vector<string>& p) {
  importDirs = p;
}

JustError<ErrorLoc> ImportStatement::check(Context&, bool) {
  return success;
}

JustError<ErrorLoc> ImportStatement::processImport(const Context& primaryContext, Context& context, ImportCache& cache, const string& content,
    const string& path) {
  if (cache.isCurrentlyImported(path))
    return codeLoc.getError("Public import cycle: " + combine(cache.getCurrentImports(), ", "));
  if (!cache.contains(path)) {
    INFO << "Parsing import " << path;
    ast = unique<AST>(TRY(parse(TRY(lex(content, CodeLoc(path, 0, 0), "end of file")))));
    TRY(addExportedContext(primaryContext, cache, *ast, path, isBuiltIn, importDirs));
  } else
    INFO << "Import " << path << " already cached";
  context.merge(cache.getContext(path));
  return success;
}

JustError<ErrorLoc> ImportStatement::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  INFO << "Resolving import " << path << " from " << codeLoc.file;
  for (auto importDir : concat({getParentPath(codeLoc.file)}, importDirs)) {
    INFO << "Trying directory " << importDir;
    auto importPath = fs::path(importDir)  / path;
    if (auto content = readFromFile(importPath.c_str())) {
      importPath = fs::canonical(importPath);
      INFO << "Imported file " << importPath;
      TRY(processImport(primaryContext, context, cache, content->value, importPath));
      return success;
    }
  }
  return codeLoc.getError("Couldn't resolve import path: " + path);
}

WithEvalError<EvalResult> Expression::eval(const Context&) const {
  return EvalError::noEval();
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

JustError<ErrorLoc> EnumDefinition::addToContext(Context& context) {
  auto type = context.typeRegistry->getEnum(name);
  context.setFullyDefined(type.get(), fullyDefined);
  if (fullyDefined) {
    if (elements.empty())
      return codeLoc.getError("Enum requires at least one element");
    TRY(getRedefinitionError(type->getName(), type->definition).addCodeLoc(codeLoc));
    type->definition = codeLoc;
    type->elements = elements;
    type->external = external;
    unordered_set<string> occurences;
    for (auto& e : elements)
      if (occurences.count(e))
        return codeLoc.getError("Duplicate enum element: " + quote(e));
  }
  if (!context.getType(name))
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
  auto concept = shared<Concept>(name, this, Context(context.typeRegistry), templateInfo.variadic);
  auto declarationsContext = Context::withParent(context);
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    auto& param = templateInfo.params[i];
    concept->modParams().push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    declarationsContext.addType(param.name, concept->modParams().back(), true);
    if (templateInfo.variadic && i == templateInfo.params.size() - 1)
      context.addUnexpandedTypePack(templateInfo.params[i].name, concept->modParams().back());
  }
  for (auto& function : functions) {
    if (function->isVirtual)
      return function->codeLoc.getError("Virtual functions are not allowed here");
    TRY(function->setFunctionType(declarationsContext, concept));
    TRY(function->check(declarationsContext));
    TRY(concept->modContext().addFunction(function->functionInfo.get()).addCodeLoc(function->codeLoc));
  }
  context.addConcept(name, concept);
  return success;
}

JustError<ErrorLoc> ConceptDefinition::check(Context& context, bool) {
  return success;
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

RangedLoopStatement::RangedLoopStatement(CodeLoc l, unique_ptr<VariableDeclaration> init,
    unique_ptr<Expression> container, unique_ptr<Statement> body)
    : Statement(l), init(std::move(init)), container(std::move(container)), body(std::move(body)) {}

JustError<ErrorLoc> RangedLoopStatement::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  auto containerType = TRY(getType(context, container));
  if (!containerType->isReference())
    containerType = ReferenceType::get(containerType);
  auto uniqueSufix = to_string(codeLoc.line) + "_" + to_string(codeLoc.column);
  containerName = "container"s + uniqueSufix;
  auto containerEndName = "container_end"s + uniqueSufix;
  bodyContext.addVariable(*containerName, containerType, codeLoc);
  containerEnd = unique<VariableDeclaration>(codeLoc, none, containerEndName,
      unique<FunctionCall>(codeLoc, IdentifierInfo("end", codeLoc),
          unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, unique<Variable>(IdentifierInfo(*containerName, codeLoc))), false));
  TRY(containerEnd->check(bodyContext));
  init->initExpr = unique<FunctionCall>(codeLoc, IdentifierInfo("begin", codeLoc),
      unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, unique<Variable>(IdentifierInfo(*containerName, codeLoc))), false);
  init->isMutable = true;
  condition = BinaryExpression::get(codeLoc, Operator::NOT_EQUAL, unique<Variable>(IdentifierInfo(init->identifier, codeLoc)),
      unique<Variable>(IdentifierInfo(containerEndName, codeLoc)));
  increment = unique<UnaryExpression>(codeLoc, Operator::INCREMENT, unique<Variable>(IdentifierInfo(init->identifier, codeLoc)));
  TRY(init->check(bodyContext));
  auto condType = TRY(getType(bodyContext, condition));
  if (condType != BuiltinType::BOOL)
    return codeLoc.getError("Equality comparison between iterators does not return type " + quote("bool"));
  TRY(getType(bodyContext, increment));
  loopId = bodyContext.setIsInLoop();
  return body->check(bodyContext);
}

JustError<ErrorLoc> RangedLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  TRY(init->checkMoves(checker));
  TRY(container->checkMoves(checker));
  checker.startLoop(loopId);
  if (auto res = body->checkMoves(checker); !res) {
    TRY(checker.endLoop(loopId));
    return res.get_error();
  }
  return checker.endLoop(loopId);
}

unique_ptr<Statement> RangedLoopStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  auto ret = unique<RangedLoopStatement>(codeLoc,
      cast<VariableDeclaration>(fun(init.get())),
      exprFun(container.get()),
      fun(body.get()));
  return ret;
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
  return ret;
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
      if (type->isConstant)
        if (auto value = type->value.dynamicCast<CompileTimeValue>())
          expr = unique<Constant>(expr->codeLoc, value);
    } else
    if (type.get_error().canEval)
      return expr->codeLoc.getError(type.get_error().error);
  }
  return type;
}

WithErrorLine<nullable<SType>> SwitchStatement::CaseElem::getType(const Context& context) {
  if (type)
    return nullable<SType>(TRY(context.getTypeFromString(*type)));
  else
    return nullable<SType>();
}

SwitchStatement::CaseElem SwitchStatement::CaseElem::transform(const StmtTransformFun& fun, const ExprTransformFun&) const {
  CaseElem ret;
  ret.codeloc = codeloc;
  ret.ids = ids;
  ret.block = cast<StatementBlock>(fun(block.get()));
  ret.type = type;
  return ret;
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

LambdaExpression::LambdaExpression(CodeLoc l, vector<FunctionParameter> params, unique_ptr<StatementBlock> block,
    optional<IdentifierInfo> returnType, LambdaCaptureInfo captureInfo)
    : Expression(l), parameters(std::move(params)), block(std::move(block)), returnType(std::move(returnType)),
      captureInfo(std::move(captureInfo)) {
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
  auto bodyContext = Context::withParent(context);
  ReturnTypeChecker returnChecker(retType);
  bodyContext.addReturnTypeChecker(&returnChecker);
  auto captureTypes = TRY(bodyContext.setLambda(&captureInfo));
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
  auto bodyContext2 = Context::withParent(bodyContext);
  TRY(block->check(bodyContext2));
  type->captures.append(captureInfo.implicitCaptures);
  if (!returnType)
    retType = returnChecker.getReturnType();
  if (!type->functionInfo) {
    FunctionType functionType(retType.get(), params, {});
    auto functioInfo = FunctionInfo::getImplicit("invoke"s, std::move(functionType));
    type->functionInfo = std::move(functioInfo);
  }
  TRY(checkBodyMoves());
  auto blockCopy = block->deepCopy();
  auto res = blockCopy->check(bodyContext);
  CHECK(!!res) << res.get_error();
  if (!block->hasReturnStatement(context) && retType != BuiltinType::VOID)
    return block->codeLoc.getError("Not all paths lead to a return statement in a lambda expression returning non-void");
  type->body = cast<StatementBlock>(std::move(blockCopy));
  type->destructorCalls = getDestructorCalls(codeLoc,
      parameters.transform([](auto& p) { return *p.name;}),
      getSubsequence(type->functionInfo->type.params, 1));
  for (auto& elem : type->destructorCalls)
    if (elem)
      TRY(elem->check(bodyContext));
  context.typeRegistry->addLambda(type.get());
  //context.addType(type->getName(), type.get());
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
      TRY(getFunction(bodyContext, codeLoc, "implicit_copy"s, {}, {PointerType::get(capture.type)}, {codeLoc}).get()
          ->addInstance(bodyContext));
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
  auto indexValue = dynamic_cast<CompileTimeValue*>(indexValue1.value.get());
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

static JustError<ErrorLoc> initializeDestructor(const Context& context, const SType& type, const string& member, CodeLoc codeLoc,
    nullable<SFunctionInfo>& destructorCall) {
  if (auto structType = type.dynamicCast<StructType>()) {
    if (structType->destructor)
      return codeLoc.getError("Can't move member from a value with a user-defined destructor");
    if (structType->hasDestructor()) {
      for (int i = 0; i < structType->members.size(); ++i)
        if (structType->members[i].name == member) {
          destructorCall = TRY(getFunction(context, codeLoc, "destruct_except"s, {CompileTimeValue::get(i)},
              {PointerType::get(type)}, {codeLoc}));
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
  TRY(initializeDestructor(context, leftType, identifier, codeLoc, destructorCall));
  return ret;
}

unique_ptr<Expression> MemberAccessExpression::transform(const StmtTransformFun& fun1, const ExprTransformFun& fun2) const {
  return unique<MemberAccessExpression>(codeLoc, lhs->transform(fun1, fun2), identifier);
}

JustError<ErrorLoc> MemberAccessExpression::checkMoves(MoveChecker& checker) const {
  return lhs->checkMoves(checker);
}

MemberIndexExpression::MemberIndexExpression(CodeLoc l, unique_ptr<Expression> lhs, unique_ptr<Expression> index)
  : Expression(l), lhs(std::move(lhs)), index(std::move(index)) {}

WithErrorLine<SType> MemberIndexExpression::getTypeImpl(const Context& context) {
  auto value = TRY(index->eval(context).addNoEvalError(
      index->codeLoc.getError("Unable to evaluate constant expression at compile-time")));
  auto leftType = TRY(lhs->getTypeImpl(context));
  if (auto res = leftType.get()->getSizeError(context); !res)
    return codeLoc.getError(leftType.get()->getName() + res.get_error());
  auto member = TRY(leftType->getTypeOfMember(value.value).addCodeLoc(codeLoc));
  memberName = member.name;
  isUnion = leftType->removeReference()->getType() == BuiltinType::UNION_TYPE;
  TRY(initializeDestructor(context, leftType, *memberName, codeLoc, destructorCall));
  return member.type;
}

unique_ptr<Expression> MemberIndexExpression::transform(const StmtTransformFun& fun1, const ExprTransformFun& fun2) const {
  return unique<MemberIndexExpression>(codeLoc, lhs->transform(fun1, fun2), index->transform(fun1, fun2));
}

JustError<ErrorLoc> MemberIndexExpression::checkMoves(MoveChecker& checker) const {
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

FatPointerConversion::FatPointerConversion(CodeLoc l, IdentifierInfo toType, unique_ptr<Expression> arg)
    : Expression(l), toType(toType), arg(std::move(arg)) {
}

static WithErrorLine<vector<SFunctionInfo>> getRequiredFunctionsForConceptType(const Context& context,
    const Concept& concept, CodeLoc codeLoc) {
  vector<SFunctionInfo> ret;
  for (auto& fun : concept.getContext().getAllFunctions())
    ret.push_back(TRY(getFunction(context, codeLoc, translateDestructorId(fun->id),
        context.getFunctions(translateDestructorId(fun->id)), {}, fun->type.params,
        vector<CodeLoc>(fun->type.params.size(), codeLoc)).addCodeLoc(codeLoc)));
  return ret;
}

WithErrorLine<SType> FatPointerConversion::getTypeImpl(const Context& context) {
  argType = TRY(getType(context, arg));
  auto ret = TRY(context.getTypeFromString(toType));
  if (argType->isPointer()) {
    argType = argType->removeReference();
    if (!ret->isPointer())
      return toType.codeLoc.getError("Expected pointer to a concept type");
    if (auto t = ret->removePointer().dynamicCast<ConceptType>())
      conceptType = t;
    else
      return toType.codeLoc.getError("Expected pointer to a concept type");
    auto concept = conceptType->getConceptFor(argType->removePointer());
    if (ret.dynamicCast<MutablePointerType>() && !argType.get().dynamicCast<MutablePointerType>())
      return toType.codeLoc.getError("Cannot cast value of type " + quote(argType->getName()) + " to a mutable pointer");
    auto functions = TRY(getRequiredFunctionsForConceptType(context, *concept, codeLoc));
    for (auto& fun : functions)
      fun->addInstance(context);
    concept->def->addFatPointer({argType->removePointer(), functions}, conceptType.get());
    return ret;
  } else {
    if (auto t = ret.dynamicCast<ConceptType>())
      conceptType = t;
    else
      return toType.codeLoc.getError("Expected a concept type");
    auto concept = conceptType->getConceptFor(argType.get());
    if (argType->isReference()) {
      TRY(context.canConvert(argType.get(), argType->removeReference(), arg).addCodeLoc(arg->codeLoc));
      CHECK(!!getType(context, arg));
    }
    auto functions = TRY(getRequiredFunctionsForConceptType(context, *concept, codeLoc));
    for (auto& fun : functions)
      fun->addInstance(context);
    concept->def->addFatPointer({argType.get(), functions}, conceptType.get());
    return ret;
  }
}

unique_ptr<Expression> FatPointerConversion::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return unique<FatPointerConversion>(codeLoc, toType, arg->transform(f1, f2));
}

JustError<ErrorLoc> FatPointerConversion::checkMoves(MoveChecker& c) const {
  return arg->checkMoves(c);
}

UncheckedStatement::UncheckedStatement(CodeLoc l, unique_ptr<Statement> elem) : Statement(l), elem(std::move(elem)) {
}

bool UncheckedStatement::hasReturnStatement(const Context& context) const {
  if (!context.getTemplateParams())
    return elem->hasReturnStatement(context);
  return true;
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
