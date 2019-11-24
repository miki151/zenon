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


unique_ptr<Expression> Expression::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  return transform(
      [&](Statement* expr) { return expr->replace(from, to, errors); },
      [&](Expression* expr) { return expr->replace(from, to, errors); });
}

unique_ptr<Expression> Expression::expand(SType from, vector<SType> to) const {
  return transform(
      [&](Statement* expr) { return expr->expand(from, to); },
      [&](Expression* expr) { return expr->expand(from, to); });
}

unique_ptr<Expression> Expression::replaceVar(string from, string to) const {
  return transform(
      [&](Statement* expr) { return expr->replaceVar(from, to); },
      [&](Expression* expr) { return expr->replaceVar(from, to); });
}

unique_ptr<Expression> Expression::expandVar(string from, vector<string> to) const {
  return transform(
      [&](Statement* expr) { return expr->expandVar(from, to); },
      [&](Expression* expr) { return expr->expandVar(from, to); });
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

WithErrorLine<SType> Constant::getTypeImpl(Context&) {
  return value->getType();
}

optional<EvalResult> Constant::eval(const Context&) const {
  return EvalResult{ value, true};
}

unique_ptr<Expression> Constant::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  ErrorBuffer errors2;
  auto ret = unique<Constant>(codeLoc, value->replace(from, to, errors2).dynamicCast<CompileTimeValue>());
  merge(errors, errors2, codeLoc);
  return ret;
}

unique_ptr<Expression> Constant::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<Constant>(codeLoc, value);
}

WithErrorLine<SType> Variable::getTypeImpl(Context& context) {
  optional<string> varError;
  if (auto id = identifier.asBasicIdentifier()) {
    if (auto& pack = context.getVariablePack())
      if (pack->name == id)
        return SType(shared<VariablePack>(pack->type, *id));
    if (auto varType = context.getTypeOfVariable(*id)) {
      lambdaCapture = context.isCapturedVariable(*id);
      return *varType;
    } else
      varError = varType.get_error();
  }
  if (auto t = context.getTypeFromString(identifier))
    return t.get()->getType();
  return codeLoc.getError(varError.value_or("Identifier not found: " + identifier.prettyString()));
}

optional<EvalResult> Variable::eval(const Context& context) const {
  auto res = context.getTypeFromString(identifier);
  if (res) {
    auto value = res.get().dynamicCast<CompileTimeValue>();
    bool isConstant = !value || !value->value.contains<CompileTimeValue::ReferenceValue>();
    return EvalResult { res.get(), isConstant};
  }
  return none;
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

optional<ErrorLoc> Variable::checkMoves(MoveChecker& checker) const {
  if (auto id = identifier.asBasicIdentifier())
    if (auto err = checker.getUsageError(*id))
      return codeLoc.getError(*err);
  return none;
}

template <typename Comp>
static bool exactArgs(const vector<SType>& argTypes, const FunctionType& f, Comp comp) {
  if (f.params.size() != argTypes.size() || !f.templateParams.empty())
    return false;
  for (int i = 0; i < f.params.size(); ++i)
    if (!comp(argTypes[i], f.params[i].type))
      return false;
  return true;
}

template <typename Comp>
static bool exactFirstArg(const vector<SType>& argTypes, const FunctionType& overload, Comp comp) {
  return !argTypes.empty() && !overload.params.empty() && comp(argTypes[0], overload.params[0].type);
}

static bool fromConcept(const vector<SType>&, const SFunctionInfo& f) {
  return f->type.fromConcept;
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
  auto isExactValueArg = [] (SType arg, SType param) {
    return param == arg->getUnderlying();
  };
  auto isExactReferenceArg = [] (SType arg, SType param) {
    bool byConstRef = param.dynamicCast<ReferenceType>() &&
        !arg.dynamicCast<MutableReferenceType>() &&
        param->getUnderlying() == arg->getUnderlying();
    bool byRef = param == arg;
    return byConstRef || byRef;
  };
  auto isConstToMutableReferenceArg = [] (SType arg, SType param) {
    bool byConstRef = param.dynamicCast<ReferenceType>() &&
        arg.dynamicCast<MutableReferenceType>() &&
        param->getUnderlying() == arg->getUnderlying();
    return byConstRef;
  };
  auto isSpecialized = [&] (const auto& args, const auto& overload) {
    for (auto& other : overloads)
      if (other != overload && context.isGeneralization(overload->parent.value_or(overload), other->parent.value_or(other)))
        return false;
    return true;
  };
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


static WithErrorLine<SFunctionInfo> handleOperatorOverloads(Context& context, CodeLoc codeLoc, Operator op,
    vector<SType> types, vector<CodeLoc> argLocs) {
  vector<SFunctionInfo> overloads;
  if (auto fun = context.getBuiltinOperator(op, types))
    overloads.push_back(fun.get());
  vector<string> errors;
  for (auto fun : context.getOperatorType(op))
    if (auto inst = instantiateFunction(context, fun, codeLoc, {}, types, argLocs, {}))
      overloads.push_back(inst.get());
    else
      errors.push_back("Candidate: " + fun->prettyString() + ": " + inst.get_error().error);
  filterOverloads(context, overloads, types);
  if (overloads.size() == 1) {
    //cout << "Chosen overload " << overloads[0].toString() << endl;
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

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr, bool rightWithBrackets) {
  switch (op) {
    case Operator::NOT_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::EQUALS, std::move(expr), rightWithBrackets));
    case Operator::LESS_OR_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::MORE_THAN, std::move(expr), rightWithBrackets));
    case Operator::MORE_OR_EQUAL:
      return unique<UnaryExpression>(loc, Operator::LOGICAL_NOT, get(loc, Operator::LESS_THAN, std::move(expr), rightWithBrackets));
    default:
      return unique<BinaryExpression>(Private{}, loc, op, std::move(expr), rightWithBrackets);
  }
}

unique_ptr<Expression> BinaryExpression::get(CodeLoc loc, Operator op, unique_ptr<Expression> a,
    unique_ptr<Expression> b, bool rightWithBrackets) {
  return get(loc, op, makeVec<unique_ptr<Expression>>(std::move(a), std::move(b)), rightWithBrackets);
}

BinaryExpression::BinaryExpression(BinaryExpression::Private, CodeLoc loc, Operator op, vector<unique_ptr<Expression>> expr,
      bool rightWithBrackets)
    : Expression(loc), op(op), expr(std::move(expr)), rightWithBrackets(rightWithBrackets) {}

WithErrorLine<SType> BinaryExpression::getTypeImpl(Context& context) {
  switch (op) {
    case Operator::NOT_EQUAL:
    case Operator::LESS_OR_EQUAL:
    case Operator::MORE_OR_EQUAL:
      FATAL << "This operator should have been rewritten";
      fail();
    default: {
      vector<SType> exprTypes;
      for (auto& elem : expr)
        if (auto t = getType(context, elem))
          exprTypes.push_back(*t);
        else
          return t;
      if (op == Operator::SUBSCRIPT && exprTypes[0].dynamicCast<VariablePack>() && !expr[1]->eval(context))
        return expr[1]->codeLoc.getError("Unable to evaluate variable pack index at compile-time");
      if (auto fun = handleOperatorOverloads(context, codeLoc, op, exprTypes,
          ::transform(expr, [&](auto& e) { return e->codeLoc;}))) {
        functionInfo = *fun;
        if (auto parent = functionInfo->parent)
          if (parent->definition)
            if (auto error = parent->definition->addInstance(context, functionInfo.get()))
              return *error;
        return functionInfo->type.retVal;
      } else
        return fun.get_error();
    }
  }
}

optional<EvalResult> BinaryExpression::eval(const Context& context) const {
  if (auto value1 = expr[0]->eval(context)) {
    if (auto value2 = expr[1]->eval(context)) {
      if (auto res = ::eval(op, {value1->value, value2->value}))
        return EvalResult{res.get(), value1->isConstant && value2->isConstant};
    }
  }
  return none;
}

unique_ptr<Expression> BinaryExpression::expandVar(string from, vector<string> to) const {
  if (op == Operator::SUBSCRIPT)
    if (auto var = dynamic_cast<Variable*>(expr[0].get()))
      if (var->identifier.asBasicIdentifier() == from)
        if (auto index = dynamic_cast<Constant*>(expr[1].get())) {
          if (auto val = index->value->value.getValueMaybe<int>()){
            if (*val >= 0 && *val < to.size())
              return unique<Variable>(IdentifierInfo(to[*val], codeLoc));
          } else
            return unique<VariablePackElement>(codeLoc, from, std::move(to), index->value);
        }
  return Expression::expandVar(std::move(from), std::move(to));
}

unique_ptr<Expression> BinaryExpression::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return get(codeLoc, op, fun(expr[0].get()), fun(expr[1].get()), rightWithBrackets);
}

optional<ErrorLoc> BinaryExpression::checkMoves(MoveChecker& checker) const {
  for (auto& e : expr) {
    if (auto error = e->checkMoves(checker))
      return error;
  }
  return none;
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

WithErrorLine<SType> UnaryExpression::getTypeImpl(Context& context) {
  nullable<SType> ret;
  auto right = getType(context, expr, op != Operator::GET_ADDRESS);
  if (!right)
    return right;
  ErrorLoc error { codeLoc, "Can't apply operator: " + quote(getString(op)) + " to type: " + quote(right.get()->getName())};
  if (auto t = expr->getTypeImpl(context)) {
    if (auto fun = handleOperatorOverloads(context, codeLoc, op, {*t}, {expr->codeLoc})) {
      functionInfo = *fun;
      if (auto parent = functionInfo->parent)
        if (parent->definition)
          if (auto error = parent->definition->addInstance(context, functionInfo.get()))
            return *error;
      return functionInfo->type.retVal;
    } else
      return fun.get_error();
  } else
    return t;
}

optional<EvalResult> UnaryExpression::eval(const Context& context) const {
  if (auto value = expr->eval(context)) {
      if (auto res = ::eval(op, {value->value}))
        return EvalResult{res.get(), value->isConstant};
  }
  return none;
}

unique_ptr<Expression> UnaryExpression::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return unique<UnaryExpression>(codeLoc, op, fun(expr.get()));
}

optional<ErrorLoc> UnaryExpression::checkMoves(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

optional<ErrorLoc> StatementBlock::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  for (auto& s : elems)
    if (auto err = s->check(bodyContext))
      return err;
  return none;
}

optional<ErrorLoc> StatementBlock::checkMovesImpl(MoveChecker& checker) const {
  checker.startBlock();
  for (auto& elem : elems)
    if (auto err = elem->checkMoves(checker)) {
      checker.endBlock();
      return err;
    }
  checker.endBlock();
  return none;
}

unique_ptr<Statement> StatementBlock::transform(const StmtTransformFun& fun, const ExprTransformFun&) const {
  auto ret = unique<StatementBlock>(codeLoc);
  for (auto& elem : elems)
    ret->elems.push_back(fun(elem.get()));
  return ret;
}

optional<ErrorLoc> IfStatement::check(Context& context, bool) {
  auto ifContext = Context::withParent(context);
  if (declaration)
    if (auto err = declaration->check(ifContext))
      return err;
  auto negate = [&] (unique_ptr<Expression> expr) {
    auto codeLoc = expr->codeLoc;
    return unique<UnaryExpression>(codeLoc, Operator::LOGICAL_NOT, std::move(expr));
  };
  if (!condition)
    condition = negate(negate(unique<Variable>(IdentifierInfo(declaration->identifier, declaration->codeLoc))));
  auto condType = getType(ifContext, condition);
  if (!condType)
    return condType.get_error();
  if (!ifContext.canConvert(*condType, ArithmeticType::BOOL)) {
    condition = negate(negate(std::move(condition)));
    condType = getType(ifContext, condition);
    if (!condType)
      return condType.get_error();
  }
  if (!ifContext.canConvert(*condType, ArithmeticType::BOOL)) {
    return codeLoc.getError(
        "Expected a type convertible to bool or with overloaded operator " +
        quote("!") + " inside if statement, got " + quote(condType.get()->getName()));
  }
  if (auto err = ifTrue->check(ifContext))
    return err;
  if (ifFalse) {
    if (auto err = ifFalse->check(ifContext))
      return err;
  }
  return none;
}

optional<ErrorLoc> IfStatement::checkMovesImpl(MoveChecker& checker) const {
  if (declaration)
    if (auto err = declaration->checkMoves(checker))
      return err;
  if (auto err = condition->checkMoves(checker))
    return err;
  checker.startBlock();
  checker.newAlternative();
  if (auto err = ifTrue->checkMoves(checker)) {
    checker.endBlock();
    return err;
  }
  if (ifFalse) {
    checker.newAlternative();
    if (auto err = ifFalse->checkMoves(checker)){
      checker.endBlock();
      return err;
    }
  }
  checker.endBlock();
  return none;
}

unique_ptr<Statement> IfStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  return unique<IfStatement>(codeLoc,
      declaration ? cast<VariableDeclaration>(fun(declaration.get())) : nullptr,
      condition ? exprFun(condition.get()) : nullptr,
      fun(ifTrue.get()),
      ifFalse ? fun(ifFalse.get()) : nullptr);
}

static optional<string> getVariableInitializationError(const char* action, const Context& context, const SType& varType,
    const SType& exprType) {
  if ((exprType.dynamicCast<ReferenceType>() || exprType.dynamicCast<MutableReferenceType>()) &&
      !exprType.get()->getUnderlying()->isBuiltinCopyable(context))
    return "Type " + quote(exprType.get()->getUnderlying()->getName()) + " cannot be copied implicitly"s;
  if (!context.canConvert(exprType, varType))
    return "Can't "s + action + " of type "
       + quote(varType->getName()) + " using a value of type " + quote(exprType->getName()); return none;
}

optional<ErrorLoc> VariableDeclaration::check(Context& context, bool) {
  if (auto err = context.checkNameConflict(identifier, "Variable"))
    return codeLoc.getError(*err);
  if (!realType) {
    if (type) {
      if (auto t = context.getTypeFromString(*type))
        realType = *t;
      else
        return t.get_error();
    } else
    if (initExpr) {
      if (auto t = getType(context, initExpr))
        realType = t.get()->getUnderlying();
      else
        return t.get_error();
    } else
      return codeLoc.getError("Initializing expression needed to infer variable type");
  }
  if (!realType->canDeclareVariable())
    return codeLoc.getError("Can't declare variable of type " + quote(realType->getName()));
  if (auto error = realType.get()->getSizeError(context))
    return codeLoc.getError(*error);
  INFO << "Adding variable " << identifier << " of type " << realType.get()->getName();
  if (!initExpr)
    return codeLoc.getError("Variable requires initialization");
  auto exprType = getType(context, initExpr);
  if (!exprType)
    return exprType.get_error();
  if (!isMutable)
    if (auto value = initExpr->eval(context))
      context.addType(identifier, value->value);
  if (auto error = getVariableInitializationError("initialize variable", context, realType.get(), *exprType))
    return initExpr->codeLoc.getError(*error);
  auto varType = isMutable ? SType(MutableReferenceType::get(realType.get())) : SType(ReferenceType::get(realType.get()));
  context.addVariable(identifier, std::move(varType));
  return none;
}

optional<ErrorLoc> VariableDeclaration::checkMovesImpl(MoveChecker& checker) const {
  if (initExpr)
    if (auto err = initExpr->checkMoves(checker))
      return err;
  checker.addVariable(identifier);
  return none;
}

unique_ptr<Statement> VariableDeclaration::transform(const StmtTransformFun&,
    const ExprTransformFun& exprFun) const {
  auto ret = unique<VariableDeclaration>(codeLoc, none, identifier,
      initExpr ? exprFun(initExpr.get()) : nullptr);
  ret->isMutable = isMutable;
  return ret;
}

optional<ErrorLoc> ReturnStatement::check(Context& context, bool) {
  auto returnType = [&]() -> WithErrorLine<SType> {
    if (expr)
      return getType(context, expr);
    else
      return SType(ArithmeticType::VOID);
  }();
  if (!returnType)
    return returnType.get_error();
  if (auto err = context.getReturnTypeChecker()->addReturnStatement(context, *returnType))
    return codeLoc.getError(*err);
  return none;
}

optional<ErrorLoc> ReturnStatement::checkMovesImpl(MoveChecker& checker) const {
  if (expr)
    if (auto err = expr->checkMoves(checker))
      return err;
  checker.returnStatement();
  return none;
}

unique_ptr<Statement> ReturnStatement::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  return unique<ReturnStatement>(codeLoc, expr ? fun(expr.get()) : nullptr);
}

optional<ErrorLoc> Statement::addToContext(Context&) {
  return none;
}

optional<ErrorLoc> Statement::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  return addToContext(context);
}

optional<ErrorLoc> Statement::checkMoves(MoveChecker& checker) const {
  checker.clearStatementUsages();
  auto ret = checkMovesImpl(checker);
  checker.clearStatementUsages();
  return ret;
}

optional<ErrorLoc> Statement::checkMovesImpl(MoveChecker&) const {
  return none;
}

bool Statement::hasReturnStatement(const Context&) const {
  return false;
}

unique_ptr<Statement> Statement::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  return transform(
      [&](Statement* s) { return s->replace(from, to, errors); },
      [&](Expression* s) { return s->replace(from, to, errors); });
}

unique_ptr<Statement> Statement::expand(SType from, vector<SType> to) const {
  return transform(
      [&](Statement* s) { return s->expand(from, to); },
      [&](Expression* s) { return s->expand(from, to); });
}

unique_ptr<Statement> Statement::replaceVar(string from, string to) const {
  return transform(
      [&](Statement* s) { return s->replaceVar(from, to); },
      [&](Expression* s) { return s->replaceVar(from, to); });
}

unique_ptr<Statement> Statement::expandVar(string from, vector<string> to) const {
  return transform(
      [&](Statement* s) { return s->expandVar(from, to); },
      [&](Expression* s) { return s->expandVar(from, to); });
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

static WithErrorLine<vector<SType>> translateConceptParams(const Context& context, const TemplateInfo::ConceptRequirement& requirement) {
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
      if (auto origParam = context.getTypeFromString(*arg, requireTypePack)) {
        missingTypePack &= (context.getTypePack() != *origParam);
        if (auto templateParam = origParam->dynamicCast<TemplateParameterType>()) {
          // Support is_enum concept
          auto& conceptParams = concept->getParams();
          if (conceptParams[min<int>(i, conceptParams.size() -1 )]->getType() != ArithmeticType::ANY_TYPE)
            templateParam->type = conceptParams[min<int>(i, conceptParams.size() -1 )]->getType();
        }
        translatedParams.push_back(std::move(*origParam));
      } else
        return origParam.get_error();
    } else
      return reqId.codeLoc.getError("Expected a type argument");
  }
  if (missingTypePack)
    return requirement.identifier.codeLoc.getError("Variadic parameter requires at least one type pack argument");
  return translatedParams;
}

NODISCARD static WithErrorLine<vector<TemplateRequirement>> applyConcept(Context& from, const TemplateInfo& requirements) {
  vector<TemplateRequirement> ret;
  for (auto& req : requirements.requirements)
    if (auto requirement = req.getValueMaybe<TemplateInfo::ConceptRequirement>()) {
      auto& reqId = requirement->identifier;
      if (requirement->variadic && reqId.parts[0].variadic)
        return reqId.codeLoc.getError("Requirement can't be both variadic and involving a variadic concept");
      if (auto concept = from.getConcept(reqId.parts[0].name)) {
        if (!concept->isVariadic() && reqId.parts[0].variadic)
          return reqId.codeLoc.getError("Concept " + quote(concept->getName()) + " is not variadic");
        auto& requirementArgs = reqId.parts[0].templateArguments;
        if ((!concept->isVariadic() && requirementArgs.size() != concept->getParams().size()) ||
            (concept->isVariadic() && requirementArgs.size() < concept->getParams().size() - 1))
          return reqId.codeLoc.getError(
              "Wrong number of template arguments to concept " + quote(concept->getName()) + ": " + quote(reqId.parts[0].toString()));
        if (auto translatedParams = translateConceptParams(from, *requirement)) {
          ErrorBuffer errors;
          auto translated = concept->translate(*translatedParams, requirements.variadic, errors);
          if (!errors.empty())
            return reqId.codeLoc.getError(errors[0]);
          from.merge(translated->getContext());
          ret.push_back(TemplateRequirement(std::move(translated), requirement->variadic));
        } else
          return translatedParams.get_error();
      } else
        return reqId.codeLoc.getError("Uknown concept: " + reqId.parts[0].name);
    } else
    if (auto expr1 = req.getValueMaybe<shared_ptr<Expression>>()) {
      auto expr = expr1->get()->deepCopy();
      if (auto res = getType(from, expr); !res)
        return res.get_error();
      if (auto value = expr->eval(from)) {
        if (value->value->getType() != ArithmeticType::BOOL)
          return expr->codeLoc.getError("Expected expression of type " + quote(ArithmeticType::BOOL->getName()) +
              ", got " + quote(value->value->getType()->getName()));
        ret.push_back(TemplateRequirement(shared_ptr<Expression>(std::move(expr)), false));
      } else
        return expr1->get()->codeLoc.getError("Unable to evaluate expression at compile-time");
    }
  return ret;
}

static SType convertPointerToReference(SType type) {
  if (auto p = type.dynamicCast<PointerType>())
    return ReferenceType::get(p->underlying);
  else if (auto p = type.dynamicCast<MutablePointerType>())
    return MutableReferenceType::get(p->underlying);
  else
    return type;
}

static SType convertReferenceToPointer(SType type) {
  if (auto p = type.dynamicCast<ReferenceType>())
    return PointerType::get(p->underlying);
  else if (auto p = type.dynamicCast<MutableReferenceType>())
    return MutablePointerType::get(p->underlying);
  else
    return type;
}

static bool paramsAreGoodForOperator(const vector<FunctionType::Param>& params) {
  for (auto& p : params)
    if (p.type->getUnderlying()->getType() == ArithmeticType::STRUCT_TYPE)
      return true;
  return false;
}

WithErrorLine<SType> FunctionDefinition::getReturnType(const Context& context) const {
  if (returnType.asBasicIdentifier() == "noreturn"s)
    return (SType) ArithmeticType::NORETURN;
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
      if (auto err = paramsContext.checkNameConflict(param.name, "template parameter"))
        return param.codeLoc.getError(*err);
      ret.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
      paramsContext.addType(param.name, ret.back());
    }
  }
  return ret;
}

optional<ErrorLoc> FunctionDefinition::setFunctionType(const Context& context, bool concept, bool builtInImport) {
  for (int i = 0; i < parameters.size(); ++i)
    if (!parameters[i].name)
      parameters[i].name = "parameter" + to_string(i);
  if (auto s = name.getReferenceMaybe<string>()) {
    if (auto err = context.checkNameConflictExcludingFunctions(*s, "Function"))
      return codeLoc.getError(*err);
  } else
  if (auto op = name.getValueMaybe<Operator>()) {
    if (!canOverload(*op, int(parameters.size())))
      return codeLoc.getError("Can't overload operator " + quote(getString(*op)) +
          " with " + to_string(parameters.size()) + " arguments.");
  }
  Context contextWithTemplateParams = Context::withParent(context);
  auto templateTypes = getTemplateParams(templateInfo, context);
  if (!templateTypes)
    return templateTypes.get_error();
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    auto& param = templateInfo.params[i];
    contextWithTemplateParams.addType(param.name, (*templateTypes)[i], true,
          i == templateInfo.params.size() - 1 && templateInfo.variadic);
  }
  auto requirements = applyConcept(contextWithTemplateParams, templateInfo);
  if (!requirements)
    return requirements.get_error();
  if (auto returnType1 = getReturnType(contextWithTemplateParams)) {
    auto returnType = *returnType1;
    if (name.contains<Operator>())
      returnType = convertPointerToReference(returnType);
    vector<FunctionType::Param> params;
    set<string> paramNames;
    for (int i = 0; i < parameters.size(); ++i) {
      auto& p = parameters[i];
      auto type = contextWithTemplateParams.getTypeFromString(p.type, isVariadicParams && i == parameters.size() - 1);
      if (!type)
        return type.get_error();
      if (*type == ArithmeticType::VOID)
        return p.codeLoc.getError("Function parameter may not have " + quote(type->get()->getName()) + " type");
      if (name.contains<Operator>())
        type = convertPointerToReference(*type);
      params.push_back({p.name, std::move(*type)});
      if (p.name) {
        if (paramNames.count(*p.name))
          return p.codeLoc.getError("Duplicate function parameter name: " + quote(*p.name));
        paramNames.insert(*p.name);
      }
    }
    if (!builtInImport && !concept && name.contains<Operator>() && !paramsAreGoodForOperator(params))
      return codeLoc.getError("Operator parameters must include at least one user-defined type");
    FunctionType functionType(returnType, params, *templateTypes);
    functionType.fromConcept = concept;
    functionType.requirements = *requirements;
    functionType.variadicTemplate = templateInfo.variadic;
    functionType.variadicParams = isVariadicParams;
    if (name.contains<ConstructorTag>() && external)
      functionType.generatedConstructor = true;
    if (external)
      functionType.setBuiltin();
    functionInfo = FunctionInfo::getDefined(name, std::move(functionType), this);
    if (functionInfo->isMainFunction()) {
      auto expectedParam = SliceType::get(ArithmeticType::STRING);
      if (!functionInfo->type.params.empty() && (functionInfo->type.params.size() > 1
          || functionInfo->type.params[0].type != expectedParam))
        return codeLoc.getError("The main() function should take no arguments or take a single argument of type "
            + quote(expectedParam->getName()));
      if (functionInfo->type.retVal != ArithmeticType::INT)
        return codeLoc.getError("The main() function should return a value of type " + quote(ArithmeticType::INT->getName()));
    }
    return none;
  } else
    return returnType1.get_error();
}

static WithErrorLine<SFunctionInfo> getFunction(const Context& context,
    CodeLoc codeLoc, IdentifierType id, vector<SType> templateArgs, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc) {
  ErrorLoc errors = codeLoc.getError("Couldn't find function " + id.prettyString() +
      " matching arguments: (" + joinTypeList(argTypes) + ")");
  vector<SFunctionInfo> overloads;
  if (auto templateType = context.getFunctionTemplate(id)) {
    for (auto& overload : *templateType)
      if (auto f = instantiateFunction(context, overload, codeLoc, templateArgs, argTypes, argLoc)) {
        overloads.push_back(f.get());
      } else
        errors = codeLoc.getError(errors.error + "\nCandidate: "s + overload->prettyString() + ": " +
            f.get_error().error);
  }
  if (id.isSimpleString("invoke") && !argTypes.empty() && argTypes[0].dynamicCast<PointerType>())
    if (auto lambda = argTypes[0]->removePointer().dynamicCast<LambdaType>()) {
      if (auto f = instantiateFunction(context, lambda->functionInfo.get(), codeLoc, templateArgs, argTypes, argLoc)) {
        if (!contains(overloads, *f))
          overloads.push_back(*f);
      } else
        errors = codeLoc.getError(errors.error + "\nCandidate: "s + lambda->functionInfo->prettyString() + ": " +
            f.get_error().error);
    }
  if (overloads.empty())
    return errors;
  filterOverloads(context, overloads, argTypes);
  CHECK(!overloads.empty());
  if (overloads.size() == 1)
    return overloads[0];
  else
    return codeLoc.getError("Multiple function overloads found:\n" +
        combine(transform(overloads, [](const auto& o) { return o->prettyString();}), "\n"));
}

WithErrorLine<SFunctionInfo> getCopyFunction(const Context& context, CodeLoc callLoc, const SType& t) {
  return getFunction(context, callLoc, IdentifierType("copy"), {}, {PointerType::get(t)}, {callLoc});
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualFunctionCallExpr(const Context& context,
    const string& funName, const string& alternativeName, const SType& alternativeType, int virtualIndex) {
  auto functionCall = unique<FunctionCall>(codeLoc, IdentifierInfo(funName, codeLoc), false);
  vector<SType> args;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      args.push_back(functionInfo->type.params[i].type);
    } else {
      functionCall->arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
      args.push_back(alternativeType);
    }
  if (auto fun = getFunction(context, codeLoc, IdentifierType(funName), {}, args,
      vector<CodeLoc>(args.size(), codeLoc)))
    return unique_ptr<Expression>(std::move(functionCall));
  else
    return fun.get_error();
}

WithErrorLine<unique_ptr<Expression>> FunctionDefinition::getVirtualOperatorCallExpr(Context& context,
    Operator op, const string& alternativeName, const SType& alternativeType, int virtualIndex) {
  vector<unique_ptr<Expression>> arguments;
  vector<SType> argTypes;
  for (int i = 0; i < parameters.size(); ++i)
    if (i != virtualIndex) {
      arguments.push_back(unique<MoveExpression>(codeLoc, *parameters[i].name));
      argTypes.push_back(functionInfo->type.params[i].type);
    } else {
      arguments.push_back(unique<MoveExpression>(codeLoc, alternativeName));
      argTypes.push_back(alternativeType);
      if (i == 0 && (alternativeType.dynamicCast<PointerType>() || alternativeType.dynamicCast<MutablePointerType>())) {
        arguments.back() = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(arguments.back()));
        argTypes.back() = argTypes.back()->removePointer();
      }
    }
  if (auto fun = handleOperatorOverloads(context, codeLoc, op, argTypes, vector<CodeLoc>(argTypes.size(), codeLoc))) {
    if (parameters.size() == 1)
      return unique_ptr<Expression>(unique<UnaryExpression>(codeLoc, op, std::move(arguments[0])));
    else {
      CHECK(parameters.size() == 2);
      return unique_ptr<Expression>(BinaryExpression::get(codeLoc, op, std::move(arguments), false));
    }
  } else
    return fun.get_error();
}

optional<ErrorLoc> FunctionDefinition::generateVirtualDispatchBody(Context& bodyContext) {
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
      // we have to change the parameter names, because they could clash with variant alternative names
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
  auto variantType = virtualType->dynamicCast<StructType>();
  bool isPointerParam = false;
  if (!variantType) {
    variantType = virtualType.get()->removePointer().dynamicCast<StructType>();
    switchExpr = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(switchExpr));
    isPointerParam = true;
  }
  if (!variantType || variantType->alternatives.empty())
    return codeLoc.getError("Virtual parameter must be of a variant type or a pointer to one");
  auto switchStatementPtr = unique<SwitchStatement>(codeLoc, std::move(switchExpr));
  auto& switchStatement = *switchStatementPtr;
  body->elems.push_back(std::move(switchStatementPtr));
  for (auto& alternative : variantType->alternatives) {
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
          alternativeType,
          {alternative.name},
          alternative.name,
          std::move(block)
        });
  }
  return none;
}

optional<ErrorLoc> FunctionDefinition::checkAndGenerateCopyFunction(const Context& context) {
  if (!body && isDefault) {
    if (parameters.size() != 1)
      return codeLoc.getError("Expected exactly one parameter in copy function");
    auto type = context.getTypeFromString(parameters[0].type);
    if (!type)
      return type.get_error();
    if (*type != PointerType::get(context.getTypeFromString(returnType).get()))
      return codeLoc.getError("Copy function parameter type must be the same as pointer to return type");
    auto structType = type.get()->removePointer().dynamicCast<StructType>();
    if (!structType)
      return codeLoc.getError("Can only generate copy function for user-defined types");
    body = unique<StatementBlock>(codeLoc);
    if (structType->alternatives.empty()) {
      auto call = unique<FunctionCall>(codeLoc, returnType, false);
      for (auto elem : structType->members) {
        auto copiedParam = unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc));
        auto copyCall = unique<FunctionCall>(codeLoc, IdentifierInfo("copy", codeLoc), false);
        copyCall->arguments.push_back(unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS,
            MemberAccessExpression::getPointerAccess(codeLoc, std::move(copiedParam), elem.name)));
        call->arguments.push_back(std::move(copyCall));
      }
      body->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(call)));
    } else {
      auto copiedParam = unique<Variable>(IdentifierInfo(*parameters[0].name, codeLoc));
      auto topSwitch = unique<SwitchStatement>(codeLoc,
          unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(copiedParam)));
      for (auto& alternative : structType->alternatives) {
        auto block = unique<StatementBlock>(codeLoc);
        auto constructorName = returnType;
        constructorName.parts.push_back(IdentifierInfo::IdentifierPart { alternative.name, {} });
        auto constructorCall = unique<FunctionCall>(codeLoc, constructorName, false);
        if (alternative.type != ArithmeticType::VOID)
          constructorCall->arguments.push_back(unique<FunctionCall>(codeLoc, IdentifierInfo("copy", codeLoc),
              unique<Variable>(IdentifierInfo(alternative.name, codeLoc)), false));
        block->elems.push_back(unique<ReturnStatement>(codeLoc, std::move(constructorCall)));
        topSwitch->caseElems.push_back(
            SwitchStatement::CaseElem {
              codeLoc,
              SType(alternative.type != ArithmeticType::VOID ? PointerType::get(alternative.type) : alternative.type),
              {alternative.name},
              alternative.name,
              std::move(block)
            }
        );
      }
      body->elems.push_back(std::move(topSwitch));
    }
  }
  return none;
}

optional<ErrorLoc> FunctionDefinition::checkAndGenerateDefaultConstructor(const Context& context) {
  if (!body && isDefault) {
    auto type = context.getTypeFromString(returnType);
    if (!type)
      return type.get_error();
    auto structType = type->dynamicCast<StructType>();
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
  return none;
}

optional<ErrorLoc> FunctionDefinition::InstanceInfo::generateBody(StatementBlock* parentBody, CodeLoc codeLoc) {
  CHECK(!body);
  auto templateParams = functionInfo->parent->type.templateParams;
  for (int i = 0; i < templateParams.size(); ++i) {
    StatementBlock* useBody = (i == 0) ? parentBody : body.get();
    ErrorLocBuffer errors;
    if (functionInfo->parent->type.variadicTemplate && i == templateParams.size() - 1) {
      vector<SType> expanded;
      for (int j = i; j < functionInfo->type.templateParams.size(); ++j)
        expanded.push_back(functionInfo->type.templateParams[j]);
      body = cast<StatementBlock>(
          useBody->expand(templateParams[i], std::move(expanded)));
    } else
      body = cast<StatementBlock>(
          useBody->replace(templateParams[i], {functionInfo->type.templateParams[i]}, errors));
    ErrorBuffer errors2;
    for (int j = i + 1; j < templateParams.size(); ++j)
      templateParams[j] = templateParams[j]->replace(templateParams[i], functionInfo->type.templateParams[i], errors2);
    merge(errors, errors2, codeLoc);
    if (!errors.empty())
      return errors[0];
  }
  CHECK(!!body);
  if (functionInfo->parent->type.variadicParams)
    if (auto& lastName = functionInfo->parent->definition->parameters.back().name) {
      vector<string> expanded;
      for (int i = 0; i < functionInfo->type.params.size() + 1 - functionInfo->parent->type.params.size(); ++i)
        expanded.push_back(getExpandedParamName(*lastName, i));
      body = cast<StatementBlock>(body->expandVar(*lastName, expanded));
    }
  return none;
}

optional<ErrorLoc> FunctionDefinition::addInstance(const Context& callContext, const SFunctionInfo& instance) {
  if (callContext.isTemplated())
    return none;
  vector<SFunctionInfo> requirements;
  for (auto& req : instance->type.requirements)
    if (auto concept = req.base.getValueMaybe<SConcept>())
      append(requirements, *callContext.getRequiredFunctions(**concept, {}));
  for (auto& fun : requirements)
    if (fun->type.fromConcept)
      return none;
  if (instance != functionInfo.get()) {
    if (body) {
      CHECK(instance->parent == functionInfo);
      for (auto& other : instances)
        if (other.functionInfo->getWithoutRequirements() == instance->getWithoutRequirements())
          return none;
      instances.push_back(InstanceInfo{unique_ptr<StatementBlock>(), instance, requirements});
      if (definitionContext) {
        if (auto error = instances.back().generateBody(body.get(), codeLoc))
          return *error;
        return checkBody(requirements, *instances.back().body,
            *instances.back().functionInfo);
      }
    }
  } else
    CHECK(instance->type.templateParams.empty());
  return none;
}

static void addTemplateParams(Context& context, vector<SType> params, bool variadic) {
  for (int i = 0; i < params.size(); ++i) {
    auto& param = params[i];
    if (auto valueType = param.dynamicCast<CompileTimeValue>()) {
      auto templateValue = valueType->value.getReferenceMaybe<CompileTimeValue::TemplateValue>();
      context.addType(templateValue->name,
          CompileTimeValue::getTemplateValue(templateValue->type, templateValue->name));
    } else
      context.addType(param->getName(), param, true, variadic && i == params.size() - 1);
  }
}

optional<ErrorLoc> FunctionDefinition::generateDefaultBodies(Context& context) {
  Context bodyContext = Context::withParent(context);
  addTemplateParams(bodyContext, functionInfo->type.templateParams, functionInfo->type.variadicTemplate);
  auto res = applyConcept(bodyContext, templateInfo);
  if (!res)
    return res.get_error();
  if (isVirtual)
    if (auto err = generateVirtualDispatchBody(bodyContext))
      return err;
  if (name == "copy"s) {
    if (auto err = checkAndGenerateCopyFunction(bodyContext))
      return err;
  } else
  if (name == ConstructorTag{}) {
    if (auto err = checkAndGenerateDefaultConstructor(bodyContext))
      return err;
  } else
  if (isDefault)
    return codeLoc.getError("Cannot generate a default body for this function");
  return none;
}

optional<ErrorLoc> FunctionDefinition::checkBody(const vector<SFunctionInfo>& requirements,
    StatementBlock& myBody,  const FunctionInfo& instanceInfo) const {
  auto bodyContext = Context::withParent(*definitionContext);
  for (auto& f : requirements) {
    if (!contains(bodyContext.getFunctions(f->id), f->getParent()))
      CHECK(!bodyContext.addFunction(f));
  }
  bool isTemplated = false;
  for (auto& t : instanceInfo.type.templateParams)
    if (!t->getMangledName()) {
      bodyContext.setTemplated();
      isTemplated = true;
      break;
    }
  for (int i = 0; i < instanceInfo.type.params.size(); ++i) {
    auto& p = instanceInfo.type.params[i];
    auto type = p.type;
    if (name.contains<Operator>())
      type = convertReferenceToPointer(type);
    if (auto name = instanceInfo.getParamName(i)) {
      auto varType = parameters[min(i, parameters.size() -1)].isMutable
          ? SType(MutableReferenceType::get(type)) : SType(ReferenceType::get(type));
      if (!isVariadicParams || i < instanceInfo.type.params.size() - 1 || !isTemplated)
        bodyContext.addVariable(*name, varType);
      else
        bodyContext.addVariablePack(*name, varType);
    }
  }
  auto retVal = instanceInfo.type.retVal;
  if (name.contains<Operator>())
    retVal = convertReferenceToPointer(retVal);
  ReturnTypeChecker returnChecker(retVal);
  bodyContext.addReturnTypeChecker(&returnChecker);
  if (auto err = myBody.check(bodyContext))
    return err;
  if (retVal == ArithmeticType::NORETURN && !myBody.hasReturnStatement(bodyContext))
    return codeLoc.getError("This function should never return");
  if (retVal != ArithmeticType::VOID && !myBody.hasReturnStatement(bodyContext))
    return codeLoc.getError("Not all paths lead to a return statement in a function returning non-void");
  return none;
}

optional<ErrorLoc> FunctionDefinition::checkForIncompleteTypes(const Context& context) {
  for (int i = 0; i < functionInfo->type.params.size(); ++i) {
    auto paramType = functionInfo->type.params[i].type;
    if (auto error = paramType->getSizeError(context))
      return parameters[i].codeLoc.getError(*error);
  }
  if (auto error = functionInfo->type.retVal->getSizeError(context))
    return returnType.codeLoc.getError(*error);
  return none;
}

optional<ErrorLoc> FunctionDefinition::check(Context& context, bool notInImport) {
  if (auto err = generateDefaultBodies(context))
    return err;
  if (body && (!templateInfo.params.empty() || notInImport)) {
    Context paramsContext = Context::withParent(context);
    addTemplateParams(paramsContext, functionInfo->type.templateParams, functionInfo->type.variadicTemplate);
    auto res = applyConcept(paramsContext, templateInfo);
    if (!res)
      return res.get_error();
    if (auto err = checkForIncompleteTypes(paramsContext))
      return err;
    // For checking the template we use the Context that includes the template params.
    definitionContext.emplace(Context::withParent(paramsContext));
    if (auto err = checkBody({}, *body, *functionInfo))
      return err;
    // For checking instances we just use the top level context.
    definitionContext.emplace(Context::withParent(context));
    for (int i = 0; i < instances.size(); ++i)
      if (!instances[i].body) {
        if (auto err = instances[i].generateBody(body.get(), codeLoc))
          return *err;
        if (auto err = checkBody(instances[i].requirements, *instances[i].body,
            *instances[i].functionInfo))
          return err;
      }
    MoveChecker moveChecker;
    for (auto& p : parameters)
      if (p.name)
        moveChecker.addVariable(*p.name);
    return body->checkMoves(moveChecker);
  }
  return none;
}

optional<ErrorLoc> FunctionDefinition::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  if (auto err = setFunctionType(context, false, cache.isCurrentlyBuiltIn()))
    return err;
  if (auto err = context.addFunction(functionInfo.get()))
    return codeLoc.getError(*err);
  return none;
}

static void addBuiltInConcepts(Context& context) {
  auto addType = [&context](const char* name, SType type) {
    shared_ptr<Concept> concept = shared<Concept>(name, Context(context.typeRegistry), false);
    concept->modParams().push_back(shared<TemplateParameterType>(type, "T", CodeLoc()));
    context.addConcept(name, concept);
  };
  addType("is_enum", ArithmeticType::ENUM_TYPE);
  addType("is_struct", ArithmeticType::STRUCT_TYPE);
}

Context createPrimaryContext(TypeRegistry* typeRegistry) {
  Context context(typeRegistry);
  for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE, ArithmeticType::BOOL,
       ArithmeticType::VOID, ArithmeticType::CHAR, ArithmeticType::STRING, ArithmeticType::NULL_TYPE})
    context.addType(type->getName(), type);
  CHECK(!context.addImplicitFunction(Operator::PLUS, FunctionType(ArithmeticType::STRING,
      {{ArithmeticType::STRING}, {ArithmeticType::STRING}}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS_UNARY, Operator::MINUS_UNARY})
    for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE})
      CHECK(!context.addImplicitFunction(op, FunctionType(type, {{type}}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT, Operator::DECREMENT})
    CHECK(!context.addImplicitFunction(op, FunctionType(ArithmeticType::VOID,
        {{MutableReferenceType::get(ArithmeticType::INT)}}, {}).setBuiltin()));
  for (auto op : {Operator::PLUS, Operator::MINUS, Operator::MULTIPLY, Operator::DIVIDE, Operator::MODULO})
    for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE})
      if (type != ArithmeticType::DOUBLE || op != Operator::MODULO)
        CHECK(!context.addImplicitFunction(op, FunctionType(type, {{type}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::INCREMENT_BY, Operator::DECREMENT_BY, Operator::MULTIPLY_BY, Operator::DIVIDE_BY})
    for (auto type : {ArithmeticType::INT, ArithmeticType::DOUBLE})
      CHECK(!context.addImplicitFunction(op, FunctionType(ArithmeticType::VOID,
          {{MutableReferenceType::get(type)}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::LOGICAL_AND, Operator::LOGICAL_OR})
    CHECK(!context.addImplicitFunction(op, FunctionType(ArithmeticType::BOOL,
        {{ArithmeticType::BOOL}, {ArithmeticType::BOOL}}, {}).setBuiltin()));
  CHECK(!context.addImplicitFunction(Operator::LOGICAL_NOT, FunctionType(ArithmeticType::BOOL,
      {{ArithmeticType::BOOL}}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS, Operator::LESS_THAN, Operator::MORE_THAN})
    for (auto type : {ArithmeticType::INT, ArithmeticType::STRING, ArithmeticType::DOUBLE})
      CHECK(!context.addImplicitFunction(op, FunctionType(ArithmeticType::BOOL, {{type}, {type}}, {}).setBuiltin()));
  for (auto op : {Operator::EQUALS})
    for (auto type : {ArithmeticType::BOOL, ArithmeticType::CHAR})
      CHECK(!context.addImplicitFunction(op, FunctionType(ArithmeticType::BOOL, {{type}, {type}}, {}).setBuiltin()));
  auto metaTypes = {ArithmeticType::ANY_TYPE, ArithmeticType::STRUCT_TYPE, ArithmeticType::ENUM_TYPE};
  for (auto type1 : metaTypes)
    for (auto type2 : metaTypes)
      CHECK(!context.addImplicitFunction(Operator::EQUALS, FunctionType(ArithmeticType::BOOL, {{type1}, {type2}}, {}).setBuiltin()));
  addBuiltInConcepts(context);
  context.addBuiltInFunction("enum_count", ArithmeticType::INT, {SType(ArithmeticType::ENUM_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto enumType = args[0].dynamicCast<EnumType>())
          return (SType) CompileTimeValue::get((int) enumType->elements.size());
        else
          fail();
      });
  context.addBuiltInFunction("struct_count", ArithmeticType::INT, {SType(ArithmeticType::STRUCT_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto structType = args[0].dynamicCast<StructType>())
          return (SType) CompileTimeValue::get((int) structType->members.size());
        else
          fail();
      });
  context.addBuiltInFunction("string_length", ArithmeticType::INT, {SType(ArithmeticType::STRING)},
      [](vector<SType> args) -> WithError<SType> {
        if (auto value = args[0].dynamicCast<CompileTimeValue>())
          if (auto s = value->value.getReferenceMaybe<string>())
            return (SType) CompileTimeValue::get((int) s->size());
        fail();
      });
  context.addBuiltInFunction("enum_strings", ArrayType::get(ArithmeticType::STRING, CompileTimeValue::get(0)),
          {SType(ArithmeticType::ENUM_TYPE)},
      [](vector<SType> args) -> WithError<SType> {
        auto enumType = args[0].dynamicCast<EnumType>();
        vector<SCompileTimeValue> values;
        for (auto& elem : enumType->elements)
          values.push_back(CompileTimeValue::get(elem));
        return (SType) CompileTimeValue::get(CompileTimeValue::ArrayValue{values, ArithmeticType::STRING});
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

static optional<ErrorLoc> addExportedContext(const Context& primaryContext, ImportCache& cache, AST& ast,
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
      if (auto err = elem->addToContext(importContext, cache, primaryContext))
        return *err;
      elem->addGeneratedConstructor(importContext, ast);
    }
  }
  for (auto& elem : ast.elems)
    if (elem->exported)
      if (auto err = elem->check(importContext))
        return *err;
  cache.popCurrentImport(isBuiltIn);
  cache.insert(path, std::move(importContext), isBuiltIn || cache.isCurrentlyBuiltIn());
  return none;
}

WithErrorLine<vector<ModuleInfo>> correctness(const string& path, AST& ast, Context& context, const Context& primaryContext,
    const vector<string>& importPaths, bool isBuiltInModule) {
  ImportCache cache(isBuiltInModule);
  if (auto err = addExportedContext(primaryContext, cache, ast, path, isBuiltInModule, importPaths))
    return *err;
  context.merge(cache.getContext(path));
  for (auto& elem : ast.elems)
    if (!elem->exported) {
      if (auto err = elem->addToContext(context, cache, primaryContext))
        return *err;
      elem->addGeneratedConstructor(context, ast);
    }
  for (auto& elem : ast.elems) {
    if (auto err = elem->check(context, true))
      return *err;
  }
  return cache.getAllImports();
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

optional<ErrorLoc> ExpressionStatement::check(Context& context, bool) {
  auto res = getType(context, expr);
  if (!res)
    return res.get_error();
  noReturnExpr = res.get() == ArithmeticType::NORETURN;
  if (!canDiscard && res.get() != ArithmeticType::VOID && res.get() != ArithmeticType::NORETURN)
    return codeLoc.getError("Expression result of type " + quote(res->get()->getName()) + " discarded");
  if (canDiscard && (res.get() == ArithmeticType::VOID || res.get() == ArithmeticType::NORETURN))
    return codeLoc.getError("Void expression result unnecessarily marked as discarded");
  return none;
}

unique_ptr<Statement> ExpressionStatement::transform(const StmtTransformFun&, const ExprTransformFun& fun) const {
  auto ret = unique<ExpressionStatement>(fun(expr.get()));
  ret->canDiscard = canDiscard;
  ret->noReturnExpr = noReturnExpr;
  return ret;
}

optional<ErrorLoc> ExpressionStatement::checkMovesImpl(MoveChecker& checker) const {
  return expr->checkMoves(checker);
}

bool ExpressionStatement::hasReturnStatement(const Context& context) const {
  return noReturnExpr;
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

optional<ErrorLoc> FunctionCall::initializeTemplateArgsAndIdentifierType(const Context& context) {
  if (!templateArgs) {
    if (auto res = context.getTypeList(identifier->parts.back().templateArguments, variadicTemplateArgs))
      templateArgs = res.get();
    else
      return res.get_error();
  }
  if (!identifierType) {
    if (auto res = context.getIdentifierType(*identifier))
      identifierType = *res;
    else
      return identifier->codeLoc.getError(res.get_error());
  }
  return none;
}

optional<ErrorLoc> FunctionCall::checkNamedArgs() const {
  for (int i = 0; i < argNames.size(); ++i) {
    auto paramName = functionInfo->getParamName(callType ? (i + 1) : i);
    if (argNames[i] && paramName && argNames[i] != paramName) {
      return arguments[i]->codeLoc.getError("Function argument " + quote(*argNames[i]) +
          " doesn't match parameter " + quote(*paramName) + " of function " +
          functionInfo->prettyString());
    }
  }
  return none;
}

optional<ErrorLoc> FunctionCall::checkVariadicCall(const Context& callContext) {
  if ((!variadicTemplateArgs && !variadicArgs))
    return none;
  auto context = Context::withParent(callContext);
  nullable<SType> returnType;
  vector<SType> expanded;
  vector<string> expandedVars;
  while (true) {
    auto call = cast<FunctionCall>(deepCopy());
    if (variadicTemplateArgs)
      call = cast<FunctionCall>(expand(templateArgs->back(), expanded));
    auto& variablePack = callContext.getVariablePack();
    if (variablePack)
      call = cast<FunctionCall>(call->expandVar(variablePack->name, expandedVars));
    if (auto type = call->getTypeImpl(context)) {
      if (!!returnType && returnType != type.get())
        return codeLoc.getError("Return type mismatch between called functions in variadic function call");
      returnType = type.get();
      // Check if we found a function that we can call with any arguments / template arguments expansion.
      // The condition might have to be improved if some corner cases fail
      if (call->functionInfo->type.variadicTemplate == this->functionInfo->type.variadicTemplate &&
          call->functionInfo->type.variadicParams == this->functionInfo->type.variadicParams)
        break;
    } else
      return type.get_error();
    expanded.push_back(shared<TemplateParameterType>(getExpandedParamName(
        callContext.getTypePack()->getName(), expanded.size()), codeLoc));
    // Go through all functions coming from a concept and try to expand them from the current type pack into 'expanded'.
    // Otherwise one of the types in 'expanded' might fail a requirement in the checked call
    for (auto& fun : callContext.getAllFunctions())
      if (fun->type.fromConcept) {
        ErrorBuffer errors;
        auto replaced = replaceInFunction(fun, callContext.getTypePack().get(), expanded.back(), errors);
        if (errors.empty() && replaced != fun)
          ignore(context.addFunction(replaced));
        CHECK(errors.empty()); // not sure if any errors should appear here, so checking just in case
      }
    if (variablePack) {
      expandedVars.push_back(getExpandedParamName("SomeVar", expanded.size()));
      ErrorBuffer errors;
      context.addVariable(expandedVars.back(),
          variablePack->type->replace(callContext.getTypePack().get(), expanded.back(), errors));
      if (!errors.empty())
        return codeLoc.getError(errors[0]);
    }
  }
  return none;
}

WithErrorLine<SType> FunctionCall::getTypeImpl(Context& callContext) {
  if (auto error = initializeTemplateArgsAndIdentifierType(callContext))
    return *error;
  optional<ErrorLoc> error;
  if (!functionInfo) {
    vector<SType> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = 0; i < arguments.size(); ++i) {
      auto context = Context::withParent(callContext);
      if (variadicArgs && i == arguments.size() - 1) {
        if (auto& currentPack = context.getVariablePack())
          context.expandVariablePack({currentPack->name});
        else
          return arguments[i]->codeLoc.getError("No parameter pack is present in this context");
      }
      if (auto t = getType(context, arguments[i]))
        argTypes.push_back(*t);
      else
        return t.get_error();
      argLocs.push_back(arguments[i]->codeLoc);
      INFO << "Function argument " << argTypes.back()->getName();
    }
    auto tryMethodCall = [&](MethodCallType thisCallType) {
      auto res = getFunction(callContext, codeLoc, *identifierType, *templateArgs, argTypes, argLocs);
      if (res)
        callType = thisCallType;
      if (res && functionInfo) {
        error = codeLoc.getError("Ambigous method call:\nCandidate: " + functionInfo->prettyString() +
            "\nCandidate: " + res.get()->prettyString());
        functionInfo = nullptr;
      } else
        res.unpack(functionInfo, error);
    };
    if (methodCall) {
      auto leftType = argTypes[0];
      if (!leftType->getUnderlying().dynamicCast<PointerType>() &&
          !leftType->getUnderlying().dynamicCast<MutablePointerType>())
        tryMethodCall(MethodCallType::FUNCTION_AS_METHOD);
      argTypes[0] = leftType.dynamicCast<MutableReferenceType>()
          ? SType(MutablePointerType::get(leftType->getUnderlying()))
          : SType(PointerType::get(leftType->getUnderlying()));
      tryMethodCall(MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER);
    } else
      getFunction(callContext, codeLoc, *identifierType, *templateArgs, argTypes, argLocs).unpack(functionInfo, error);
  }
  if (functionInfo) {
    if (auto error = checkVariadicCall(callContext))
      return *error;
    if (auto error = functionInfo->type.retVal->getSizeError(callContext))
      return codeLoc.getError(*error);
    if (auto error = checkNamedArgs())
      return *error;
    if (auto parent = functionInfo->parent)
      if (parent->definition)
        if (auto error = parent->definition->addInstance(callContext, functionInfo.get()))
          return codeLoc.getError("When instantiating template " + parent->prettyString() + " as " + functionInfo->prettyString()  + ":\n"
            + error->loc.toString() + ": " + error->error);
    return functionInfo->type.retVal;
  }
  return *error;
}

optional<EvalResult> FunctionCall::eval(const Context& context) const {
  if (identifier)
    if (auto name = identifier->asBasicIdentifier()) {
      vector<SType> args;
      vector<CodeLoc> locs;
      for (auto& e : arguments) {
        locs.push_back(e->codeLoc);
        if (auto res = e->eval(context))
          args.push_back(res->value);
        else
          return res;
      }
      if (auto res = context.invokeFunction(*name, codeLoc, std::move(args), std::move(locs)))
        return EvalResult{res.get(), true};
    }
  return none;
}

unique_ptr<Expression> FunctionCall::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  auto ret = cast<FunctionCall>(transform(
      [from, to, &errors](Statement* st) { return st->replace(from, to, errors); },
      [from, to, &errors](Expression* expr) { return expr->replace(from, to, errors); }));
  ret->templateArgs.emplace();
  ErrorBuffer errors2;
  for (auto& arg : *templateArgs)
    ret->templateArgs->push_back(arg->replace(from, to, errors2));
  ret->identifierType = identifierType->replace(from, to, errors2);
  merge(errors, errors2, codeLoc);
  return ret;
}

unique_ptr<Expression> FunctionCall::expand(SType from, vector<SType> to) const {
  auto ret = cast<FunctionCall>(transform(
      [from, to](Statement* expr) { return expr->expand(from, to); },
      [from, to](Expression* expr) { return expr->expand(from, to); }));
  ret->templateArgs.emplace();
  for (auto& arg : *templateArgs) {
    if (arg == from)
      append(*ret->templateArgs, to);
    else
      ret->templateArgs->push_back(arg);
  }
  ret->variadicTemplateArgs = false;
  return ret;
}

unique_ptr<Expression> FunctionCall::replaceVar(string from, string to) const {
  auto ret = cast<FunctionCall>(transform(
      [from, to](Statement* expr) { return expr->replaceVar(from, to); },
      [from, to](Expression* expr) { return expr->replaceVar(from, to); }));
  ret->argNames = ::transform(argNames, [&](auto& n) -> optional<string> { if (n == from) return to; else return n; });
  return ret;
}

unique_ptr<Expression> FunctionCall::expandVar(string from, vector<string> to) const {
  auto ret = cast<FunctionCall>(transform(
      [from, to](Statement* expr) { return expr->expandVar(from, to); },
      [from, to](Expression* expr) { return expr->expandVar(from, to); }));
  if (variadicArgs) {
    auto lastArg = std::move(ret->arguments.back());
    ret->arguments.pop_back();
    ret->argNames.pop_back();
    for (auto& toName : to) {
      ret->arguments.push_back(lastArg->replaceVar(from, toName));
      ret->argNames.push_back(none);
    }
  }
  ret->variadicArgs = false;
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

optional<ErrorLoc> FunctionCall::checkMoves(MoveChecker& checker) const {
  for (auto& e : arguments)
    if (auto error = e->checkMoves(checker))
      return error;
  return none;
}

FunctionCall::FunctionCall(CodeLoc l, bool methodCall, Private) : Expression(l), methodCall(methodCall) {}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

optional<ErrorLoc> SwitchStatement::check(Context& context, bool) {
  if (auto t = getType(context, expr))
    return t.get()->handleSwitchStatement(*this, context, Type::SwitchArgument::VALUE);
  else
    return t.get_error();
}

optional<ErrorLoc> SwitchStatement::checkMovesImpl(MoveChecker& checker) const {
  if (auto error = expr->checkMoves(checker))
    return error;
  checker.startBlock();
  for (auto& elem : caseElems) {
    checker.newAlternative();
    checker.startBlock();
    if (elem.declaredVar)
      checker.addVariable(*elem.declaredVar);
    if (auto err = elem.block->checkMoves(checker)) {
      checker.endBlock();
      checker.endBlock();
      return err;
    }
    checker.endBlock();
  }
  if (defaultBlock) {
    checker.newAlternative();
    if (auto err = defaultBlock->checkMoves(checker)) {
      checker.endBlock();
      return err;
    }
  }
  checker.endBlock();
  return none;
}

unique_ptr<Statement> SwitchStatement::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  auto ret = unique<SwitchStatement>(codeLoc, expr->replace(from, to, errors));
  ErrorBuffer errors2;
  ret->targetType = targetType->replace(from, to, errors2);
  merge(errors, errors2, codeLoc);
  if (defaultBlock)
    ret->defaultBlock = cast<StatementBlock>(defaultBlock->replace(from, to, errors));
  for (auto& elem : caseElems)
    ret->caseElems.push_back(elem.replace(from, to, errors));
  return ret;
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

VariantDefinition::VariantDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

static WithErrorLine<shared_ptr<StructType>> getNewOrIncompleteStruct(Context& context, string name, CodeLoc codeLoc,
    const TemplateInfo& templateInfo, bool incomplete) {
  if (auto existing = context.getType(name)) {
    auto asStruct = existing.get().dynamicCast<StructType>();
    if (context.isFullyDefined(existing.get().get()) || !asStruct)
      // if it's not an incomplete struct type then this returns a conflict error
      return codeLoc.getError(*context.checkNameConflict(name, "Type"));
    return asStruct;
  } else {
    if (auto err = context.checkNameConflict(name, "Type"))
      return codeLoc.getError(*err);
    auto paramsContext = Context::withParent(context);
    auto returnType = context.typeRegistry->getStruct(name);
    auto paramTypes = getTemplateParams(templateInfo, context);
    if (!paramTypes)
      return paramTypes.get_error();
    for (auto& t : *paramTypes)
      returnType->templateParams.push_back(std::move(t));
    context.addType(name, returnType, !incomplete);
    return returnType;
  }
}

static optional<string> getRedefinitionError(const string& typeName, const optional<CodeLoc>& definition) {
  if (definition)
    return "Type " + quote(typeName) + " has already been defined at " +
        definition->file + ", line " + to_string(definition->line);
  else
    return none;
}

optional<ErrorLoc> VariantDefinition::addToContext(Context& context) {
  auto res = getNewOrIncompleteStruct(context, name, codeLoc, templateInfo, false);
  if (!res)
    return res.get_error();
  type = *res;
  context.setFullyDefined(type.get().get(), true);
  if (auto err = getRedefinitionError(type->getName(), type->definition))
    return codeLoc.getError(*err);
  type->definition = codeLoc;
  auto membersContext = Context::withParent(context);
  if (templateInfo.params.size() != type->templateParams.size())
    return codeLoc.getError("Number of template parameters differs from forward declaration");
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  if (auto res = applyConcept(membersContext, templateInfo))
    type->requirements = *res;
  else
    return res.get_error();
  unordered_set<string> subtypeNames;
  for (auto& subtype : elements) {
    if (subtypeNames.count(subtype.name))
      return subtype.codeLoc.getError("Duplicate variant alternative: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    if (auto t = membersContext.getTypeFromString(subtype.type))
      type->alternatives.push_back({subtype.name, *t});
    else
      return t.get_error();
    vector<FunctionType::Param> params;
    if (auto subtypeInfo = membersContext.getTypeFromString(subtype.type)) {
      if (*subtypeInfo != ArithmeticType::VOID)
        params.push_back(FunctionType::Param{*subtypeInfo});
    } else
      return subtypeInfo.get_error();
    auto constructor = FunctionType(type.get(), params, {});
    constructor.parentType = type.get();
    CHECK(!type->staticContext.addImplicitFunction(subtype.name, constructor));
  }
  return none;
}

optional<ErrorLoc> VariantDefinition::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  for (auto& param : type->templateParams)
    bodyContext.addType(param->getName(), param);
  CHECK(!!applyConcept(bodyContext, templateInfo));
  type->updateInstantations();
  return none;
}

optional<ErrorLoc> StructDefinition::addToContext(Context& context) {
  if (auto typeTmp = getNewOrIncompleteStruct(context, name, codeLoc, templateInfo, incomplete))
    type = *typeTmp;
  else
    return typeTmp.get_error();
  if (!incomplete) {
    if (auto err = getRedefinitionError(type->getName(), type->definition))
      return codeLoc.getError(*err);
    type->definition = codeLoc;
    context.setFullyDefined(type.get().get(), true);
    if (templateInfo.params.size() != type->templateParams.size())
      return codeLoc.getError("Number of template parameters of type " + quote(type->getName()) +
          " differs from forward declaration.");
    auto membersContext = Context::withParent(context);
    addTemplateParams(membersContext, type->templateParams, false);
    if (auto res = applyConcept(membersContext, templateInfo))
      type->requirements = *res;
    else
      return res.get_error();
    for (auto& member : members)
      if (auto memberType = membersContext.getTypeFromString(member.type))
        type->members.push_back({member.name, *memberType});
      else
        return memberType.get_error();
    for (auto& member : members)
      if (auto error = type->members.back().type->getSizeError(membersContext))
        return member.codeLoc.getError(*error);
    for (int i = 0; i < members.size(); ++i)
      for (int j = i + 1; j < members.size(); ++j)
        if (members[i].name == members[j].name)
          return members[j].codeLoc.getError("Duplicate member: " + quote(members[j].name));
    type->external = external;
  }
  return none;
}

optional<ErrorLoc> StructDefinition::check(Context& context, bool notInImport) {
  auto methodBodyContext = Context::withParent(context);
  addTemplateParams(methodBodyContext, type->templateParams, false);
  CHECK(!!applyConcept(methodBodyContext, templateInfo));
  type->updateInstantations();
  if (!incomplete)
    if (auto error = type->getSizeError(context))
      return codeLoc.getError(*error);
  return none;
}

void StructDefinition::addGeneratedConstructor(Context& context, const AST& ast) const {
  if (!incomplete) {
    bool hasUserDefinedConstructors = false;
    for (auto& elem : ast.elems)
      if (auto functionDef = dynamic_cast<const FunctionDefinition*>(elem.get()))
        if (functionDef->name.contains<ConstructorTag>() && functionDef->returnType.parts[0].name == name)
          hasUserDefinedConstructors = true;
    if (!external) {
      vector<FunctionType::Param> constructorParams;
      for (auto& member : type->members)
        constructorParams.push_back({member.name, member.type});
      auto fun = FunctionType(type.get(), std::move(constructorParams), type->templateParams);
      fun.generatedConstructor = true;
      if (!hasUserDefinedConstructors)
        CHECK(!context.addImplicitFunction(ConstructorTag{}, fun));
      fun.templateParams.clear();
      fun.parentType = type.get();
      CHECK(!type->getStaticContext().addImplicitFunction(ConstructorTag{}, fun));
      type->getStaticContext().addType(name, type.get());
    }
  }
}

MoveExpression::MoveExpression(CodeLoc l, string id) : Expression(l), identifier(id) {
}

WithErrorLine<SType> MoveExpression::getTypeImpl(Context& context) {
  if (!type) {
    if (auto ret = context.getTypeOfVariable(identifier)) {
      if (context.isCapturedVariable(identifier))
        return codeLoc.getError("Can't move from a captured value");
      if (!ret.get_value().dynamicCast<MutableReferenceType>() &&
          !ret.get_value().dynamicCast<ReferenceType>())
        return codeLoc.getError("Can't move from " + quote(ret.get_value()->getName()));
      type = ret.get_value()->getUnderlying();
    } else
      return codeLoc.getError(ret.get_error());
  }
  return type.get();
}

unique_ptr<Expression> MoveExpression::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  auto ret = unique<MoveExpression>(codeLoc, identifier);
  ErrorBuffer errors2;
  ret->type = type->replace(from, to, errors2);
  merge(errors, errors2, codeLoc);
  return ret;
}

unique_ptr<Expression> MoveExpression::replaceVar(string from, string to) const {
  if (from == identifier)
    return unique<MoveExpression>(codeLoc, to);
  else
    return deepCopy();
}

unique_ptr<Expression> MoveExpression::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<MoveExpression>(codeLoc, identifier);
}

optional<ErrorLoc> MoveExpression::checkMoves(MoveChecker& checker) const {
  if (auto err = checker.moveVariable(codeLoc, identifier))
    return codeLoc.getError(*err);
  return none;
}

EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

optional<ErrorLoc> EmbedStatement::check(Context&, bool) {
  return none;
}

Statement::TopLevelAllowance EmbedStatement::allowTopLevel() const {
  return TopLevelAllowance::CAN;
}

unique_ptr<Statement> EmbedStatement::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  auto ret = unique<EmbedStatement>(codeLoc, value);
  ret->replacements = replacements;
  ret->replacements.push_back({from, to});
  return ret;
}

bool EmbedStatement::hasReturnStatement(const Context&) const {
  return true;
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<Statement> i, unique_ptr<Expression> c,
                                   unique_ptr<Expression> it, unique_ptr<Statement> b)
  : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

optional<ErrorLoc> ForLoopStatement::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  if (auto err = init->check(bodyContext))
    return *err;
  auto condType = getType(bodyContext, cond);
  if (!condType)
    return condType.get_error();
  if (*condType != ArithmeticType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  auto res = getType(bodyContext, iter);
  if (!res)
    return res.get_error();
  loopId = bodyContext.setIsInLoop();
  return body->check(bodyContext);
}

optional<ErrorLoc> ForLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  if (auto err = init->checkMoves(checker))
    return *err;
  checker.startLoop(loopId);
  if (auto err = cond->checkMoves(checker))
    return *err;
  if (auto err = iter->checkMoves(checker))
    return *err;
  if (auto err = body->checkMoves(checker)) {
    if (auto err = checker.endLoop(loopId))
      return err;
    return *err;
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

optional<ErrorLoc> StaticForLoopStatement::checkExpressions(const Context& bodyContext) const {
  auto context = Context::withParent(bodyContext);
  auto initVal = init->eval(context);
  if (!initVal)
    return init->codeLoc.getError("Unable to evaluate expression at compile time");
  context.addType(counter, CompileTimeValue::getReference(initVal->value.dynamicCast<CompileTimeValue>()));
  auto condVal = cond->eval(context);
  if (!condVal)
    return cond->codeLoc.getError("Unable to evaluate expression at compile time");
  if (condVal->value->getType()->getUnderlying() != ArithmeticType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote(ArithmeticType::BOOL->getName()));
  auto iterVal = iter->eval(context);
  if (!iterVal)
    return iter->codeLoc.getError("Unable to evaluate expression at compile time");
  return none;
}

WithErrorLine<vector<unique_ptr<Statement>>> StaticForLoopStatement::getUnrolled(const Context& context, SType counterType) const {
  vector<unique_ptr<Statement>> ret;
  auto counterValue = CompileTimeValue::getReference(init->eval(context)->value.dynamicCast<CompileTimeValue>());
  const int maxIterations = 500;
  int countIter = 0;
  while (1) {
    auto evalContext = Context::withParent(context);
    evalContext.addType(counter, counterValue);
    auto condValue = cond->eval(evalContext)->value;
    if (!*condValue.dynamicCast<CompileTimeValue>()->value.getValueMaybe<bool>())
      break;
    auto currentCounter = counterValue->value.getValueMaybe<CompileTimeValue::ReferenceValue>()->value;
    auto replaceExpr = unique<Constant>(body->codeLoc, currentCounter);
    ErrorLocBuffer errors;
    ret.push_back(body->replace(counterType, currentCounter, errors));
    if (!errors.empty())
      return errors[0];
    if (auto err = ret.back()->check(evalContext))
      return *err;
    CHECK(iter->eval(evalContext)->value->getMangledName());
    if (++countIter >= maxIterations)
      return codeLoc.getError("Static loop reached maximum number of iterations (" + to_string(maxIterations) + ")");
  }
  return std::move(ret);
}

optional<ErrorLoc> StaticForLoopStatement::check(Context& context, bool) {
  if (auto err = context.checkNameConflict(counter, "variable"))
    return codeLoc.getError(*err);
  auto bodyContext = Context::withParent(context);
  if (auto err = checkExpressions(bodyContext))
    return *err;
  auto initType = getType(bodyContext, init);
  if (initType)
    bodyContext.addVariable(counter, MutableReferenceType::get(getType(bodyContext, init).get()));
  else
    return initType.get_error();
  auto condType = getType(bodyContext, cond);
  if (!condType)
    return condType.get_error();
  if (*condType != ArithmeticType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  auto res = getType(bodyContext, iter);
  if (!res)
    return res.get_error();
  auto counterType = CompileTimeValue::getTemplateValue(*initType, counter);
  bodyContext.addType(counter, counterType);
  auto bodyTemplateContext = Context::withParent(bodyContext);
  bodyTemplateContext.setTemplated();
  if (auto err = body->check(bodyTemplateContext))
    // If it's not a templated context then we will be unrolling the loop and find errors there
    // (it still needs to be checked to avoid replace() errors :()
    // This may potentially accept some bad code in some corner cases.
    if (bodyContext.isTemplated())
      return err;
  if (!bodyContext.isTemplated()) {
    if (auto res = getUnrolled(context, counterType))
      unrolled = std::move(*res);
    else
      return res.get_error();
  }
  return none;
}

optional<ErrorLoc> StaticForLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  static int loopId = 0;
  --loopId;
  checker.startLoop(loopId);
  if (auto err = body->checkMoves(checker)) {
    if (auto err = checker.endLoop(loopId))
      return err;
    return *err;
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

optional<ErrorLoc> WhileLoopStatement::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  auto condType = getType(bodyContext, cond);
  if (!condType)
    return condType.get_error();
  if (*condType != ArithmeticType::BOOL)
    return cond->codeLoc.getError("Loop condition must be of type " + quote("bool"));
  loopId = bodyContext.setIsInLoop();
  return body->check(bodyContext);
}

optional<ErrorLoc> WhileLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  checker.startLoop(loopId);
  if (auto err = cond->checkMoves(checker))
    return *err;
  if (auto err = body->checkMoves(checker)) {
    if (auto err = checker.endLoop(loopId))
      return err;
    return *err;
  }
  return checker.endLoop(loopId);
  return none;
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

optional<ErrorLoc> ImportStatement::check(Context&, bool) {
  return none;
}

optional<ErrorLoc> ImportStatement::processImport(const Context& primaryContext, Context& context, ImportCache& cache, const string& content,
    const string& path) {
  if (cache.isCurrentlyImported(path))
    return codeLoc.getError("Public import cycle: " + combine(cache.getCurrentImports(), ", "));
  if (!cache.contains(path)) {
    INFO << "Parsing import " << path;
    if (auto tokens = lex(content, CodeLoc(path, 0, 0), "end of file")) {
      if (auto parsed = parse(std::move(*tokens)))
        ast = unique<AST>(std::move(*parsed));
      else
        return parsed.get_error();
    } else
      return tokens.get_error();
    if (auto err = addExportedContext(primaryContext, cache, *ast, path, isBuiltIn, importDirs))
      return *err;
  } else
    INFO << "Import " << path << " already cached";
  context.merge(cache.getContext(path));
  return none;
}

optional<ErrorLoc> ImportStatement::addToContext(Context& context, ImportCache& cache, const Context& primaryContext) {
  INFO << "Resolving import " << path << " from " << codeLoc.file;
  for (auto importDir : concat({getParentPath(codeLoc.file)}, importDirs)) {
    INFO << "Trying directory " << importDir;
    auto importPath = fs::path(importDir)  / path;
    if (auto content = readFromFile(importPath.c_str())) {
      importPath = fs::canonical(importPath);
      INFO << "Imported file " << importPath;
      if (auto err = processImport(primaryContext, context, cache, content->value, importPath))
        return err;
      return none;
    }
  }
  return codeLoc.getError("Couldn't resolve import path: " + path);
}

optional<EvalResult> Expression::eval(const Context&) const {
  return none;
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

optional<ErrorLoc> EnumDefinition::addToContext(Context& context) {
  auto type = context.typeRegistry->getEnum(name);
  context.setFullyDefined(type.get(), fullyDefined);
  if (fullyDefined) {
    if (elements.empty())
      return codeLoc.getError("Enum requires at least one element");
    if (auto err = getRedefinitionError(type->getName(), type->definition))
      return codeLoc.getError(*err);
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
  return none;
}

optional<ErrorLoc> EnumDefinition::check(Context&, bool) {
  return none;
}

EnumConstant::EnumConstant(CodeLoc l, string name, string element) : Expression(l), enumName(name), enumElement(element) {
}

WithErrorLine<SType> EnumConstant::getTypeImpl(Context& context) {
  auto type = context.getTypeFromString(IdentifierInfo(enumName, codeLoc));
  if (!type)
    return type.get_error();
  if (auto enumType = type->dynamicCast<EnumType>()) {
    if (!context.isFullyDefined(enumType.get()))
      return codeLoc.getError("Enum type " + quote(enumType->getName()) + " elements are not known in this context");
    if (!contains(enumType->elements, enumElement))
      return codeLoc.getError(quote(enumElement) + " is not an element of enum " + quote(enumName));
  } else
    return codeLoc.getError(quote(type.get()->getName()) + " is not an enum type");
  return type;
}

optional<EvalResult> EnumConstant::eval(const Context& context) const {
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

unique_ptr<Expression> EnumConstant::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  auto ret = unique<EnumConstant>(codeLoc, enumName, enumElement);
  ErrorBuffer errors2;
  ret->enumType = enumType->replace(from, to, errors2);
  merge(errors, errors2, codeLoc);
  return ret;
}

unique_ptr<Expression> EnumConstant::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<EnumConstant>(codeLoc, enumName, enumElement);
}

ConceptDefinition::ConceptDefinition(CodeLoc l, string name) : Statement(l), name(name) {
}

optional<ErrorLoc> ConceptDefinition::addToContext(Context& context) {
  shared_ptr<Concept> concept = shared<Concept>(name, Context(context.typeRegistry), templateInfo.variadic);
  auto declarationsContext = Context::withParent(context);
  for (int i = 0; i < templateInfo.params.size(); ++i) {
    auto& param = templateInfo.params[i];
    concept->modParams().push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    declarationsContext.addType(param.name, concept->modParams().back(), true,
        templateInfo.variadic && i == templateInfo.params.size() - 1);
  }
  for (auto& function : functions) {
    if (function->isVirtual)
      return function->codeLoc.getError("Virtual functions are not allowed here");
    if (auto err = function->setFunctionType(declarationsContext, true))
      return err;
    if (auto err = function->check(declarationsContext))
      return err;
    if (auto err = concept->modContext().addFunction(function->functionInfo.get()))
      return function->codeLoc.getError(*err);
  }
  context.addConcept(name, concept);
  return none;
}

optional<ErrorLoc> ConceptDefinition::check(Context& context, bool) {
  return none;
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

optional<ErrorLoc> RangedLoopStatement::check(Context& context, bool) {
  auto bodyContext = Context::withParent(context);
  auto containerTypeTmp = getType(context, container);
  if (!containerTypeTmp)
    return containerTypeTmp.get_error();
  auto containerType = containerTypeTmp.get();
  if (containerType->getUnderlying() == containerType)
    containerType = ReferenceType::get(containerType);
  auto uniqueSufix = to_string(codeLoc.line) + "_" + to_string(codeLoc.column);
  containerName = "container"s + uniqueSufix;
  auto containerEndName = "container_end"s + uniqueSufix;
  bodyContext.addVariable(*containerName, containerType);
  containerEnd = unique<VariableDeclaration>(codeLoc, none, containerEndName,
      unique<FunctionCall>(codeLoc, IdentifierInfo("end", codeLoc),
          unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, unique<Variable>(IdentifierInfo(*containerName, codeLoc))), false));
  if (auto err = containerEnd->check(bodyContext))
    return err;
  init->initExpr = unique<FunctionCall>(codeLoc, IdentifierInfo("begin", codeLoc),
      unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, unique<Variable>(IdentifierInfo(*containerName, codeLoc))), false);
  init->isMutable = true;
  condition = BinaryExpression::get(codeLoc, Operator::NOT_EQUAL, unique<Variable>(IdentifierInfo(init->identifier, codeLoc)),
      unique<Variable>(IdentifierInfo(containerEndName, codeLoc)), false);
  increment = unique<UnaryExpression>(codeLoc, Operator::INCREMENT, unique<Variable>(IdentifierInfo(init->identifier, codeLoc)));
  if (auto err = init->check(bodyContext))
    return err;
  auto condType = getType(bodyContext, condition);
  if (!condType)
    return condType.get_error();
  if (*condType != ArithmeticType::BOOL)
    return codeLoc.getError("Equality comparison between iterators does not return type " + quote("bool"));
  auto incType = getType(bodyContext, increment);
  if (!incType)
    return incType.get_error();
  loopId = bodyContext.setIsInLoop();
  return body->check(bodyContext);
}

optional<ErrorLoc> RangedLoopStatement::checkMovesImpl(MoveChecker& checker) const {
  if (auto err = init->checkMoves(checker))
    return *err;
  if (auto err = container->checkMoves(checker))
    return *err;
  checker.startLoop(loopId);
  if (auto err = body->checkMoves(checker)) {
    if (auto err = checker.endLoop(loopId))
      return err;
    return *err;
  }
  return checker.endLoop(loopId);
  return none;
}

unique_ptr<Statement> RangedLoopStatement::transform(const StmtTransformFun& fun,
    const ExprTransformFun& exprFun) const {
  auto ret = unique<RangedLoopStatement>(codeLoc,
      cast<VariableDeclaration>(fun(init.get())),
      exprFun(container.get()),
      fun(body.get()));
  ret->condition = exprFun(condition.get());
  ret->increment = exprFun(increment.get());
  ret->containerName = containerName;
  ret->containerEnd = cast<VariableDeclaration>(fun(containerEnd.get()));
  return ret;
}

optional<ErrorLoc> BreakStatement::check(Context& context, bool) {
  if (auto id = context.getLoopId()) {
    loopId = *id;
    return none;
  } else
    return codeLoc.getError("Break statement outside of a loop");
}

unique_ptr<Statement> BreakStatement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  auto ret = unique<BreakStatement>(codeLoc);
  ret->loopId = loopId;
  return ret;
}

optional<ErrorLoc> BreakStatement::checkMovesImpl(MoveChecker& checker) const {
  checker.breakStatement(loopId);
  return none;
}

optional<ErrorLoc> ContinueStatement::check(Context& context, bool) {
  if (!context.getLoopId())
    return codeLoc.getError("Continue statement outside of a loop");
  return none;
}

unique_ptr<Statement> ContinueStatement::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<ContinueStatement>(codeLoc);
}

optional<ErrorLoc> ContinueStatement::checkMovesImpl(MoveChecker& checker) const {
  if (auto err = checker.continueStatement())
    return codeLoc.getError(*err);
  return none;
}

ArrayLiteral::ArrayLiteral(CodeLoc codeLoc) : Expression(codeLoc) {
}

WithErrorLine<SType> ArrayLiteral::getTypeImpl(Context& context) {
  auto typeTmp = typeId ? context.getTypeFromString(*typeId, false) : getType(context, contents[0]);
  if (!typeTmp)
    return typeTmp.get_error();
  auto ret = typeTmp.get()->getUnderlying();
  for (int i = 0; i < contents.size(); ++i) {
    if (i > 0 || !typeId) {
      typeTmp = getType(context, contents[i]);
      if (!typeTmp)
        return typeTmp.get_error();
    }
    if (auto error = getVariableInitializationError("construct array", context, ret, typeTmp.get()))
      return contents[i]->codeLoc.getError(*error);
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

optional<ErrorLoc> ArrayLiteral::checkMoves(MoveChecker& checker) const {
  for (auto& e : contents)
    if (auto error = e->checkMoves(checker))
      return error;
  return none;
}

WithErrorLine<SType> getType(Context& context, unique_ptr<Expression>& expr, bool evaluateAtCompileTime) {
  auto type = expr->getTypeImpl(context);
  if (!type)
    return type;
  if (evaluateAtCompileTime) {
    if (auto type = expr->eval(context)) {
      if (type->isConstant)
        if (auto value = type->value.dynamicCast<CompileTimeValue>())
          expr = unique<Constant>(expr->codeLoc, value);
    }
  }
  return type;
}

WithErrorLine<nullable<SType>> SwitchStatement::CaseElem::getType(const Context& context) {
  return type.visit(
      [&](const IdentifierInfo& id) -> WithErrorLine<nullable<SType>> {
        if (auto ret = context.getTypeFromString(id)) {
          type = ret.get();
          return nullable<SType>(ret.get());
        } else
          return ret.get_error();
      },
      [](const SType& t) -> WithErrorLine<nullable<SType>> { return nullable<SType>(t);},
      [](none_t) -> WithErrorLine<nullable<SType>> { return nullable<SType>(); }
  );
}

SwitchStatement::CaseElem SwitchStatement::CaseElem::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  CaseElem ret;
  ret.codeloc = codeloc;
  ret.ids = ids;
  ret.block = cast<StatementBlock>(block->replace(from, to, errors));
  ErrorBuffer errors2;
  if (auto t = type.getReferenceMaybe<SType>())
    ret.type = (*t)->replace(from, to, errors2);
  else
    ret.type = type;
  merge(errors, errors2, codeloc);
  return ret;
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

optional<ErrorLoc> ExternConstantDeclaration::addToContext(Context& context) {
  if (auto err = context.checkNameConflict(identifier, "Variable"))
    return codeLoc.getError(*err);
  if (auto t = context.getTypeFromString(type))
    realType = *t;
  else
    return t.get_error();
  if (realType == ArithmeticType::VOID)
    return codeLoc.getError("Can't declare constant of type " + quote(ArithmeticType::VOID->getName()));
  INFO << "Adding extern constant " << identifier << " of type " << realType.get()->getName();
  context.addVariable(identifier, ReferenceType::get(realType.get()));
  return none;
}

LambdaExpression::LambdaExpression(CodeLoc l, vector<FunctionParameter> params, unique_ptr<StatementBlock> block,
    optional<IdentifierInfo> returnType, vector<LambdaCaptureInfo> captures)
    : Expression(l), parameters(std::move(params)), block(std::move(block)), returnType(std::move(returnType)),
      captures(std::move(captures)) {
}

WithErrorLine<SType> LambdaExpression::getTypeImpl(Context& context) {
  if (type && !recheck)
    return SType(type.get());
  nullable<SType> retType;
  if (recheck)
    retType = type->functionInfo->type.retVal;
  else if (returnType) {
    if (auto res = context.getTypeFromString(*returnType))
      retType = *res;
    else
      return res.get_error();
  }
  auto bodyContext = Context::withParent(context);
  ReturnTypeChecker returnChecker(retType);
  bodyContext.addReturnTypeChecker(&returnChecker);
  auto captureTypes = bodyContext.setLambda(captures);
  if (!captureTypes)
    return captureTypes.get_error();
  vector<FunctionType::Param> params;
  if (!recheck) {
    type = shared<LambdaType>();
    type->captures = *captureTypes;
    for (auto& param : parameters)
      type->parameterNames.push_back(param.name);
    params.push_back(FunctionType::Param(PointerType::get(type.get())));
    set<string> paramNames;
    for (auto& param : parameters) {
      auto type = bodyContext.getTypeFromString(param.type);
      if (!type)
        return type.get_error();
      if (*type == ArithmeticType::VOID)
        return param.codeLoc.getError("Function parameter may not have " + quote(type->get()->getName()) + " type");
      auto varType = param.isMutable ? SType(MutableReferenceType::get(*type)) : SType(ReferenceType::get(*type));
      params.push_back({param.name, varType});
      if (param.name) {
        bodyContext.addVariable(*param.name, varType);
        if (paramNames.count(*param.name))
          return param.codeLoc.getError("Duplicate function parameter name: " + quote(*param.name));
        paramNames.insert(*param.name);
      }
    }
  } else
    for (int i = 1; i < type->functionInfo->type.params.size(); ++i) {
      auto& param = type->functionInfo->type.params[i];
      if (parameters[i - 1].name)
        bodyContext.addVariable(*parameters[i - 1].name, param.type);
    }
  recheck = false;
  auto bodyContext2 = Context::withParent(bodyContext);
  if (auto err = block->check(bodyContext2))
    return *err;
  if (!returnType)
    retType = returnChecker.getReturnType();
  if (!type->functionInfo) {
    FunctionType functionType(retType.get(), params, {});
    auto functioInfo = FunctionInfo::getImplicit("invoke"s, std::move(functionType));
    type->functionInfo = std::move(functioInfo);
  }
  if (auto err = checkBodyMoves())
    return *err;
  auto blockCopy = block->deepCopy();
  auto err = blockCopy->check(bodyContext);
  CHECK(!err) << err->loc.toString() << " " << err->error;
  if (!block->hasReturnStatement(context) && retType != ArithmeticType::VOID)
    return block->codeLoc.getError("Not all paths lead to a return statement in a lambda expression returning non-void");
  type->body = cast<StatementBlock>(std::move(blockCopy));
  context.typeRegistry->addLambda(type.get());
  context.addType(type->getName(), type.get());
  return SType(type.get());
}

optional<ErrorLoc> LambdaExpression::checkBodyMoves() const {
  MoveChecker moveChecker;
  for (auto& p : parameters)
    if (p.name)
      moveChecker.addVariable(*p.name);
  return block->checkMoves(moveChecker);
}

optional<ErrorLoc> LambdaExpression::checkMoves(MoveChecker& checker) const {
  for (auto& capture : captures) {
    if (capture.type == LambdaCaptureType::MOVE) {
      if (auto err = checker.moveVariable(capture.codeLoc, capture.name))
        return capture.codeLoc.getError(*err);
    } else
    if (auto err = checker.getUsageError(capture.name))
      return capture.codeLoc.getError(*err);
  }
  return none;
}

unique_ptr<Expression> LambdaExpression::transform(const StmtTransformFun& fun, const ExprTransformFun& exprFun) const {
  auto ret = unique<LambdaExpression>(codeLoc, parameters, cast<StatementBlock>(block->transform(fun, exprFun)), returnType, captures);
  ret->recheck = !!ret->type;
  return ret;
}

unique_ptr<Expression> LambdaExpression::replace(SType from, SType to, ErrorLocBuffer& errors) const {
  auto ret = unique<LambdaExpression>(codeLoc, parameters, cast<StatementBlock>(block->replace(from, to, errors)), returnType, captures);
  ErrorBuffer errors2;
  ret->type = type->replace(from, to, errors2).dynamicCast<LambdaType>();
  ret->recheck = true;
  merge(errors, errors2, codeLoc);
  return ret;
}

CountOfExpression::CountOfExpression(CodeLoc l, string id) : Expression(l), identifier(std::move(id)) {
}

WithErrorLine<SType> CountOfExpression::getTypeImpl(Context& context) {
  if (!count) {
    if (context.getTypeOfVariable(identifier))
      return codeLoc.getError("Expected variable or type pack");
    auto& variablePack = context.getVariablePack();
    if (!variablePack || variablePack->name != identifier) {
      if (auto t = context.getTypeFromString(IdentifierInfo(identifier, codeLoc), true))
        type = *t;
      else
        return t;
    }
  }
  return SType(ArithmeticType::INT);
}

unique_ptr<Expression> CountOfExpression::expand(SType from, vector<SType> to) const {
  if (type == from) {
    auto ret = unique<CountOfExpression>(codeLoc, identifier);
    ret->count = to.size();
    return std::move(ret);
  } else
    return deepCopy();
}

unique_ptr<Expression> CountOfExpression::expandVar(string from, vector<string> to) const {
  if (identifier == from) {
    auto ret = unique<CountOfExpression>(codeLoc, identifier);
    ret->count = to.size();
    return std::move(ret);
  } else
    return deepCopy();
}

unique_ptr<Expression> CountOfExpression::transform(const StmtTransformFun&, const ExprTransformFun&) const {
  return unique<CountOfExpression>(codeLoc, identifier);
}

optional<EvalResult> CountOfExpression::eval(const Context&) const {
  if (count)
    return EvalResult{CompileTimeValue::get(*count), true};
  else
    return EvalResult{CompileTimeValue::getTemplateValue(ArithmeticType::INT, "countof"), false};
}


VariablePackElement::VariablePackElement(CodeLoc l, string packName, vector<string> ids, SCompileTimeValue index)
    : Expression(l), packName(std::move(packName)), ids(std::move(ids)), index(std::move(index)) {
}

WithErrorLine<SType> VariablePackElement::getTypeImpl(Context& context) {
  if (!ids.empty())
    return context.getTypeOfVariable(ids[0]).addCodeLoc(codeLoc);
  else
    return codeLoc.getError("Unable to get type of empty variable pack. If this error surfaces then it's a bug.");
}

unique_ptr<Expression> VariablePackElement::replace(SType from, SType to, ErrorLocBuffer& buf) const {
  ErrorBuffer buf2;
  auto newIndex = index->replace(from, to, buf2).dynamicCast<CompileTimeValue>();
  merge(buf, buf2, codeLoc);
  if (auto val = newIndex->value.getValueMaybe<int>()) {
    if (*val < 0 || *val >= ids.size()) {
      buf.push_back(codeLoc.getError("Variable pack index out of bounds: " + to_string(*val) + ", with "
          + to_string(ids.size()) + " elements"));
      return deepCopy();
    }
    return unique<Variable>(IdentifierInfo(ids[*val], codeLoc));
  }
  return cast<Expression>(unique<VariablePackElement>(codeLoc, packName, ids, newIndex));
}

unique_ptr<Expression> VariablePackElement::transform(const StmtTransformFun& f1, const ExprTransformFun& f2) const {
  return cast<Expression>(unique<VariablePackElement>(codeLoc, packName, ids, index));
}

unique_ptr<MemberAccessExpression> MemberAccessExpression::getPointerAccess(CodeLoc l, unique_ptr<Expression> lhs, string id) {
  return unique<MemberAccessExpression>(l,
      unique<UnaryExpression>(l, Operator::POINTER_DEREFERENCE, std::move(lhs)),
      id);
}

MemberAccessExpression::MemberAccessExpression(CodeLoc l , unique_ptr<Expression> lhs, string id)
  : Expression(l), lhs(std::move(lhs)), identifier(id) { }

WithErrorLine<SType> MemberAccessExpression::getTypeImpl(Context& context) {
  if (auto leftType1 = lhs->getTypeImpl(context)) {
    auto leftType = leftType1.get();
    if (auto error = leftType->getSizeError(context))
      return codeLoc.getError(leftType->getName() + *error);
    if (auto member = leftType->getTypeOfMember(identifier))
      return *member;
    else
      return codeLoc.getError(member.get_error());
  } else
    return leftType1;
}

unique_ptr<Expression> MemberAccessExpression::transform(const StmtTransformFun& fun1, const ExprTransformFun& fun2) const {
  return unique<MemberAccessExpression>(codeLoc, lhs->transform(fun1, fun2), identifier);
}

optional<ErrorLoc> MemberAccessExpression::checkMoves(MoveChecker& checker) const {
  return lhs->checkMoves(checker);
}

MemberIndexExpression::MemberIndexExpression(CodeLoc l, unique_ptr<Expression> lhs, unique_ptr<Expression> index)
  : Expression(l), lhs(std::move(lhs)), index(std::move(index)) {}

WithErrorLine<SType> MemberIndexExpression::getTypeImpl(Context& context) {
  if (auto value = index->eval(context)) {
    if (auto leftType = lhs->getTypeImpl(context)) {
      if (auto error = leftType.get()->getSizeError(context))
        return codeLoc.getError(leftType.get()->getName() + *error);
      if (auto member = leftType.get()->getTypeOfMember(value->value)) {
        memberName = member->name;
        return member->type;
      } else
        return codeLoc.getError(member.get_error());
    } else
      return leftType;
  } else
    return index->codeLoc.getError("Unable to evaluate constant expression at compile-time");
}

unique_ptr<Expression> MemberIndexExpression::transform(const StmtTransformFun& fun1, const ExprTransformFun& fun2) const {
  return unique<MemberIndexExpression>(codeLoc, lhs->transform(fun1, fun2), index->transform(fun1, fun2));
}

optional<ErrorLoc> MemberIndexExpression::checkMoves(MoveChecker& checker) const {
  return lhs->checkMoves(checker);
}
