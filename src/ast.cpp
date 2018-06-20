#include "stdafx.h"
#include "ast.h"
#include "type.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"
#include "code_loc.h"

Node::Node(CodeLoc l) : codeLoc(l) {}


BinaryExpression::BinaryExpression(CodeLoc loc, Operator o, unique_ptr<Expression> u1, unique_ptr<Expression> u2)
    : Expression(loc), op(o) {
  e1 = std::move(u1);
  e2 = std::move(u2);
}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<VariableDeclaration> d, unique_ptr<Expression> c,
    unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), declaration(std::move(d)), condition(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, SType t, string v) : Expression(l), type(t), value(v) {
  INFO << "Created constant " << quote(v) << " of type " << t->getName();
}

Variable::Variable(CodeLoc l, string id) : Expression(l), identifier(id) {
  INFO << "Parsed variable " << id;
}

FunctionCall::FunctionCall(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {
  INFO << "Function call " << id.toString();;
}

VariableDeclaration::VariableDeclaration(CodeLoc l, optional<IdentifierInfo> t, string id, unique_ptr<Expression> ini)
    : Statement(l), type(t), identifier(id), initExpr(std::move(ini)) {
  string type = "auto";
  if (t)
    type = t->toString();
  INFO << "Declared variable " << quote(id) << " of type " << quote(type);
}

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, FunctionName name) : Statement(l), returnType(r), name(name) {}

SType Constant::getType(Context&) {
  return type;
}

SType Variable::getType(Context& context) {
  return context.getTypeOfVariable(identifier).get(codeLoc);
}

nullable<SType> Variable::getDotOperatorType(Expression* left, Context& callContext) {
  if (left)
    return getType(left->getType(callContext)->getContext());
  else
    return nullptr;
}

static bool isExactArg(SType arg, SType param) {
  bool byValue = param == arg->getUnderlying();
  bool byConstRef = param.dynamicCast<ReferenceType>() &&
      !arg.dynamicCast<MutableReferenceType>() &&
      param->getUnderlying() == arg->getUnderlying();
  bool byRef = param == arg;
  /*cout << "Passing " << arg->getName() << " to " << param->getName() << endl;
  if (byValue) cout << "By value "; if (byConstRef) cout << "By const ref "; if (byRef) cout << "By ref ";
  cout << endl;*/
  return byValue || byConstRef || byRef;
}

static bool exactArgs(const vector<SType>& argTypes, const FunctionType& f) {
  if (f.params.size() != argTypes.size())
    return false;
  for (int i = 0; i < f.params.size(); ++i)
    if (!isExactArg(argTypes[i], f.params[i].type))
      return false;
  return true;
}

static bool exactFirstArg(const vector<SType>& argTypes, const FunctionType& overload) {
  return !argTypes.empty() && !overload.params.empty() && isExactArg(argTypes[0], overload.params[0].type);
}

static bool nonConcept(const vector<SType>&, const FunctionType& f) {
  return !f.fromConcept;
}

static vector<FunctionType> filterOverloads(vector<FunctionType> overloads, const vector<SType>& argTypes) {
  auto filter = [&] (auto fun, const char* method) {
    auto worse = overloads;
    overloads.clear();
    for (auto& overload : worse)
      if (fun(argTypes, overload)) {
        overloads.push_back(overload);
        //cout << overload.toString() << " chosen by " << method << endl;
      }
    if (overloads.empty())
      overloads = worse;
  };
  filter(&exactArgs, "all args exact");
  filter(&exactFirstArg, "first arg exact");
  // sometimes a function is both in the global context and in the concept, so filter those in concepts
  filter(&nonConcept, "non concept");
  return overloads;
}

SType BinaryExpression::getType(Context& context) {
  if (op == Operator::POINTER_MEMBER_ACCESS) {
    e1 = unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE, std::move(e1));
    op = Operator::MEMBER_ACCESS;
  }
  auto& leftExpr = *e1;
  auto& rightExpr = *e2;
  auto left = leftExpr.getType(context);
  switch (op) {
    case Operator::POINTER_MEMBER_ACCESS:
      FATAL << "This was handled above";
      return left;
    case Operator::MEMBER_ACCESS: {
      if (auto rightType = rightExpr.getDotOperatorType(&leftExpr, context)) {
        if (!left.dynamicCast<ReferenceType>() && !left.dynamicCast<MutableReferenceType>())
          rightType = rightType->getUnderlying();
        else if (left.dynamicCast<ReferenceType>() && rightType.get().dynamicCast<MutableReferenceType>())
          rightType = ReferenceType::get(rightType->getUnderlying());
        return rightType.get();
      } else
        codeLoc.error("Bad use of operator " + quote("."));
    }
    default: {
      nullable<SType> ret;
      auto right = rightExpr.getType(context);
      for (auto fun : left->getContext().getOperatorType(op)) {
        if (auto inst = instantiateFunction(context, fun, codeLoc, {}, {right}, {codeLoc}, {})) {
          subscriptOpWorkaround = false;
          return inst->retVal;
        } else
          codeLoc.error("Can't apply operator: " + quote(getString(op)) + " to types: " +
              quote(left->getName()) + " and " + quote(right->getName()) + "\n"
              "Candidate: "s + fun.toString() + ": " + inst.get_error().error);
      }
      vector<FunctionType> overloads;
      for (auto fun : context.getOperatorType(op))
        if (auto inst = instantiateFunction(context, fun, codeLoc, {}, {left, right},
            {leftExpr.codeLoc, rightExpr.codeLoc}, {}))
          overloads.push_back(*inst);
      overloads = filterOverloads(overloads, {left, right});
      if (overloads.size() == 1) {
        //cout << "Chosen overload " << overloads[0].toString() << endl;
        return overloads[0].retVal;
      } else {
          string error = "No overload found for operator: " + quote(getString(op)) + " with argument types: " +
              quote(left->getName()) + " and " + quote(right->getName());
          for (auto& f : overloads)
            error += "\nCandidate: " + f.toString();
          codeLoc.error(error);
      }
    }
  }
}

void StatementBlock::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  for (auto& s : elems) {
    s->check(bodyContext);
  }
}

void IfStatement::check(Context& context) {
  auto ifContext = Context::withParent(context);
  if (declaration)
    declaration->check(ifContext);
  if (!condition) {
    auto negate = [&] (unique_ptr<Expression> expr) {
      return unique<UnaryExpression>(declaration->codeLoc, Operator::LOGICAL_NOT, std::move(expr));
    };
    condition = negate(negate(unique<Variable>(declaration->codeLoc, declaration->identifier)));
  }
  auto condType = condition->getType(ifContext);
  codeLoc.check(ifContext.canConvert(condType, ArithmeticType::BOOL),
      "Expected a type convertible to bool or with overloaded operator " +
      quote("!") + " inside if statement, got " + quote(condType->getName()));
  ifTrue->check(ifContext);
  if (ifFalse)
    ifFalse->check(ifContext);
}

void VariableDeclaration::check(Context& context) {
  context.checkNameConflict(codeLoc, identifier, "Variable");
  auto inferType = [&] () -> SType {
    if (type)
      return context.getTypeFromString(*type).get(codeLoc);
    else if (initExpr)
      return initExpr->getType(context)->getUnderlying();
    else
      codeLoc.error("Initializing expression needed to infer variable type");
  };
  realType = inferType();
  INFO << "Adding variable " << identifier << " of type " << realType.get()->getName();
  if (initExpr) {
    auto exprType = initExpr->getType(context);
    initExpr->codeLoc.check(context.canConvert(exprType, realType.get()), "Can't initialize variable of type "
        + quote(realType.get()->getName()) + " with value of type " + quote(exprType->getName()));
    initExpr->codeLoc.check((!exprType.dynamicCast<ReferenceType>() && !exprType.dynamicCast<MutableReferenceType>()) ||
        context.canCopyConstruct(exprType->getUnderlying()),
        "Type " + quote(exprType->getUnderlying()->getName()) + " cannot be copied");
  } else
    codeLoc.check(context.canConstructWith(realType.get(), {}), "Type " + quote(realType->getName()) + " requires initialization");
  auto varType = isMutable ? SType(MutableReferenceType::get(realType.get())) : SType(ReferenceType::get(realType.get()));
  context.addVariable(identifier, std::move(varType));
}

void ReturnStatement::check(Context& context) {
  if (!expr)
    codeLoc.check(context.getReturnType() && context.getReturnType() == ArithmeticType::VOID,
        "Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = expr->getType(context);
    codeLoc.check(context.canConvert(returnType, context.getReturnType().get()),
        "Can't return value of type " + quote(returnType->getName()) +
        " from a function returning " + context.getReturnType()->getName());
  }
}

void Statement::addToContext(Context&) {}

bool Statement::hasReturnStatement(const Context&) const {
  return false;
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

bool ReturnStatement::hasReturnStatement(const Context&) const {
  return true;
}

static vector<SConcept> applyConcept(Context& from, const TemplateInfo& templateInfo, const vector<SType>& templateTypes) {
  auto getTemplateParam = [&](CodeLoc codeLoc, const string& name) {
    if (auto type = from.getTypeFromString(IdentifierInfo(name)))
      return *type;
    for (int i = 0; i < templateInfo.params.size(); ++i)
      if (templateInfo.params[i].name == name)
        return templateTypes[i];
    codeLoc.error("Uknown type: " + quote(name));
  };
  vector<SConcept> ret;
  for (auto& requirement : templateInfo.requirements) {
    if (auto concept = from.getConcept(requirement.parts[0].name)) {
      auto& requirementArgs = requirement.parts[0].templateArguments;
      requirement.codeLoc.check(requirementArgs.size() == concept->getParams().size(),
          "Wrong number of template arguments to concept " + quote(requirement.parts[0].toString()));
      vector<SType> translatedParams;
      for (int i = 0; i < requirementArgs.size(); ++i)
        translatedParams.push_back(
            getTemplateParam(requirementArgs[i].codeLoc, requirementArgs[i].parts[0].name));
      auto translated = concept->translate(translatedParams);
      from.merge(translated->getContext());
      ret.push_back(translated);
    } else
      requirement.codeLoc.error("Uknown concept: " + requirement.parts[0].name);
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

void FunctionDefinition::setFunctionType(const Context& context, bool method) {
  if (auto s = name.getReferenceMaybe<string>())
    context.checkNameConflictExcludingFunctions(codeLoc, *s, "Function");
  else if (auto op = name.getValueMaybe<Operator>()) {
    codeLoc.check(canOverload(*op, (int) parameters.size() + (method ? 1 : 0)), "Can't overload operator " + quote(getString(*op)) +
        " with " + to_string(parameters.size()) + " arguments.");
  }
  Context contextWithTemplateParams = Context::withParent(context);
  vector<SType> templateTypes;
  for (auto& param : templateInfo.params) {
    templateTypes.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    contextWithTemplateParams.checkNameConflict(param.codeLoc, param.name, "template parameter");
    contextWithTemplateParams.addType(param.name, templateTypes.back());
  }
  auto requirements = applyConcept(contextWithTemplateParams, templateInfo, templateTypes);
  if (auto returnType1 = contextWithTemplateParams.getTypeFromString(this->returnType)) {
    auto returnType = *returnType1;
    if (name.contains<Operator>())
      returnType = convertPointerToReference(returnType);
    vector<FunctionType::Param> params;
    for (auto& p : parameters) {
      auto type = contextWithTemplateParams.getTypeFromString(p.type).get(p.codeLoc);
      if (name.contains<Operator>())
        type = convertPointerToReference(type);
      params.push_back({p.name, std::move(type)});
    }
    functionType = FunctionType(context.getFunctionId(name), FunctionCallType::FUNCTION, returnType, params, templateTypes);
    functionType->requirements = requirements;
  } else
    codeLoc.error(returnType1.get_error());
}

void FunctionDefinition::checkFunctionBody(Context& context, bool templateStruct) const {
  if (body && (!templateInfo.params.empty() || templateStruct || context.getCurrentImports().empty())) {
    Context bodyContext = Context::withParent(context);
    applyConcept(bodyContext, templateInfo, functionType->templateParams);
    for (auto& param : functionType->templateParams)
      bodyContext.addType(param->getName(), param);
    for (auto& p : parameters)
      if (p.name) {
        auto rawType = bodyContext.getTypeFromString(p.type).get(p.codeLoc);
        bodyContext.addVariable(*p.name, p.isMutable
            ? SType(MutableReferenceType::get(std::move(rawType)))
            : SType(ReferenceType::get(std::move(rawType))));
      }
    auto retVal = bodyContext.getTypeFromString(returnType).get(codeLoc);
    bodyContext.setReturnType(retVal);
    if (retVal != ArithmeticType::VOID && body && !body->hasReturnStatement(context) && !name.contains<ConstructorId>())
      codeLoc.error("Not all paths lead to a return statement in a function returning non-void");
    body->check(bodyContext);
  }
}

void FunctionDefinition::check(Context& context) {
  checkFunctionBody(context, false);
}

void FunctionDefinition::addToContext(Context& context) {
  setFunctionType(context, false);
  codeLoc.checkNoError(context.addFunction(*functionType));
}

static void initializeArithmeticTypes(Context& context) {
  CHECK(!context.addFunction(FunctionType("size"s, FunctionCallType::FUNCTION, ArithmeticType::INT,
      {{PointerType::get(ArithmeticType::STRING)}}, {})));
  CHECK(!context.addFunction(FunctionType("substring"s, FunctionCallType::FUNCTION, ArithmeticType::STRING,
      {{PointerType::get(ArithmeticType::STRING)}, {ArithmeticType::INT}, {ArithmeticType::INT}}, {})));
  CHECK(!context.addFunction(FunctionType(Operator::SUBSCRIPT, FunctionCallType::FUNCTION, ArithmeticType::CHAR,
      {{ArithmeticType::STRING}, {ArithmeticType::INT}}, {})));
  CHECK(!context.addFunction(FunctionType(Operator::PLUS, FunctionCallType::FUNCTION, ArithmeticType::STRING,
      {{ArithmeticType::STRING}, {ArithmeticType::STRING}}, {})));
  for (auto op : {Operator::PLUS_UNARY, Operator::MINUS_UNARY})
    CHECK(!context.addFunction(FunctionType(op, FunctionCallType::FUNCTION, ArithmeticType::INT, {{ArithmeticType::INT}}, {})));
  for (auto op : {Operator::PLUS, Operator::MINUS, Operator::MULTIPLY})
    CHECK(!context.addFunction(FunctionType(op, FunctionCallType::FUNCTION, ArithmeticType::INT,
        {{ArithmeticType::INT}, {ArithmeticType::INT}}, {})));
  for (auto op : {Operator::LOGICAL_AND, Operator::LOGICAL_OR})
    CHECK(!context.addFunction(FunctionType(op, FunctionCallType::FUNCTION, ArithmeticType::BOOL,
        {{ArithmeticType::BOOL}, {ArithmeticType::BOOL}}, {})));
  CHECK(!context.addFunction(FunctionType(Operator::LOGICAL_NOT, FunctionCallType::FUNCTION, ArithmeticType::BOOL,
      {{ArithmeticType::BOOL}}, {})));
  for (auto op : {Operator::EQUALS, Operator::LESS_THAN, Operator::MORE_THAN})
    for (auto type : {ArithmeticType::INT, ArithmeticType::STRING})
      CHECK(!context.addFunction(FunctionType(op, FunctionCallType::FUNCTION, ArithmeticType::BOOL,
          {{type}, {type}}, {})));
  for (auto op : {Operator::EQUALS})
    for (auto type : {ArithmeticType::BOOL, ArithmeticType::CHAR})
      CHECK(!context.addFunction(FunctionType(op, FunctionCallType::FUNCTION, ArithmeticType::BOOL, {{type}, {type}}, {})));
  for (auto& type : {ArithmeticType::BOOL, ArithmeticType::CHAR, ArithmeticType::INT, ArithmeticType::STRING})
    context.addCopyConstructorFor(type);
}

vector<string> correctness(const AST& ast, const vector<string>& importPaths) {
  Context context;
  initializeArithmeticTypes(context);
  for (auto type : {ArithmeticType::INT, ArithmeticType::BOOL,
       ArithmeticType::VOID, ArithmeticType::CHAR, ArithmeticType::STRING})
    context.addType(type->getName(), type);
  for (auto& elem : ast.elems) {
    if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
      import->setImportDirs(importPaths);
    elem->addToContext(context);
  }
  for (auto& elem : ast.elems) {
    elem->check(context);
  }
  return context.getAllImports();
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

void ExpressionStatement::check(Context& context) {
  expr->getType(context);
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

SType FunctionCall::getType(Context& context) {
  return getDotOperatorType(nullptr, context).get();
}

static WithErrorLine<FunctionType> getFunction(const Context& idContext, const Context& callContext, CodeLoc codeLoc,
    IdentifierInfo id, const vector<SType>& argTypes, const vector<CodeLoc>& argLoc) {
  ErrorLoc errors = codeLoc.getError("Couldn't find function " + id.toString() +
      " matching arguments: (" + joinTypeList(argTypes) + ")");
  vector<FunctionType> overloads;
  if (auto templateType = idContext.getFunctionTemplate(id)) {
    for (auto& overload : *templateType)
      if (auto f = callContext.instantiateFunctionTemplate(codeLoc, overload, id, argTypes, argLoc)) {
        overloads.push_back(*f);
      } else
        errors = codeLoc.getError(errors.error + "\nCandidate: "s + overload.toString() + ": " + f.get_error().error);
  }
  if (overloads.empty())
    return errors;
  overloads = filterOverloads(overloads, argTypes);
  CHECK(!overloads.empty());
  if (overloads.size() == 1)
    return overloads[0];
  else
    return codeLoc.getError("Multiple function overloads found:\n" +
        combine(transform(overloads, [](const auto& o) { return o.toString();}), "\n"));
}

nullable<SType> FunctionCall::getDotOperatorType(Expression* left, Context& callContext) {
  optional<ErrorLoc> error;
  if (!functionType) {
    vector<SType> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = 0; i < arguments.size(); ++i) {
      argTypes.push_back(arguments[i]->getType(callContext));
      argLocs.push_back(arguments[i]->codeLoc);
      INFO << "Function argument " << argTypes.back()->getName();
    }
    if (!left)
      getFunction(callContext, callContext, codeLoc, identifier, argTypes, argLocs)
          .unpack(functionType, error);
    else {
      auto leftType = left->getType(callContext);
      callType = MethodCallType::METHOD;
      auto tryMethodCall = [&](MethodCallType thisCallType) {
        auto res = getFunction(callContext, callContext, codeLoc, identifier, concat({leftType}, argTypes), concat({left->codeLoc}, argLocs));
        if (res) {
          if (res->externalMethod)
            callType = MethodCallType::METHOD;
          else
            callType = thisCallType;
        }
        if (res && functionType)
          codeLoc.error("Ambigous method call:\nCandidate: " + functionType->toString() + "\nCandidate: " + res->toString());
        res.unpack(functionType, error);
      };
      if (!leftType->getUnderlying().dynamicCast<PointerType>() &&
          !leftType->getUnderlying().dynamicCast<MutablePointerType>())
        tryMethodCall(MethodCallType::FUNCTION_AS_METHOD);
      leftType = leftType.dynamicCast<MutableReferenceType>()
          ? SType(MutablePointerType::get(leftType->getUnderlying()))
          : SType(PointerType::get(leftType->getUnderlying()));
      tryMethodCall(MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER);
    }
  }
  if (functionType)
    return functionType->retVal;
  else
    error->execute();
}

FunctionCallNamedArgs::FunctionCallNamedArgs(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {}

SType FunctionCallNamedArgs::getType(Context& context) {
  return getDotOperatorType(nullptr, context).get();
}

nullable<SType> FunctionCallNamedArgs::getDotOperatorType(Expression* left, Context& callContext) {
  const auto& leftContext = left ? left->getType(callContext)->getContext() : callContext;
  optional<ErrorLoc> error = ErrorLoc{codeLoc, "Function not found: " + identifier.toString()};
  if (!functionType) {
    optional<vector<ArgMatching>> matchings;
    if (!left) {
      matchArgs(leftContext, callContext, false).unpack(matchings, error);
      if (matchings)
        for (auto& matching : *matchings) {
          callContext.instantiateFunctionTemplate(codeLoc, matching.function, identifier, matching.args, matching.codeLocs)
              .unpack(functionType, error);
          if (functionType)
            break;
        }
    } else {
      auto leftType = left->getType(callContext);
      callType = MethodCallType::METHOD;
      matchArgs(callContext, callContext, true).unpack(matchings, error);
      if (matchings) {
        auto tryMethodCall = [&] (MethodCallType thisCallType) {
          for (auto& matching : *matchings) {
            auto res = callContext.instantiateFunctionTemplate(codeLoc, matching.function, identifier,
                concat({leftType}, matching.args), concat({left->codeLoc}, matching.codeLocs));
            if (res)
              callType = thisCallType;
            if (res && functionType)
              codeLoc.error("Ambigous method call:\nCandidate: " + functionType->toString() + "\nCandidate: " + res->toString());
            res.unpack(functionType, error);
          }
        };
        if (!leftType->getUnderlying().dynamicCast<PointerType>() &&
            !leftType->getUnderlying().dynamicCast<MutablePointerType>())
        tryMethodCall(MethodCallType::FUNCTION_AS_METHOD);
        leftType = leftType.dynamicCast<MutableReferenceType>()
            ? SType(MutablePointerType::get(leftType->getUnderlying()))
            : SType(PointerType::get(leftType->getUnderlying()));
        tryMethodCall(MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER);
      }
    }
  }
  if (functionType)
    return functionType->retVal;
  else
    error->execute();
}

WithErrorLine<vector<FunctionCallNamedArgs::ArgMatching>> FunctionCallNamedArgs::matchArgs(const Context& functionContext, Context& callContext, bool skipFirst) {
  auto functionOverloads = functionContext.getFunctionTemplate(identifier);
  if (!functionOverloads)
    return codeLoc.getError(functionOverloads.get_error());
  ErrorLoc error = codeLoc.getError("Couldn't find function overload matching named arguments.");
  vector<ArgMatching> ret;
  for (auto& overload : *functionOverloads) {
    set<string> toInitialize;
    set<string> initialized;
    map<string, int> paramIndex;
    auto paramNames = transform(overload.params, [](const FunctionType::Param& p) { return p.name; });
    int count = 0;
    vector<string> notInitialized;
    vector<SType> argTypes;
    vector<CodeLoc> argLocs;
    for (int i = (skipFirst ? 1 : 0); i < paramNames.size(); ++i) {
      if (auto paramName = paramNames.at(i)) {
        toInitialize.insert(*paramName);
        paramIndex[*paramName] = count++;
      } else {
        error = codeLoc.getError("Can't call function that has an unnamed parameter this way");
        goto nextOverload;
      }
    }
    for (auto& elem : arguments) {
      if (!toInitialize.count(elem.name)) {
        error = elem.codeLoc.getError("No parameter named " + quote(elem.name)
            + " in function " + quote(identifier.toString()));
        goto nextOverload;
      }
      if (initialized.count(elem.name))
        return elem.codeLoc.getError("Parameter " + quote(elem.name) + " listed more than once");
      initialized.insert(elem.name);
    }
    for (auto& elem : toInitialize)
      if (!initialized.count(elem))
        notInitialized.push_back("" + quote(elem));
    if (!notInitialized.empty()) {
      error = codeLoc.getError("Function parameters: " + combine(notInitialized, ",")
          + " were not initialized");
      goto nextOverload;
    }
    sort(arguments.begin(), arguments.end(),
        [&](const Argument& m1, const Argument& m2) { return paramIndex[m1.name] < paramIndex[m2.name]; });
    for (auto& arg : arguments) {
      argTypes.push_back(arg.expr->getType(callContext));
      argLocs.push_back(arg.codeLoc);
    }
    ret.push_back(ArgMatching{argTypes, argLocs, overload});
    nextOverload:
    continue;
  }
  if (!ret.empty())
    return ret;
  else
    return error;
}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

void SwitchStatement::check(Context& context) {
  expr->getType(context)->handleSwitchStatement(*this, context, expr->codeLoc, false);
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

void VariantDefinition::addToContext(Context& context) {
  context.checkNameConflict(codeLoc, name, "Type");
  type = StructType::get(StructType::VARIANT, name);
  context.addType(name, type.get());
  auto membersContext = Context::withParent({&context, &type->context});
  for (auto& param : templateInfo.params)
    type->templateParams.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
  type->requirements = applyConcept(membersContext, templateInfo, type->templateParams);
  for (auto& param : type->templateParams)
    type->context.addType(param->getName(), param);
  unordered_set<string> subtypeNames;
  for (auto& subtype : elements) {
    subtype.codeLoc.check(!subtypeNames.count(subtype.name), "Duplicate variant alternative: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    vector<FunctionType::Param> params;
    auto subtypeInfo = membersContext.getTypeFromString(subtype.type).get(subtype.codeLoc);
    if (subtypeInfo != ArithmeticType::VOID)
      params.push_back(FunctionType::Param{subtypeInfo});
    auto constructor = FunctionType(subtype.name, FunctionCallType::FUNCTION, type.get(), params, {});
    constructor.parentType = type.get();
    CHECK(!type->staticContext.addFunction(constructor));
  }
}

void VariantDefinition::check(Context& context) {
  auto bodyContext = Context::withParent({&context, &type->context});
  applyConcept(bodyContext, templateInfo, type->templateParams);
  for (auto& subtype : elements)
    type->alternatives.push_back({subtype.name, bodyContext.getTypeFromString(subtype.type).get(subtype.codeLoc)});
  bodyContext.addVariable("this", PointerType::get(type.get()));
  type->updateInstantations();
  auto canCopyConstructAllAlternatives = [&] {
    for (auto& alternative : type->alternatives)
      if (alternative.type != ArithmeticType::VOID && !bodyContext.canCopyConstruct(alternative.type))
        return false;
    return true;
  };
  if (canCopyConstructAllAlternatives() && !context.canCopyConstruct(type.get()))
    CHECK(!context.addCopyConstructorFor(type.get(), type->templateParams));
}

void StructDefinition::addToContext(Context& context) {
  context.checkNameConflict(codeLoc, name, "Type");
  type = StructType::get(StructType::STRUCT, name);
  context.addType(name, type.get());
  auto membersContext = Context::withParent({&context, &type->context});
  for (auto& param : templateInfo.params)
    type->templateParams.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  type->requirements = applyConcept(membersContext, templateInfo, type->templateParams);
  bool hasConstructor = false;
  for (auto& method : methods) {
    if (method->name.contains<ConstructorId>()) {
      // We have to fix the return type of the constructor otherwise it can't be looked up in the context
      method->returnType.parts[0].templateArguments =
          transform(templateInfo.params, [](const auto& p) { return IdentifierInfo(p.name); });
      method->setFunctionType(membersContext, true);
      method->functionType->callType = FunctionCallType::CONSTRUCTOR;
      method->codeLoc.check(method->functionType->templateParams.empty(), "Constructor can't have template parameters.");
      method->functionType->templateParams = type->templateParams;
      method->functionType->parentType = type.get();
      method->codeLoc.checkNoError(context.addFunction(*method->functionType));
      hasConstructor = true;
    } else
      method->codeLoc.error("Only constructors and members can be defined inside struct body.");
  }
  if (!hasConstructor) {
    vector<FunctionType::Param> constructorParams;
    for (auto& member : members)
      if (auto memberType = membersContext.getTypeFromString(member.type))
        constructorParams.push_back({member.name, *memberType});
    auto constructor = FunctionType(SType(type.get()), FunctionCallType::CONSTRUCTOR, type.get(), std::move(constructorParams), type->templateParams);
    constructor.parentType = type.get();
    CHECK(!context.addFunction(constructor));
  }
  auto canCopyConstructAllMembers = [&] {
    for (auto& member : members)
      if (auto memberType = membersContext.getTypeFromString(member.type))
        if (!membersContext.canCopyConstruct(*memberType))
          return false;
    return true;
  };
  if (!external && canCopyConstructAllMembers() && !context.canCopyConstruct(type.get()))
    CHECK(!context.addCopyConstructorFor(type.get(), type->templateParams));
}

static void checkConstructor(const StructType& type, const Context& context, const FunctionDefinition& method) {
  map<string, int> memberIndex;
  int index =  0;
  for (auto& member : type.context.getBottomLevelVariables())
    memberIndex[member] = index++;
  index = -1;
  for (auto& initializer : method.initializers) {
    auto memberType = type.context.getTypeOfVariable(initializer.paramName).get_value();
    auto initContext = Context::withParent(context);
    for (auto& p : method.functionType->params)
      if (p.name)
        initContext.addVariable(*p.name, p.type);
    auto exprType = initializer.expr->getType(initContext);
    initializer.codeLoc.check(!!memberType, type.getName() + " has no member named " + quote(initializer.paramName));
    initializer.codeLoc.check(context.canConstructWith(memberType->getUnderlying(), {exprType}),
        "Can't assign to member " + quote(initializer.paramName) + " of type " + quote(memberType->getName()) +
        " from expression of type " + quote(exprType->getName()));
    int currentIndex = memberIndex.at(initializer.paramName);
    initializer.codeLoc.check(currentIndex > index, "Can't initialize member " + quote(initializer.paramName) + " out of order");
    index = currentIndex;
    memberIndex.erase(initializer.paramName);
  }
  for (auto& nonInitialized : memberIndex)
    method.codeLoc.check(context.canConstructWith(type.context.getTypeOfVariable(nonInitialized.first).get_value()->getUnderlying(), {}),
        "Member " + quote(nonInitialized.first) + " needs to be initialized");
}

void StructDefinition::check(Context& context) {
  auto methodBodyContext = Context::withParent({&context, &type->context});
  auto staticFunContext = Context::withParent({&context, &type->staticContext});
  for (auto& param : type->templateParams)
    methodBodyContext.addType(param->getName(), param);
  methodBodyContext.addVariable("this", PointerType::get(type.get()));
  applyConcept(methodBodyContext, templateInfo, type->templateParams);
  for (auto& member : members) {
    INFO << "Struct member " << member.name << " " << member.type.toString() << " line " << member.codeLoc.line << " column " << member.codeLoc.column;
    type->context.addVariable(member.name, MutableReferenceType::get(methodBodyContext.getTypeFromString(member.type).get(member.codeLoc)));
  }
  for (int i = 0; i < methods.size(); ++i) {
    if (!external)
      methods[i]->codeLoc.check(!!methods[i]->body, "Method body expected in non-extern struct");
    if (methods[i]->functionType->name.contains<SType>()) {
      checkConstructor(*type, methodBodyContext, *methods[i]);
      methods[i]->checkFunctionBody(staticFunContext, !templateInfo.params.empty());
    } else
      methods[i]->checkFunctionBody(methodBodyContext, !templateInfo.params.empty());
  }
  type->updateInstantations();
  codeLoc.check(!type->hasInfiniteSize(), quote(type->getName()) + " has infinite size");
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

SType UnaryExpression::getType(Context& context) {
  nullable<SType> ret;
  auto right = expr->getType(context);
  ErrorLoc error { codeLoc, "Can't apply operator: " + quote(getString(op)) + " to type: " + quote(right->getName())};
  for (auto fun : right->getContext().getOperatorType(op))
    if (auto inst = instantiateFunction(context, fun, codeLoc, {}, {}, {})) {
      CHECK(!ret);
      ret = inst->retVal;
    } else
      error = codeLoc.getError(error.error + "\nCandidate: "s + fun.toString() + ": " + inst.get_error().error);
  for (auto fun : context.getOperatorType(op))
    if (auto inst = instantiateFunction(context, fun, codeLoc, {}, {right}, {expr->codeLoc})) {
      CHECK(!ret);
      ret = inst->retVal;
    } else
      error = codeLoc.getError(error.error + "\nCandidate: "s + fun.toString() + ": " + inst.get_error().error);
  if (ret)
    return ret.get();
  else
    error.execute();
}

MoveExpression::MoveExpression(CodeLoc l, string id) : Expression(l), identifier(id) {
}

SType MoveExpression::getType(Context& context) {
  if (!type) {
    if (auto ret = context.getTypeOfVariable(identifier)) {
      codeLoc.check(!!ret.get_value().dynamicCast<MutableReferenceType>(), "Can't move from " + quote(ret.get_value()->getName()));
      codeLoc.checkNoError(context.setVariableAsMoved(identifier));
      type = ret.get_value()->getUnderlying();
    } else
      codeLoc.error(ret.get_error());
  }
  return type.get();
}


EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

void EmbedStatement::check(Context&) {
}

Statement::TopLevelAllowance EmbedStatement::allowTopLevel() const {
  if (isPublic)
    return TopLevelAllowance::MUST;
  else
    return TopLevelAllowance::CAN;
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<Statement> i, unique_ptr<Expression> c,
                                   unique_ptr<Expression> it, unique_ptr<Statement> b)
  : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

bool ForLoopStatement::hasReturnStatement(const Context& s) const {
  return body->hasReturnStatement(s);
}

void ForLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  init->check(bodyContext);
  cond->codeLoc.check(cond->getType(bodyContext) == ArithmeticType::BOOL,
      "Loop condition must be of type " + quote("bool"));
  iter->getType(bodyContext);
  body->check(bodyContext);
}

WhileLoopStatement::WhileLoopStatement(CodeLoc l, unique_ptr<Expression> c, unique_ptr<Statement> b)
  : Statement(l), cond(std::move(c)), body(std::move(b)) {}

bool WhileLoopStatement::hasReturnStatement(const Context& s) const {
  return body->hasReturnStatement(s);
}

void WhileLoopStatement::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  cond->codeLoc.check(cond->getType(bodyContext) == ArithmeticType::BOOL,
      "Loop condition must be of type " + quote("bool"));
  body->check(bodyContext);
}

ImportStatement::ImportStatement(CodeLoc l, string p, bool pub) : Statement(l), path(p), isPublic(pub) {
}

void ImportStatement::setImportDirs(const vector<string>& p) {
  importDirs = p;
}

void ImportStatement::check(Context&) {
}

void ImportStatement::processImport(Context& context, const string& content, const string& path) {
  auto contentHash = std::hash<string>()(content);
  codeLoc.check(!context.isCurrentlyImported(contentHash),
      "Public import cycle: " + combine(context.getCurrentImports(), ", "));
  if ((!isPublic && !context.getCurrentImports().empty()) || context.wasEverImported(contentHash))
    return;
  context.pushImport(path, contentHash);
  auto tokens = lex(content, path);
  ast = unique<AST>(parse(tokens));
  for (auto& elem : ast->elems) {
    if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
      import->setImportDirs(importDirs);
    elem->addToContext(context);
  }
  for (auto& elem : ast->elems)
    elem->check(context);
  context.popImport();
}

void ImportStatement::addToContext(Context& s) {
  INFO << "Resolving import " << path;
  for (auto importDir : concat({getParentPath(codeLoc.file)}, importDirs)) {
    auto importPath = importDir + "/" + path;
    INFO << "Trying directory " << importPath;
    if (auto content = readFromFile(importPath.c_str())) {
      INFO << "Imported file " << importPath;
      processImport(s, content->value, importPath);
      return;
    }
  }
  codeLoc.error("Couldn't resolve import path: " + path);
}

nullable<SType> Expression::getDotOperatorType(Expression* left, Context& callContext) {
  return nullptr;
}

EnumDefinition::EnumDefinition(CodeLoc l, string n) : Statement(l), name(n) {}

void EnumDefinition::addToContext(Context& s) {
  codeLoc.check(!elements.empty(), "Enum requires at least one element");
  unordered_set<string> occurences;
  for (auto& e : elements)
    codeLoc.check(!occurences.count(e), "Duplicate enum element: " + quote(e));
  s.addType(name, shared<EnumType>(name, elements));
}

void EnumDefinition::check(Context& s) {
}

EnumConstant::EnumConstant(CodeLoc l, string name, string element) : Expression(l), enumName(name), enumElement(element) {
}

SType EnumConstant::getType(Context& context) {
  return context.getTypeFromString(IdentifierInfo(enumName)).get(codeLoc);
}

ConceptDefinition::ConceptDefinition(CodeLoc l, string name) : Statement(l), name(name) {

}

void ConceptDefinition::addToContext(Context& context) {
  shared_ptr<Concept> concept = shared<Concept>(name);
  auto declarationsContext = Context::withParent(context);
  for (auto& param : templateInfo.params) {
    concept->modParams().push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    declarationsContext.addType(param.name, concept->modParams().back());
  }
  for (auto& function : functions) {
    function->setFunctionType(declarationsContext, false);
    function->functionType->fromConcept = true;
    function->check(declarationsContext);
    function->codeLoc.checkNoError(concept->modContext().addFunction(*function->functionType));
  }
  context.addConcept(name, concept);
}

void ConceptDefinition::check(Context& context) {
}

CodegenStage CodegenStage::define() {
  CodegenStage ret;
  ret.isDefine = true;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::declare() {
  CodegenStage ret;
  ret.isDefine = false;
  ret.isImport = false;
  return ret;
}

CodegenStage CodegenStage::setImport() {
  isImport = true;
  return *this;
}
