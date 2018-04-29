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

IfStatement::IfStatement(CodeLoc loc, unique_ptr<Expression> c, unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), cond(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
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

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, string n) : Statement(l), returnType(r), nameOrOp(n) {}

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, Operator op) : Statement(l), returnType(r), nameOrOp(op) {}

SType Constant::getType(const Context&) {
  return type;
}

SType Variable::getType(const Context& context) {
  if (auto ret = context.getTypeOfVariable(identifier))
    return ret.get();
  else
    codeLoc.error("Undefined variable: " + identifier);
}

nullable<SType> Variable::getDotOperatorType(const Context& idContext, const Context& callContext) {
  return getType(idContext);
}

SType BinaryExpression::getType(const Context& context) {
  return getOperationResult(e1->codeLoc, op, context, *e1, *e2);
}

void StatementBlock::check(Context& context) {
  auto bodyContext = Context::withParent(context);
  for (auto& s : elems) {
    s->check(bodyContext);
  }
}

void IfStatement::check(Context& context) {
  auto condType = cond->getType(context);
  codeLoc.check(canConvert(condType, ArithmeticType::BOOL), "Expected a type convertible to bool inside if statement, got "
      + quote(condType->getName()));
  ifTrue->check(context);
  if (ifFalse)
    ifFalse->check(context);
}

void VariableDeclaration::check(Context& context) {
  context.checkNameConflict(codeLoc, identifier, "Variable");
  auto inferType = [&] () -> SType {
    if (type) {
      if (auto typeId = context.getTypeFromString(*type))
        return typeId.get();
      else
        codeLoc.error("Type " + quote(type->toString()) + " not recognized");
    }
    else if (initExpr)
      return initExpr->getType(context);
    else
      codeLoc.error("Initializing expression needed to infer variable type");
  };
  realType = inferType();
  INFO << "Adding variable " << identifier << " of type " << realType.get()->getName();
  if (initExpr) {
    auto exprType = initExpr->getType(context);
    initExpr->codeLoc.check(ReferenceType::get(realType.get())->canAssign(exprType), "Can't initialize variable of type "
        + quote(realType.get()->getName()) + " with value of type " + quote(exprType->getName()));
  } else
    codeLoc.check(!requiresInitialization(realType.get()), "Type " + quote(realType->getName()) + " requires initialization");
  context.addVariable(identifier, ReferenceType::get(realType.get()));
}

void ReturnStatement::check(Context& context) {
  if (!expr)
    codeLoc.check(context.getReturnType() && context.getReturnType() == ArithmeticType::VOID,
        "Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = expr->getType(context);
    codeLoc.check(ReferenceType::get(context.getReturnType().get())->canAssign(returnType),
        "Attempting to return value of type "s + quote(returnType->getName()) +
         " in a function returning "s + quote(context.getReturnType()->getName()));
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

static void applyConcept(const Context& from, const TemplateInfo& templateInfo, const vector<SType>& templateTypes) {
  auto getTemplateParam = [&](CodeLoc codeLoc, const string& name) {
    for (int i = 0; i < templateInfo.params.size(); ++i)
      if (templateInfo.params[i].name == name)
        return templateTypes[i];
    codeLoc.error("Type " + quote(name) + " not part of template arguments");
  };
  for (auto& requirement : templateInfo.requirements) {
    if (auto concept = from.getConcept(requirement.parts[0].name)) {
      auto& requirementArgs = requirement.parts[0].templateArguments;
      requirement.codeLoc.check(requirementArgs.size() == concept->params.size(),
          "Wrong number of template arguments to concept " + quote(requirement.parts[0].toString()));
      for (int i = 0; i < requirementArgs.size(); ++i) {
        auto& requirementArg = requirementArgs[i].parts[0].name;
        auto upgradedParam = getTemplateParam(requirementArgs[i].codeLoc, requirementArg);
        Context addedContext;
        addedContext.deepCopyFrom(concept->params[i]->context);
        for (int j = 0; j < requirementArgs.size(); ++j)
          addedContext.replace(concept->params[j], getTemplateParam(requirementArgs[j].codeLoc, requirementArgs[j].parts[0].name));
        upgradedParam->context.mergeAndCollapse(std::move(addedContext));
      }
    }
  }
}

void FunctionDefinition::setFunctionType(const Context& context) {
  Context contextWithTemplateParams = Context::withParent(context);
  if (auto name = nameOrOp.getReferenceMaybe<string>())
    context.checkNameConflict(codeLoc, *name, "Function");
  else {
    auto op = *nameOrOp.getValueMaybe<Operator>();
    codeLoc.check(!context.getOperatorType(op), "Operator " + quote(getString(op)) + " already defined");
  }
  vector<SType> templateTypes;
  for (auto& param : templateInfo.params) {
    templateTypes.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    contextWithTemplateParams.checkNameConflict(param.codeLoc, param.name, "template parameter");
    contextWithTemplateParams.addType(param.name, templateTypes.back());
  }
  applyConcept(context, templateInfo, templateTypes);
  if (auto returnType = contextWithTemplateParams.getTypeFromString(this->returnType)) {
    vector<FunctionType::Param> params;
    for (auto& p : parameters)
      if (auto paramType = contextWithTemplateParams.getTypeFromString(p.type)) {
        params.push_back({p.name, paramType.get()});
      } else
        p.codeLoc.error("Unrecognized parameter type: " + quote(p.type.toString()));
    functionType = FunctionType(FunctionCallType::FUNCTION, returnType.get(), params, templateTypes );
  } else
    codeLoc.error("Unrecognized return type: " + this->returnType.toString());
}

void FunctionDefinition::checkFunction(Context& context, bool templateStruct) {
  Context bodyContext = Context::withParent(context);
  if (auto op = nameOrOp.getValueMaybe<Operator>()) {
    codeLoc.check(templateInfo.params.empty(), "Operator overload can't have template parameters.");
    codeLoc.check(canOverload(*op, parameters.size()), "Can't overload operator " + quote(getString(*op)) +
        " with " + to_string(parameters.size()) + " arguments.");
  }
  for (auto& param : functionType->templateParams)
    bodyContext.addType(param->getName(), param);
  if (auto returnType = bodyContext.getTypeFromString(this->returnType)) {
    for (auto& p : parameters)
      if (auto paramType = bodyContext.getTypeFromString(p.type))
        bodyContext.addVariable(p.name, paramType.get());
      else
        p.codeLoc.error("Unrecognized parameter type: " + quote(p.type.toString()));
    bodyContext.setReturnType(returnType.get());
    if (returnType != ArithmeticType::VOID && body && !body->hasReturnStatement(context))
      codeLoc.error("Not all paths lead to a return statement in a function returning non-void");
  } else
    codeLoc.error("Unrecognized return type: " + this->returnType.toString());
  if (body && (!templateInfo.params.empty() || templateStruct || context.getImports().empty()))
    body->check(bodyContext);
}

void FunctionDefinition::check(Context& context) {
  checkFunction(context, false);
}

void FunctionDefinition::addToContext(Context& context) {
  setFunctionType(context);
  context.addFunction(nameOrOp, *functionType);
}

static void initializeArithmeticTypes() {
  ArithmeticType::STRING->context.addFunction("size"s, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::INT, {}, {}));
  ArithmeticType::STRING->context.addFunction("substring"s, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::STRING,
      {{"index", ArithmeticType::INT}, {"length", ArithmeticType::INT}}, {}));
  ArithmeticType::STRING->context.addFunction(Operator::SUBSCRIPT, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::CHAR,
      {{"index", ArithmeticType::INT}}, {}));
  ArithmeticType::STRING->context.addFunction(Operator::PLUS, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::STRING,
      {{"right side", ArithmeticType::STRING}}, {}));
  for (auto op : {Operator::PLUS_UNARY, Operator::MINUS_UNARY})
    ArithmeticType::INT->context.addFunction(op, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::INT,
        {}, {}));
  for (auto op : {Operator::PLUS, Operator::MINUS, Operator::MULTIPLY})
    ArithmeticType::INT->context.addFunction(op, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::INT,
        {{"right side", ArithmeticType::INT}}, {}));
  for (auto op : {Operator::EQUALS, Operator::LESS_THAN, Operator::MORE_THAN})
    for (auto type : {ArithmeticType::INT, ArithmeticType::STRING})
      type->context.addFunction(op, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::BOOL,
          {{"right side", type}}, {}));
  for (auto op : {Operator::EQUALS})
    for (auto type : {ArithmeticType::BOOL, ArithmeticType::CHAR})
      type->context.addFunction(op, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::BOOL, {{"right sie", type}}, {}));
}

void correctness(const AST& ast) {
  Context context;
  initializeArithmeticTypes();
  for (auto type : {ArithmeticType::INT, ArithmeticType::BOOL,
       ArithmeticType::VOID, ArithmeticType::CHAR, ArithmeticType::STRING})
    context.addType(type->getName(), type);
  for (auto& elem : ast.elems)
    elem->addToContext(context);
  for (auto& elem : ast.elems) {
    elem->check(context);
  }
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

void ExpressionStatement::check(Context& context) {
  expr->getType(context);
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

SType FunctionCall::getType(const Context& context) {
  return getDotOperatorType(context, context).get();
}

static FunctionType getFunction(const Context& idContext, const Context& callContext, CodeLoc codeLoc, IdentifierInfo id,
    const vector<SType>& argTypes, const vector<CodeLoc>& argLoc) {
  auto templateType = idContext.getFunctionTemplate(codeLoc, id);
  return callContext.instantiateFunctionTemplate(codeLoc, templateType, id, argTypes, argLoc);
}

nullable<SType> FunctionCall::getDotOperatorType(const Context& idContext, const Context& callContext) {
  vector<SType> argTypes;
  vector<CodeLoc> argLocs;
  for (int i = 0; i < arguments.size(); ++i) {
    argTypes.push_back(arguments[i]->getType(callContext));
    argLocs.push_back(arguments[i]->codeLoc);
    INFO << "Function argument " << argTypes.back()->getName();
  }
  functionType = getFunction(idContext, callContext, codeLoc, identifier, argTypes, argLocs);
  return functionType->retVal;
}

FunctionCallNamedArgs::FunctionCallNamedArgs(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {}

SType FunctionCallNamedArgs::getType(const Context& context) {
  return getDotOperatorType(context, context).get();
}

nullable<SType> FunctionCallNamedArgs::getDotOperatorType(const Context& idContext, const Context& callContext) {
  set<string> toInitialize;
  set<string> initialized;
  map<string, int> paramIndex;
  vector<string> paramNames = idContext.getFunctionParamNames(codeLoc, identifier);
  int count = 0;
  for (auto& param : paramNames) {
    toInitialize.insert(param);
    paramIndex[param] = count++;
  }
  for (auto& elem : arguments) {
    elem.codeLoc.check(toInitialize.count(elem.name), "No parameter named " + quote(elem.name)
        + " in function " + quote(identifier.toString()));
    elem.codeLoc.check(!initialized.count(elem.name), "Parameter " + quote(elem.name) + " listed more than once");
    initialized.insert(elem.name);
  }
  vector<string> notInitialized;
  for (auto& elem : toInitialize)
    if (!initialized.count(elem))
      notInitialized.push_back("" + quote(elem));
  codeLoc.check(notInitialized.empty(), "Function parameters: " + combine(notInitialized, ",")
      + " were not initialized" );
  sort(arguments.begin(), arguments.end(),
      [&](const Argument& m1, const Argument& m2) { return paramIndex[m1.name] < paramIndex[m2.name]; });
  vector<SType> argTypes;
  vector<CodeLoc> argLocs;
  for (auto& arg : arguments) {
    argTypes.push_back(arg.expr->getType(callContext));
    argLocs.push_back(arg.codeLoc);
  }
  functionType = getFunction(idContext, callContext, codeLoc, identifier, argTypes, argLocs);
  return functionType->retVal;
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
  applyConcept(context, templateInfo, type->templateParams);
  for (auto& param : type->templateParams)
    type->context.addType(param->getName(), param);
  unordered_set<string> subtypeNames;
  for (auto& subtype : elements) {
    subtype.codeLoc.check(!subtypeNames.count(subtype.name), "Duplicate variant alternative: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    vector<FunctionType::Param> params;
    if (auto subtypeInfo = membersContext.getTypeFromString(subtype.type)) {
      if (subtypeInfo != ArithmeticType::VOID)
        params.push_back(FunctionType::Param{"", subtypeInfo.get()});
    } else
      subtype.codeLoc.error("Unrecognized type: " + quote(subtype.type.toString()));
    type->staticContext.addFunction(subtype.name, FunctionType(FunctionCallType::FUNCTION, type.get(), params, {}));
  }
  for (auto& method : methods) {
    method->setFunctionType(membersContext);
    type->context.addFunction(method->nameOrOp, *method->functionType);
  }
}

void VariantDefinition::check(Context& context) {
  auto methodBodyContext = Context::withParent({&context, &type->context});
  for (auto& subtype : elements) {
    if (auto subtypeInfo = methodBodyContext.getTypeFromString(subtype.type))
      type->alternatives.push_back({subtype.name, subtypeInfo.get()});
    else
      subtype.codeLoc.error("Unrecognized type: " + quote(subtype.type.toString()));
  }
  methodBodyContext.addVariable("this", PointerType::get(type.get()));
  for (int i = 0; i < methods.size(); ++i)
    methods[i]->checkFunction(methodBodyContext, !templateInfo.params.empty());
  type->updateInstantations();
}

void StructDefinition::addToContext(Context& context) {
  context.checkNameConflict(codeLoc, name, "Type");
  type = StructType::get(StructType::STRUCT, name);
  context.addType(name, type.get());
  auto membersContext = Context::withParent({&context, &type->context});
  for (auto& param : templateInfo.params)
    type->templateParams.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
  applyConcept(context, templateInfo, type->templateParams);
  for (auto& param : type->templateParams)
    membersContext.addType(param->getName(), param);
  vector<FunctionType::Param> constructorParams;
  for (auto& member : members)
    if (auto memberType = membersContext.getTypeFromString(member.type))
      constructorParams.push_back({member.name, memberType.get()});
  for (auto& method : methods) {
    method->setFunctionType(membersContext);
    type->context.addFunction(method->nameOrOp, *method->functionType);
  }
  auto constructor = FunctionType(FunctionCallType::CONSTRUCTOR, type.get(), std::move(constructorParams), type->templateParams);
  context.addFunction(name, constructor);
}

void StructDefinition::check(Context& context) {
  auto methodBodyContext = Context::withParent({&context, &type->context});
  for (auto& param : type->templateParams)
    methodBodyContext.addType(param->getName(), param);
  methodBodyContext.addVariable("this", PointerType::get(type.get()));
  for (auto& member : members) {
    INFO << "Struct member " << member.name << " " << member.type.toString() << " line " << member.codeLoc.line << " column " << member.codeLoc.column;
    if (auto memberType = methodBodyContext.getTypeFromString(member.type))
      type->context.addVariable(member.name, ReferenceType::get(memberType.get()));
    else
      member.codeLoc.error("Type " + quote(member.type.toString()) + " not recognized");
  }
  for (int i = 0; i < methods.size(); ++i)
    methods[i]->checkFunction(methodBodyContext, !templateInfo.params.empty());
  type->updateInstantations();
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

SType UnaryExpression::getType(const Context& context) {
  return getUnaryOperationResult(expr->codeLoc, op, expr->getType(context));
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

void ImportStatement::check(Context& s) {
}

void ImportStatement::addToContext(Context& s) {
  if ((!isPublic && !s.getImports().empty()) || contains(s.getAllImports(), path))
    return;
  codeLoc.check(!contains(s.getImports(), path), "Public import cycle: " + combine(s.getImports(), ", "));
  s.pushImport(path);
  string content = readFromFile(path.c_str(), codeLoc);
  INFO << "Imported file " << path;
  auto tokens = lex(content, path);
  ast = unique<AST>(parse(tokens));
  for (auto& elem : ast->elems)
    elem->addToContext(s);
  for (auto& elem : ast->elems)
    elem->check(s);
  s.popImport();
}

nullable<SType> Expression::getDotOperatorType(const Context& idContext, const Context& callContext) {
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

SType EnumConstant::getType(const Context& context) {
  if (auto type = context.getTypeFromString(IdentifierInfo(enumName)))
    return type.get();
  else
    codeLoc.error("Unrecognized type: " + quote(enumName));
}

ConceptDefinition::ConceptDefinition(CodeLoc l, string name) : Statement(l), name(name) {

}

void ConceptDefinition::addToContext(Context& context) {
  shared_ptr<Concept> concept = shared<Concept>(name);
  auto declarationsContext = Context::withParent(context);
  for (auto& param : templateInfo.params) {
    concept->params.push_back(shared<TemplateParameterType>(param.name, param.codeLoc));
    declarationsContext.addType(param.name, concept->params.back());
  }
  for (auto& type : types)
    for (int i = 0; i < templateInfo.params.size(); ++i)
      if (type.name == templateInfo.params[i].name) {
        for (auto& method : type.methods) {
          method->setFunctionType(declarationsContext);
          method->check(declarationsContext);
          method->functionType->parentConcept = concept;
          concept->params[i]->context.addFunction(method->nameOrOp, *method->functionType);
        }
      }
  context.addConcept(name, concept);
}

void ConceptDefinition::check(Context& context) {
}
