#include "ast.h"
#include "type.h"
#include "reader.h"
#include "lexer.h"
#include "parser.h"

using namespace std;

Node::Node(CodeLoc l) : codeLoc(l) {}


BinaryExpression::BinaryExpression(CodeLoc loc, Operator o, unique_ptr<Expression> u1, unique_ptr<Expression> u2)
    : Expression(loc), op(o) {
  e1 = std::move(u1);
  e2 = std::move(u2);
}

IfStatement::IfStatement(CodeLoc loc, unique_ptr<Expression> c, unique_ptr<Statement> t, unique_ptr<Statement> f)
  : Statement(loc), cond(std::move(c)), ifTrue(std::move(t)), ifFalse(std::move(f)) {
}

Constant::Constant(CodeLoc l, ArithmeticType t, string v) : Expression(l), type(t), value(v) {
  INFO << "Created constant " << quote(v) << " of type " << getName(Type(t));
}

Variable::Variable(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {
  INFO << "Parsed variable " << id.toString();
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

FunctionDefinition::FunctionDefinition(CodeLoc l, IdentifierInfo r, string n) : Statement(l), returnType(r), name(n) {}

Type Constant::getType(const State&) {
  return type;
}

Type Variable::getType(const State& state) {
  if (auto ret = state.getTypeOfVariable(identifier))
    return *ret;
  else
    codeLoc.error("Undefined variable: " + identifier.toString());
  return {};
}

optional<Type> Variable::getDotOperatorType(const State& idContext, const State& callContext) {
  return getType(idContext);
}

Type BinaryExpression::getType(const State& state) {
  return getOperationResult(e1->codeLoc, op, state, *e1, *e2);
}

void StatementBlock::check(State& state) {
  auto stateCopy = state;
  for (auto& s : elems) {
    s->check(stateCopy);
  }
}

void IfStatement::check(State& state) {
  auto condType = cond->getType(state);
  codeLoc.check(canConvert(condType, ArithmeticType::BOOL), "Expected a type convertible to bool inside if statement, got "
      + quote(getName(condType)));
  ifTrue->check(state);
  if (ifFalse)
    ifFalse->check(state);
}

void VariableDeclaration::check(State& state) {
  state.checkNameConflict(codeLoc, identifier, "Variable");
  auto inferType = [&] () -> optional<Type> {
    if (type) {
      if (auto typeId = state.getTypeFromString(*type))
        return typeId;
      else {
        codeLoc.error("Type " + quote(type->toString()) + " not recognized");
        return none;
      }
    }
    else if (initExpr)
      return initExpr->getType(state);
    else {
      codeLoc.error("Initializing expression needed to infer variable type");
      return none;
    }
  };
  if (auto typeId = inferType()) {
    realType = *typeId;
    INFO << "Adding variable " << identifier << " of type " << getName(*typeId);
    if (initExpr) {
      auto exprType = initExpr->getType(state);
      initExpr->codeLoc.check(canAssign(ReferenceType(*typeId), exprType), "Can't initialize variable of type "
          + quote(getName(*typeId)) + " with value of type " + quote(getName(exprType)));
    } else
      codeLoc.check(!requiresInitialization(*typeId), "Type " + quote(getName(*typeId)) + " requires initialization");
    state.addVariable(identifier, ReferenceType(*typeId));
  }
}

void ReturnStatement::check(State& state) {
  if (!expr)
    codeLoc.check(state.getReturnType() && *state.getReturnType() == ArithmeticType::VOID,
        "Expected an expression in return statement in a function returning non-void");
  else {
    auto returnType = expr->getType(state);
    codeLoc.check(canAssign(ReferenceType(*state.getReturnType()), returnType),
        "Attempting to return value of type "s + quote(getName(returnType)) +
         " in a function returning "s + quote(getName(*state.getReturnType())));
  }
}

bool Statement::hasReturnStatement(const State&) const {
  return false;
}

bool IfStatement::hasReturnStatement(const State& state) const {
  return ifTrue->hasReturnStatement(state) && ifFalse && ifFalse->hasReturnStatement(state);
}

bool StatementBlock::hasReturnStatement(const State& state) const {
  for (auto& s : elems)
    if (s->hasReturnStatement(state))
      return true;
  return false;
}

bool ReturnStatement::hasReturnStatement(const State&) const {
  return true;
}

optional<FunctionType> FunctionDefinition::addToState(const State& state) {
  State stateCopy = state;
  state.checkNameConflict(codeLoc, name, "Function");
  vector<Type> templateTypes;
  for (auto& param : templateParams) {
    templateTypes.push_back(TemplateParameter{param});
    stateCopy.addType(param, templateTypes.back());
  }
  if (auto returnType = stateCopy.getTypeFromString(this->returnType)) {
    vector<FunctionType::Param> params;
    for (auto& p : parameters)
      if (auto paramType = stateCopy.getTypeFromString(p.type)) {
        params.push_back({p.name, *paramType});
      } else
        p.codeLoc.error("Unrecognized parameter type: " + quote(p.type.toString()));
    return FunctionType(FunctionCallType::FUNCTION, *returnType, params, templateTypes );
  } else {
    codeLoc.error("Unrecognized return type: " + this->returnType.toString());
    return none;
  }
}

void FunctionDefinition::checkFunction(State& state, bool templateStruct) {
  State stateCopy = state;
  vector<Type> templateTypes;
  for (auto& param : templateParams) {
    templateTypes.push_back(TemplateParameter{param});
    stateCopy.addType(param, templateTypes.back());
  }
  if (auto returnType = stateCopy.getTypeFromString(this->returnType)) {
    for (auto& p : parameters)
      if (auto paramType = stateCopy.getTypeFromString(p.type))
        stateCopy.addVariable(p.name, *paramType);
      else
        p.codeLoc.error("Unrecognized parameter type: " + quote(p.type.toString()));
    stateCopy.setReturnType(*returnType);
    if (*returnType != ArithmeticType::VOID && body && !body->hasReturnStatement(state))
      codeLoc.error("Not all paths lead to a return statement in a function returning non-void");
  } else
    codeLoc.error("Unrecognized return type: " + this->returnType.toString());
  if (body && (!templateParams.empty() || templateStruct || state.getImports().empty()))
    body->check(stateCopy);
}

void FunctionDefinition::check(State& state) {
  state.addFunction(name, *addToState(state));
  checkFunction(state, false);
}

void correctness(const AST& ast) {
  State state;
  state.addType("int", ArithmeticType::INT);
  state.addType("bool", ArithmeticType::BOOL);
  state.addType("void", ArithmeticType::VOID);
  for (auto& elem : ast.elems) {
    elem->check(state);
  }
}

ExpressionStatement::ExpressionStatement(unique_ptr<Expression> e) : Statement(e->codeLoc), expr(std::move(e)) {}

void ExpressionStatement::check(State& state) {
  expr->getType(state);
}

StructDefinition::StructDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

Type FunctionCall::getType(const State& state) {
  return *getDotOperatorType(state, state);
}

static FunctionType getFunction(const State& idContext, const State& callContext, CodeLoc codeLoc, IdentifierInfo id,
    const vector<Type>& argTypes, const vector<CodeLoc>& argLoc) {
  auto templateType = idContext.getFunctionTemplate(codeLoc, id);
  return callContext.instantiateFunctionTemplate(codeLoc, templateType, id, argTypes, argLoc);
}

optional<Type> FunctionCall::getDotOperatorType(const State& idContext, const State& callContext) {
  vector<Type> argTypes;
  vector<CodeLoc> argLocs;
  for (int i = 0; i < arguments.size(); ++i) {
    argTypes.push_back(arguments[i]->getType(callContext));
    argLocs.push_back(arguments[i]->codeLoc);
    INFO << "Function argument " << getName(argTypes.back());
  }
  auto type = getFunction(idContext, callContext, codeLoc, identifier, argTypes, argLocs);
  callType = type.callType;
  return *type.retVal;
}

FunctionCallNamedArgs::FunctionCallNamedArgs(CodeLoc l, IdentifierInfo id) : Expression(l), identifier(id) {}

Type FunctionCallNamedArgs::getType(const State& state) {
  return *getDotOperatorType(state, state);
}

optional<Type> FunctionCallNamedArgs::getDotOperatorType(const State& idContext, const State& callContext) {
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
  vector<Type> argTypes;
  vector<CodeLoc> argLocs;
  for (auto& arg : arguments) {
    argTypes.push_back(arg.expr->getType(callContext));
    argLocs.push_back(arg.codeLoc);
  }
  auto type = getFunction(idContext, callContext, codeLoc, identifier, argTypes, argLocs);
  callType = type.callType;
  return *type.retVal;
}

SwitchStatement::SwitchStatement(CodeLoc l, unique_ptr<Expression> e) : Statement(l), expr(std::move(e)) {}

void SwitchStatement::check(State& state) {
  auto exprType = getUnderlying(expr->getType(state));
  auto inputType = exprType.getReferenceMaybe<VariantType>();
  expr->codeLoc.check(!!inputType, "Expected a variant type, got " + quote(getName(exprType)));
  expr->codeLoc.check(!!inputType, "Expected variant type in switch input, got " + quote(getName(exprType)));
  subtypesPrefix = inputType->name;
  if (!inputType->templateParams.empty()) {
    subtypesPrefix += "<";
    for (auto& t : inputType->templateParams)
      subtypesPrefix += getName(t) + ",";
    subtypesPrefix.pop_back();
    subtypesPrefix += ">";
  }
  subtypesPrefix += "::";  unordered_set<string> handledTypes;
  for (auto& caseElem : caseElems) {
    caseElem.codeloc.check(inputType->types.count(caseElem.id), "Element " + quote(caseElem.id) + " not present in " +
        quote(getName(exprType)));
    caseElem.codeloc.check(!handledTypes.count(caseElem.id), "Variant element " + quote(caseElem.id)
        + " handled more than once in switch statement");
    handledTypes.insert(caseElem.id);
    auto stateCopy = state;
    auto realType = inputType->types.at(caseElem.id);
    caseElem.declareVar = !(realType == ArithmeticType::VOID);
    if (caseElem.declareVar)
      stateCopy.addVariable(caseElem.id, realType);
    if (caseElem.type) {
      if (auto t = state.getTypeFromString(*caseElem.type))
        caseElem.type->codeLoc.check(*t == inputType->types.at(caseElem.id), "Can't handle variant element "
            + quote(caseElem.id) + " of type " + quote(getName(realType)) + " as type " + quote(getName(*t)));
    }
    caseElem.block->check(stateCopy);
  }
  if (!defaultBlock) {
    vector<string> unhandled;
    for (auto& elem : inputType->types)
      if (!handledTypes.count(elem.first))
        unhandled.push_back(quote(elem.first));
    codeLoc.check(unhandled.empty(), quote(getName(exprType)) + " subtypes " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    defaultBlock->codeLoc.check(handledTypes.size() < inputType->types.size(), "Default switch statement unnecessary when all "
        "variant cases are handled");
    defaultBlock->check(state);
  }
}

bool SwitchStatement::hasReturnStatement(const State& state) const {
  for (auto& elem : caseElems)
    if (!elem.block->hasReturnStatement(state))
      return false;
  if (defaultBlock && !defaultBlock->hasReturnStatement(state))
    return false;
  return true;
}

VariantDefinition::VariantDefinition(CodeLoc l, string n) : Statement(l), name(n) {
}

void VariantDefinition::check(State& state) {
  state.checkNameConflict(codeLoc, name, "Type");
  VariantType type(name);
  unordered_set<string> subtypeNames;
  State stateCopy = state;
  for (auto& param : templateParams) {
    type.templateParams.push_back(TemplateParameter{param});
    stateCopy.addType(param, type.templateParams.back());
  }
  struct ConstructorInfo {
    string subtypeName;
    FunctionCallType callType;
    vector<FunctionType::Param> constructorParams;
  };
  vector<ConstructorInfo> constructors;
  for (auto& subtype : elements) {
    ConstructorInfo constructorInfo;
    constructorInfo.callType = FunctionCallType::FUNCTION;
    constructorInfo.subtypeName = subtype.name;
    subtype.codeLoc.check(!subtypeNames.count(subtype.name), "Duplicate variant alternative: " + quote(subtype.name));
    subtypeNames.insert(subtype.name);
    if (auto subtypeInfo = stateCopy.getTypeFromString(subtype.type)) {
      if (!(*subtypeInfo == ArithmeticType::VOID))
        constructorInfo.constructorParams.push_back(FunctionType::Param{"", *subtypeInfo});
      type.types.insert({subtype.name, *subtypeInfo});
    } else
      subtype.codeLoc.error("Unrecognized type: " + quote(subtype.type.toString()));
    constructors.push_back(constructorInfo);
  }
  vector<FunctionType> methodTypes;
  for (auto& method : methods) {
    methodTypes.push_back(*method->addToState(stateCopy));
    stateCopy.addFunction(method->name, methodTypes.back());
  }
  stateCopy.addVariable("this", PointerType(type));
  for (int i = 0; i < methods.size(); ++i) {
    methods[i]->checkFunction(stateCopy, !templateParams.empty());
    type.methods.push_back({methods[i]->name, methodTypes[i]});
  }
  for (auto& elem : constructors)
    type.staticMethods.push_back({elem.subtypeName, FunctionType(elem.callType, type, elem.constructorParams, {})});
  state.addType(name, type);
}

void StructDefinition::check(State& state) {
  state.checkNameConflict(codeLoc, name, "Type");
  StructType type(name);
  auto stateCopy = state;
  for (auto& param : templateParams) {
    type.templateParams.push_back(TemplateParameter{param});
    stateCopy.addType(param, type.templateParams.back());
  }
  auto methodState = stateCopy;
  for (auto& member : members) {
    INFO << "Struct member " << member.name << " " << member.type.toString() << " line " << member.codeLoc.line << " column " << member.codeLoc.column;
    if (auto memberType = stateCopy.getTypeFromString(member.type)) {
      type.members.push_back({member.name, *memberType});
      methodState.addVariable(member.name, ReferenceType(*memberType));
    } else
      member.codeLoc.error("Type " + quote(member.type.toString()) + " not recognized");
  }
  vector<FunctionType> methodTypes;
  for (auto& method : methods) {
    methodTypes.push_back(*method->addToState(methodState));
    methodState.addFunction(method->name, methodTypes.back());
  }
  for (int i = 0; i < methods.size(); ++i) {
    methods[i]->checkFunction(methodState, !templateParams.empty());
    type.methods.push_back({methods[i]->name, methodTypes[i]});
  }
  state.addType(name, type);
  vector<FunctionType::Param> constructorParams;
  for (auto& member : type.members)
    constructorParams.push_back({member.name, *member.type});
  state.addFunction(name, FunctionType(FunctionCallType::CONSTRUCTOR, type, std::move(constructorParams), type.templateParams));
}

UnaryExpression::UnaryExpression(CodeLoc l, Operator o, unique_ptr<Expression> e)
    : Expression(l), op(o), expr(std::move(e)) {}

Type UnaryExpression::getType(const State& state) {
  return getUnaryOperationResult(expr->codeLoc, op, expr->getType(state));
}

EmbedStatement::EmbedStatement(CodeLoc l, string v) : Statement(l), value(v) {
}

void EmbedStatement::check(State&) {
}

ForLoopStatement::ForLoopStatement(CodeLoc l, unique_ptr<Statement> i, unique_ptr<Expression> c,
    unique_ptr<Expression> it, unique_ptr<Statement> b)
    : Statement(l), init(std::move(i)), cond(std::move(c)), iter(std::move(it)), body(std::move(b)) {}

bool ForLoopStatement::hasReturnStatement(const State& s) const {
  return body->hasReturnStatement(s);
}

void ForLoopStatement::check(State& s) {
  auto stateCopy = s;
  init->check(stateCopy);
  cond->codeLoc.check(cond->getType(stateCopy) == ArithmeticType::BOOL,
      "Loop condition must be of type " + quote("bool"));
  iter->getType(stateCopy);
  body->check(stateCopy);
}

ImportStatement::ImportStatement(CodeLoc l, string p, bool pub) : Statement(l), path(p), isPublic(pub) {
}

void ImportStatement::check(State& s) {
  if ((!isPublic && !s.getImports().empty()) || contains(s.getAllImports(), path))
    return;
  codeLoc.check(!contains(s.getImports(), path), "Public import cycle: " + combine(s.getImports(), ", "));
  s.pushImport(path);
  string content = readFromFile(path.c_str(), codeLoc);
  INFO << "Imported file " << path;
  auto tokens = lex(content, path);
  ast = make_unique<AST>(parse(tokens));
  for (auto& elem : ast->elems)
    elem->check(s);
  s.popImport();
}

optional<Type> Expression::getDotOperatorType(const State& idContext, const State& callContext) {
  return none;
}
