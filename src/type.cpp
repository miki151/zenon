#include "type.h"
#include "state.h"
#include "ast.h"

ArithmeticType::DefType ArithmeticType::INT = shared<ArithmeticType>("int");
ArithmeticType::DefType ArithmeticType::VOID = shared<ArithmeticType>("void");
ArithmeticType::DefType ArithmeticType::BOOL = shared<ArithmeticType>("bool");
ArithmeticType::DefType ArithmeticType::STRING = shared<ArithmeticType>("string");
ArithmeticType::DefType ArithmeticType::CHAR = shared<ArithmeticType>("char");

string getTemplateParamNames(const vector<SType>& templateParams) {
  string ret;
  if (!templateParams.empty()) {
    auto paramNames = transform(templateParams, [](auto& e) { return e->getName(); });
    ret += "<" + combine(paramNames, ",") + ">";
  }
  return ret;
}

string ArithmeticType::getName() const {
  return name;
}

ArithmeticType::ArithmeticType(const char* name) : name(name) {
}

string ReferenceType::getName() const {
  return "reference("s + underlying->getName() + ")";
}

string PointerType::getName() const {
  return underlying->getName() + "*";
}

string StructType::getName() const {
  return name + getTemplateParamNames(templateParams);
}

string TemplateParameterType::getName() const {
  return name;
}

string EnumType::getName() const {
  return name;
}

void EnumType::handleSwitchStatement(SwitchStatement& statement, State& state, CodeLoc codeLoc) const {
  statement.type = SwitchStatement::ENUM;
  unordered_set<string> handledElems;
  statement.subtypesPrefix = name + "::";
  for (auto& caseElem : statement.caseElems) {
    caseElem.codeloc.check(!caseElem.type, "Expected enum element");
    caseElem.codeloc.check(contains(elements, caseElem.id), "Element " + quote(caseElem.id) +
        " not present in enum" + quote(name));
    caseElem.codeloc.check(!handledElems.count(caseElem.id), "Enum element " + quote(caseElem.id)
        + " handled more than once in switch statement");
    handledElems.insert(caseElem.id);
    caseElem.block->check(state);
  }
  if (!statement.defaultBlock) {
    vector<string> unhandled;
    for (auto& elem : elements)
      if (!handledElems.count(elem))
        unhandled.push_back(quote(elem));
    codeLoc.check(unhandled.empty(), quote(name) + " elements " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    statement.defaultBlock->codeLoc.check(handledElems.size() < elements.size(),
        "Default switch statement unnecessary when all enum elements are handled");
    statement.defaultBlock->check(state);
  }
}

int getNewId() {
  static int idCounter = 0;
  return ++idCounter;
}

FunctionType::FunctionType(FunctionCallType t, SType returnType, vector<Param> p, vector<SType> tpl)
    : callType(t), retVal(std::move(returnType)), params(std::move(p)), templateParams(tpl) {
}

SType Type::getUnderlying() {
  return get_this().get();
}

SType ReferenceType::getUnderlying() {
  return underlying;
}

shared_ptr<ReferenceType> ReferenceType::get(SType type) {
  static map<SType, shared_ptr<ReferenceType>> generated;
  if (!generated.count(type))
    generated.insert({type, shared<ReferenceType>(type)});
  return generated.at(type);
}

ReferenceType::ReferenceType(SType t) : underlying(t->getUnderlying()) {
}

shared_ptr<PointerType> PointerType::get(SType type) {
  static map<SType, shared_ptr<PointerType>> generated;
  if (!generated.count(type))
    generated.insert({type, shared<PointerType>(type)});
  return generated.at(type);
}

PointerType::PointerType(SType t) : underlying(t->getUnderlying()) {
}

bool Type::canAssign(SType from) const{
  return false;
}

bool ReferenceType::canAssign(SType from) const {
  return underlying == from->getUnderlying();
}

shared_ptr<StructType> StructType::get(Kind kind, string name) {
  auto ret = shared<StructType>();
  ret->kind = kind;
  ret->name = name;
  ret->parent = ret;
  return ret;
}

static State getStringTypeContext() {
  State ret;
  ret.addFunction("size"s, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::INT, {}, {}));
  ret.addFunction(Operator::SUBSCRIPT, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::CHAR,
      {{"index", ArithmeticType::INT}}, {}));
  return ret;
}

optional<State> ArithmeticType::getTypeContext() const {
  if (get_this().get() == ArithmeticType::STRING)
    return getStringTypeContext();
  else
    return none;
}

optional<State> ReferenceType::getTypeContext() const {
  return underlying->getTypeContext();
}

void ReferenceType::handleSwitchStatement(SwitchStatement& statement, State& state, CodeLoc codeLoc) const {
  underlying->handleSwitchStatement(statement, state, codeLoc);
}

optional<State> StructType::getTypeContext() const {
  State ret = state;
  for (auto& method : methods)
    ret.addFunction(method.nameOrOp, *method.type);
  return ret;
}

void StructType::handleSwitchStatement(SwitchStatement& statement, State& outsideState, CodeLoc codeLoc) const {
  codeLoc.check(kind == StructType::VARIANT, "Expected a variant or enum type");
  statement.type = SwitchStatement::VARIANT;
  statement.subtypesPrefix = name;
  if (!templateParams.empty()) {
    statement.subtypesPrefix += "<";
    for (auto& t : templateParams)
      statement.subtypesPrefix += t->getName() + ",";
    statement.subtypesPrefix.pop_back();
    statement.subtypesPrefix += ">";
  }
  statement.subtypesPrefix += "::";
  unordered_set<string> handledTypes;
  for (auto& caseElem : statement.caseElems) {
    caseElem.codeloc.check(!!state.getAlternatives().getType(caseElem.id), "Element " + quote(caseElem.id) +
        " not present in variant " + quote(name));
    caseElem.codeloc.check(!handledTypes.count(caseElem.id), "Variant element " + quote(caseElem.id)
        + " handled more than once in switch statement");
    handledTypes.insert(caseElem.id);
    auto caseBodyState = outsideState;
    auto realType = state.getAlternatives().getType(caseElem.id).get();
    caseElem.declareVar = !(realType == ArithmeticType::VOID);
    if (caseElem.declareVar)
      caseBodyState.getVariables().add(caseElem.id, realType);
    if (caseElem.type) {
      if (auto t = outsideState.getTypeFromString(*caseElem.type))
        caseElem.type->codeLoc.check(t == realType, "Can't handle variant element "
            + quote(caseElem.id) + " of type " + quote(realType->getName()) + " as type " + quote(t->getName()));
    }
    caseElem.block->check(caseBodyState);
  }
  if (!statement.defaultBlock) {
    vector<string> unhandled;
    for (auto& member : state.getAlternatives().getNames())
      if (!handledTypes.count(member))
        unhandled.push_back(quote(member));
    codeLoc.check(unhandled.empty(), quote(name) + " subtypes " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    statement.defaultBlock->codeLoc.check(handledTypes.size() < state.getAlternatives().getNames().size(),
        "Default switch statement unnecessary when all variant cases are handled");
    statement.defaultBlock->check(outsideState);
  }
}

shared_ptr<StructType> StructType::getInstance(vector<SType> newTemplateParams) {
  auto self = get_this().get().dynamicCast<StructType>();
  if (templateParams == newTemplateParams)
    return self;
  for (auto type : instantations) {
    if (type->templateParams == newTemplateParams)
      return type;
  }
  auto type = StructType::get(kind, name);
  type->parent = self;
  instantations.push_back(type);
  return type;
}

void replaceInFunction(FunctionType&, SType from, SType to);

void StructType::updateInstantations() {
  for (auto type1 : instantations) {
    auto type = type1.dynamicCast<StructType>();
    for (int i = 0; i < templateParams.size(); ++i) {
      type->methods = methods;
      for (auto& method : type->methods)
        replaceInFunction(*method.type, templateParams[i], type->templateParams[i]);
      type->staticMethods = staticMethods;
      for (auto& method : type->staticMethods)
        replaceInFunction(method.second, templateParams[i], type->templateParams[i]);
      type->state = state;
      type->state.replace(templateParams[i], type->templateParams[i]);
    }
  }
}

bool canConvert(SType from, SType to) {
  return from->getUnderlying() == to;
}

bool requiresInitialization(SType) {
  return true;
}

TemplateParameterType::TemplateParameterType(string n, CodeLoc l) : name(n), declarationLoc(l) {}

SType Type::replace(SType from, SType to) const {
  return get_this().get();
}

SType ReferenceType::replace(SType from, SType to) const {
  return ReferenceType::get(underlying->replace(from, to));
}

SType PointerType::replace(SType from, SType to) const {
  return PointerType::get(underlying->replace(from, to));
}

SType TemplateParameterType::replace(SType from, SType to) const {
  auto self = get_this().get();
  if (from == self)
    return to;
  else
    return self;
}

SType StructType::replace(SType from, SType to) const {
  vector<SType> newTemplateParams;
  for (auto& param : templateParams)
    newTemplateParams.push_back(param->replace(from, to));
  auto ret = parent->getInstance(newTemplateParams);
  // This is how we check if instantiate gave us a new type to fill
  if (ret->templateParams != newTemplateParams) {
    ret->templateParams = newTemplateParams;
    INFO << "New instantiation: " << ret->getName();
    ret->state = state;
    auto checkVoidMembers = [&] (const Variables& vars) {
      for (auto& member : vars.getNames()) {
        auto memberType = vars.getType(member).get();
        if (auto param = memberType.dynamicCast<TemplateParameterType>())
          param->declarationLoc.check(to != ArithmeticType::VOID,
              "Can't instantiate member type with type " + quote(ArithmeticType::VOID->getName()));
      }
    };
    checkVoidMembers(ret->state.getVariables());
    checkVoidMembers(ret->state.getAlternatives());
    checkVoidMembers(ret->state.getConstants());
    ret->state.replace(from, to);
    for (auto& method : methods) {
      ret->methods.push_back(method);
      replaceInFunction(*ret->methods.back().type, from, to);
    }
    for (auto& method : staticMethods) {
      ret->staticMethods.push_back(method);
      replaceInFunction(ret->staticMethods.back().second, from, to);
    }
  } else
    INFO << "Found instantiated: " << ret->getName();
  return ret;
}

void replaceInFunction(FunctionType& in, SType from, SType to) {
  in.retVal = in.retVal->replace(from, to);
  for (auto& param : in.params)
    param.type = param.type->replace(from, to);
}

nullable<SType> Type::instantiate(vector<SType> templateParams) const {
  if (templateParams.empty())
    return get_this().get();
  else
    return nullptr;
}

optional<State> Type::getTypeContext() const {
  return none;
}

optional<FunctionType> Type::getStaticMethod(const string&) const {
  return none;
}

void Type::handleSwitchStatement(SwitchStatement&, State&, CodeLoc codeLoc) const {
  codeLoc.error("Can't switch on value of type " + quote(getName()));
}

optional<FunctionType> StructType::getStaticMethod(const string& methodName) const {
  for (auto& elem : staticMethods)
    if (elem.first == methodName)
      return elem.second;
  return none;
}

nullable<SType> StructType::instantiate(vector<SType> newTemplateParams) const {
  if (newTemplateParams.size() != templateParams.size())
    return nullptr;
  auto ret = get_this().get();
  for (int i = 0; i < templateParams.size(); ++i)
    ret = ret->replace(templateParams[i], newTemplateParams[i]);
  return ret;
}

struct TypeMapping {
  vector<SType> templateParams;
  vector<nullable<SType>> templateArgs;
  optional<int> getParamIndex(const SType& t) {
    for (int i = 0; i < templateParams.size(); ++i)
      if (templateParams[i] == t)
        return i;
    return none;
  }
};

bool canDeduce(TypeMapping& mapping, SType paramType, SType argType) {
  if (auto refType = argType.dynamicCast<ReferenceType>()) {
    argType = refType->underlying;
    if (auto refType = paramType.dynamicCast<ReferenceType>())
      paramType = refType->underlying;
  }
  if (auto index = mapping.getParamIndex(paramType)) {
    auto& arg = mapping.templateArgs.at(*index);
    if (arg && arg != argType)
      return false;
    arg = argType;
    return true;
  } else
    return paramType->canMap(mapping, argType);
}

bool StructType::canMap(TypeMapping& mapping, SType argType) const {
  auto argStruct = argType.dynamicCast<StructType>();
  if (!argStruct || parent.get() != argStruct->parent.get())
    return false;
  for (int i = 0; i < templateParams.size(); ++i)
    if (!::canDeduce(mapping, templateParams[i], argStruct->templateParams[i]))
      return false;
  return true;
}

bool PointerType::canMap(TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<PointerType>())
    return ::canDeduce(mapping, underlying, argPointer->underlying);
  return false;
}

bool ReferenceType::canMap(TypeMapping&, SType from) const {
  return from == get_this().get() || underlying == from;
}


bool Type::canMap(TypeMapping&, SType argType) const {
  return argType == get_this().get();
}

void instantiateFunction(FunctionType& type, CodeLoc codeLoc, vector<SType> templateArgs, vector<SType> argTypes,
    vector<CodeLoc> argLoc) {
  vector<SType> funParams = transform(type.params, [](const FunctionType::Param& p) { return p.type; });
  codeLoc.check(templateArgs.size() <= type.templateParams.size(), "Too many template arguments.");
  TypeMapping mapping { type.templateParams, vector<nullable<SType>>(type.templateParams.size()) };
  for (int i = 0; i < templateArgs.size(); ++i)
    mapping.templateArgs[i] = templateArgs[i];
  codeLoc.check(funParams.size() == argTypes.size(), "Wrong number of function arguments.");
  for (int i = 0; i < argTypes.size(); ++i)
    if (!canDeduce(mapping, funParams[i], argTypes[i])) {
      string deducedAsString;
      if (auto index = mapping.getParamIndex(funParams[i]))
        if (auto deduced = mapping.templateArgs.at(*index))
          deducedAsString = ", deduced as " + quote(deduced->getName());
      argLoc[i].error("Can't bind argument of type "
        + quote(argTypes[i]->getName()) + " to parameter " + quote(funParams[i]->getName()) + deducedAsString);
    }
  for (int i = 0; i < type.templateParams.size(); ++i) {
    if (i >= templateArgs.size()) {
      if (auto deduced = mapping.templateArgs[i])
        templateArgs.push_back(deduced.get());
      else
        codeLoc.error("Couldn't deduce template argument " + quote(type.templateParams[i]->getName()));
    }
    replaceInFunction(type, type.templateParams[i], templateArgs[i]);
    type.templateParams[i] = templateArgs[i];
  }
}

EnumType::EnumType(string n, vector<string> e) : name(n), elements(e) {}

