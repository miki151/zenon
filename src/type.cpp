#include "type.h"
#include "context.h"
#include "ast.h"

ArithmeticType::DefType ArithmeticType::INT = shared<ArithmeticType>("int");
ArithmeticType::DefType ArithmeticType::VOID = shared<ArithmeticType>("void");
ArithmeticType::DefType ArithmeticType::BOOL = shared<ArithmeticType>("bool");
SType Type::STRING = shared<StringType>("string");
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

void EnumType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc) const {
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
    caseElem.block->check(context);
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
    statement.defaultBlock->check(context);
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

const Context& ReferenceType::getContext() const {
  return underlying->getContext();
}

void ReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc) const {
  underlying->handleSwitchStatement(statement, context, codeLoc);
}

void StructType::handleSwitchStatement(SwitchStatement& statement, Context& outsideContext, CodeLoc codeLoc) const {
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
    caseElem.codeloc.check(!!context.getAlternatives().getType(caseElem.id), "Element " + quote(caseElem.id) +
        " not present in variant " + quote(name));
    caseElem.codeloc.check(!handledTypes.count(caseElem.id), "Variant element " + quote(caseElem.id)
        + " handled more than once in switch statement");
    handledTypes.insert(caseElem.id);
    auto caseBodyContext = outsideContext;
    auto realType = context.getAlternatives().getType(caseElem.id).get();
    caseElem.declareVar = !(realType == ArithmeticType::VOID);
    if (caseElem.declareVar)
      caseBodyContext.getVariables().add(caseElem.id, realType);
    if (caseElem.type) {
      if (auto t = outsideContext.getTypeFromString(*caseElem.type))
        caseElem.type->codeLoc.check(t == realType, "Can't handle variant element "
            + quote(caseElem.id) + " of type " + quote(realType->getName()) + " as type " + quote(t->getName()));
    }
    caseElem.block->check(caseBodyContext);
  }
  if (!statement.defaultBlock) {
    vector<string> unhandled;
    for (auto& member : context.getAlternatives().getNames())
      if (!handledTypes.count(member))
        unhandled.push_back(quote(member));
    codeLoc.check(unhandled.empty(), quote(name) + " subtypes " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    statement.defaultBlock->codeLoc.check(handledTypes.size() < context.getAlternatives().getNames().size(),
        "Default switch statement unnecessary when all variant cases are handled");
    statement.defaultBlock->check(outsideContext);
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

void StructType::updateInstantations() {
  for (auto type1 : instantations) {
    auto type = type1.dynamicCast<StructType>();
    for (int i = 0; i < templateParams.size(); ++i) {
      type->context = context;
      type->staticContext = staticContext;
      type->context.replace(templateParams[i], type->templateParams[i]);
      type->staticContext.replace(templateParams[i], type->templateParams[i]);
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
    ret->context = context;
    ret->staticContext = staticContext;
    auto checkVoidMembers = [&] (const Variables& vars) {
      for (auto& member : vars.getNames()) {
        auto memberType = vars.getType(member).get();
        if (auto param = memberType.dynamicCast<TemplateParameterType>())
          param->declarationLoc.check(to != ArithmeticType::VOID,
              "Can't instantiate member type with type " + quote(ArithmeticType::VOID->getName()));
      }
    };
    checkVoidMembers(ret->context.getVariables());
    checkVoidMembers(ret->context.getAlternatives());
    checkVoidMembers(ret->context.getConstants());
    ret->context.replace(from, to);
    ret->staticContext.replace(from, to);
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

const Context& Type::getContext() const {
  static Context empty;
  return empty;
}

const Context& Type::getStaticContext() const {
  static Context empty;
  return empty;
}

void Type::handleSwitchStatement(SwitchStatement&, Context&, CodeLoc codeLoc) const {
  codeLoc.error("Can't switch on value of type " + quote(getName()));
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


const Context& TypeWithContext::getContext() const {
  return context;
}

const Context& TypeWithContext::getStaticContext() const {
  return staticContext;
}

static Context getStringTypeContext() {
  Context ret;
  ret.addFunction("size"s, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::INT, {}, {}));
  ret.addFunction(Operator::SUBSCRIPT, FunctionType(FunctionCallType::FUNCTION, ArithmeticType::CHAR,
      {{"index", ArithmeticType::INT}}, {}));
  return ret;
}

const Context& StringType::getContext() const {
  static Context s = getStringTypeContext();
  return s;
}
