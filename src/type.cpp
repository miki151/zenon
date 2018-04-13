#include "type.h"
#include "state.h"

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

string TemplateParameter::getName() const {
  return name;
}

string EnumType::getName() const {
  return name;
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

nullable<SType> StructType::getMember(const string& name) const {
  for (auto& member : members)
    if (member.name == name)
      return member.type;
  return nullptr;
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

optional<State> StructType::getTypeContext() const {
  State state;
  if (kind != VARIANT)
    for (auto& member : members)
      state.addVariable(member.name, ReferenceType::get(member.type));
  for (auto& method : methods)
    state.addFunction(method.nameOrOp, *method.type);
  return state;
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
      type->members = members;
      for (auto& member : type->members)
        member.type = member.type->replace(templateParams[i], type->templateParams[i]);
    }
  }
}

bool canConvert(SType from, SType to) {
  return from->getUnderlying() == to;
}

bool requiresInitialization(SType) {
  return true;
}

TemplateParameter::TemplateParameter(string n) : name(n) {}

SType Type::replace(SType from, SType to) const {
  return get_this().get();
}

SType ReferenceType::replace(SType from, SType to) const {
  return ReferenceType::get(underlying->replace(from, to));
}

SType PointerType::replace(SType from, SType to) const {
  return PointerType::get(underlying->replace(from, to));
}

SType TemplateParameter::replace(SType from, SType to) const {
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
    for (auto& member : members)
      ret->members.push_back({member.name, member.type->replace(from, to)});
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

