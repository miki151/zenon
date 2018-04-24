#include "type.h"
#include "context.h"
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

ArithmeticType::ArithmeticType(const string& name) : name(name) {
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

string FunctionType::toString(const string& name) const {
  return retVal->getName() + " " + name + joinTemplateParams(templateParams) + "(" +
      combine(transform(params, [](const Param& t) { return t.type->getName(); }), ", ") + ")";
}

SType Type::getUnderlying() {
  return get_this().get();
}

SType ReferenceType::getUnderlying() {
  return underlying;
}

shared_ptr<ReferenceType> ReferenceType::get(SType type) {
  static map<SType, shared_ptr<ReferenceType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<ReferenceType>(type);
    generated.insert({type, ret});
    ret->context.addFunction(Operator::GET_ADDRESS,
        FunctionType(FunctionCallType::FUNCTION, PointerType::get(type), {}, {}));
    ret->context.addFunction(Operator::ASSIGNMENT,
        FunctionType(FunctionCallType::FUNCTION, ret, {{"right side", type}}, {}));
  }
  return generated.at(type);
}

ReferenceType::ReferenceType(SType t) : underlying(t->getUnderlying()) {
  context.merge(underlying->getContext());
}

shared_ptr<PointerType> PointerType::get(SType type) {
  static map<SType, shared_ptr<PointerType>> generated;
  if (!generated.count(type))
    generated.insert({type, shared<PointerType>(type)});
  return generated.at(type);
}

PointerType::PointerType(SType t) : underlying(t->getUnderlying()) {
  context.addFunction(Operator::POINTER_DEREFERENCE,
      FunctionType(FunctionCallType::FUNCTION, ReferenceType::get(t), {}, {}));
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
  auto getAlternativeType = [&] (const string& name) -> nullable<SType> {
    for (auto& alternative : alternatives)
      if (alternative.name == name)
        return alternative.type;
    return nullptr;
  };
  for (auto& caseElem : statement.caseElems) {
    caseElem.codeloc.check(!!getAlternativeType(caseElem.id), "Element " + quote(caseElem.id) +
        " not present in variant " + quote(getName()));
    caseElem.codeloc.check(!handledTypes.count(caseElem.id), "Variant element " + quote(caseElem.id)
        + " handled more than once in switch statement");
    handledTypes.insert(caseElem.id);
    auto caseBodyContext = Context::withParent(outsideContext);
    auto realType = getAlternativeType(caseElem.id).get();
    caseElem.declareVar = !(realType == ArithmeticType::VOID);
    if (caseElem.declareVar)
      caseBodyContext.addVariable(caseElem.id, ReferenceType::get(realType));
    if (caseElem.type) {
      if (auto t = outsideContext.getTypeFromString(*caseElem.type))
        caseElem.type->codeLoc.check(t == realType, "Can't handle variant element "
            + quote(caseElem.id) + " of type " + quote(realType->getName()) + " as type " + quote(t->getName()));
    }
    caseElem.block->check(caseBodyContext);
  }
  if (!statement.defaultBlock) {
    vector<string> unhandled;
    for (auto& alternative : alternatives)
      if (!handledTypes.count(alternative.name))
        unhandled.push_back(quote(alternative.name));
    codeLoc.check(unhandled.empty(), quote(name) + " subtypes " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    statement.defaultBlock->codeLoc.check(handledTypes.size() < alternatives.size(),
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
  type->alternatives = alternatives;
  type->parent = self;
  instantations.push_back(type);
  return type;
}

void StructType::updateInstantations() {
  for (auto type1 : instantations) {
    auto type = type1.dynamicCast<StructType>();
    for (int i = 0; i < templateParams.size(); ++i) {
      type->context.deepCopyFrom(context);
      type->staticContext.deepCopyFrom(staticContext);
      type->context.replace(templateParams[i], type->templateParams[i]);
      type->staticContext.replace(templateParams[i], type->templateParams[i]);
      type->alternatives = alternatives;
      for (auto& alternative : type->alternatives)
        alternative.type = alternative.type->replace(templateParams[i], type->templateParams[i]);
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
  auto self = get_this().get();
  if (from == self)
    return to;
  else
    return self;
}

SType ReferenceType::replace(SType from, SType to) const {
  return ReferenceType::get(underlying->replace(from, to));
}

SType PointerType::replace(SType from, SType to) const {
  return PointerType::get(underlying->replace(from, to));
}

static void checkNonVoidMember (const SType& type, const SType& to) {
  if (auto param = type.dynamicCast<TemplateParameterType>())
    param->declarationLoc.check(to != ArithmeticType::VOID,
        "Can't instantiate member type with type " + quote(ArithmeticType::VOID->getName()));
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
    ret->context.deepCopyFrom(context);
    ret->staticContext.deepCopyFrom(staticContext);
    for (auto& member : ret->context.getBottomLevelVariables()) {
      auto memberType = ret->context.getTypeOfVariable(member).get();
      checkNonVoidMember(memberType, to);
    }
    ret->alternatives = alternatives;
    for (auto& alternative : ret->alternatives) {
      checkNonVoidMember(alternative.type, to);
      alternative.type = alternative.type->replace(from, to);
    }
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
  if (in.parentConcept) {
    in.parentConcept = shared<Concept>(*in.parentConcept);
    for (auto& t : in.parentConcept->params)
      t = t->replace(from, to);
  }
}

nullable<SType> Type::instantiate(CodeLoc codeLoc, vector<SType> templateParams) const {
  if (templateParams.empty())
    return get_this().get();
  else
    return nullptr;
}

const Context& Type::getContext() const {
  return context;
}

const Context& Type::getStaticContext() const {
  return staticContext;
}

void Type::handleSwitchStatement(SwitchStatement&, Context&, CodeLoc codeLoc) const {
  codeLoc.error("Can't switch on value of type " + quote(getName()));
}

void checkConcepts(CodeLoc codeLoc, const vector<SType>& params, const vector<SType>& args) {
  for (int i = 0; i < params.size(); ++i) {
    Context tmp;
    tmp.deepCopyFrom(params[i]->context);
    for (int j = 0; j < params.size(); ++j)
      tmp.replace(params[j], args[j]);
    auto missingFunctions = args[i]->context.getMissingFunctions(tmp);
    if (!missingFunctions.empty())
      codeLoc.error("Function not implemented by type: " + quote(args[i]->getName()) + ": " +
          missingFunctions[0].second.toString(missingFunctions[0].first) + ", required by concept: " +
          quote(missingFunctions[0].second.parentConcept->getName()));
  }
}

nullable<SType> StructType::instantiate(CodeLoc codeLoc, vector<SType> templateArgs) const {
  if (templateArgs.size() != templateParams.size())
    return nullptr;
  auto ret = get_this().get();
  checkConcepts(codeLoc, templateParams, templateArgs);
  for (int i = 0; i < templateParams.size(); ++i)
    ret = ret->replace(templateParams[i], templateArgs[i]);
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
  }
  checkConcepts(codeLoc, type.templateParams, templateArgs);
  for (int i = 0; i < type.templateParams.size(); ++i) {
    replaceInFunction(type, type.templateParams[i], templateArgs[i]);
    type.templateParams[i] = templateArgs[i];
  }
}

EnumType::EnumType(string n, vector<string> e) : name(n), elements(e) {}

Concept::Concept(const string& name) : name(name) {
}

string Concept::getName() const {
  return name + joinTemplateParams(params);
}

string joinTemplateParams(const vector<SType>& params) {
  if (params.empty())
    return "";
  else
    return "<" + combine(transform(params, [](const auto& arg) { return arg->getName(); } ), ", ") + ">";
}
