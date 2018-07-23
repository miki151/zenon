#include "type.h"
#include "context.h"
#include "ast.h"

ArithmeticType::DefType ArithmeticType::INT = shared<ArithmeticType>("int");
ArithmeticType::DefType ArithmeticType::DOUBLE = shared<ArithmeticType>("double");
ArithmeticType::DefType ArithmeticType::VOID = shared<ArithmeticType>("void");
ArithmeticType::DefType ArithmeticType::BOOL = shared<ArithmeticType>("bool");
ArithmeticType::DefType ArithmeticType::STRING = shared<ArithmeticType>("string");
ArithmeticType::DefType ArithmeticType::CHAR = shared<ArithmeticType>("char");

string ArithmeticType::getName(bool withTemplateArguments) const {
  return name;
}

ArithmeticType::ArithmeticType(const string& name) : name(name) {
}

string ReferenceType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + " const&";
}

string MutableReferenceType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "&";
}

string PointerType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "*";
}

string PointerType::getCodegenName() const {
  return underlying->getCodegenName() + " const*";
}

string MutablePointerType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + " mutable*";
}

string MutablePointerType::getCodegenName() const {
  return underlying->getCodegenName() + "*";
}

string StructType::getName(bool withTemplateArguments) const {
  return name + (withTemplateArguments ? joinTemplateParams(templateParams) : "");
}

string StructType::getCodegenName() const {
  return name + joinTemplateParamsCodegen(templateParams);
}

string TemplateParameterType::getName(bool withTemplateArguments) const {
  return name;
}

string EnumType::getName(bool withTemplateArguments) const {
  return name;
}

void EnumType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc, bool isReference) const {
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

FunctionType::FunctionType(FunctionId name, FunctionCallType t, SType returnType, vector<Param> p, vector<SType> tpl)
  : name(name), callType(t), retVal(std::move(returnType)), params(std::move(p)), templateParams(tpl) {
}

string FunctionType::toString() const {
  string myName = name.visit(
      [&](const string& s) { return s; },
      [&](Operator op) { return "operator "s + getString(op); },
      [&](SType type) { return type->getName(false); });
  return retVal->getName() + " " + myName + joinTemplateParams(templateParams) + "(" +
      combine(transform(params, [](const Param& t) { return t.type->getName(); }), ", ") + ")" +
      (fromConcept ? " [from concept]" : "");
}

string Type::getCodegenName() const {
  return getName();
}

SType Type::getUnderlying() const {
  return get_this().get();
}

SType ReferenceType::getUnderlying() const {
  return underlying;
}

SType MutableReferenceType::getUnderlying() const {
  return underlying;
}

shared_ptr<ReferenceType> ReferenceType::get(SType type) {
  static map<SType, shared_ptr<ReferenceType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<ReferenceType>(type);
    generated.insert({type, ret});
    CHECK(!ret->context.addFunction(
        FunctionType(Operator::GET_ADDRESS, FunctionCallType::FUNCTION, PointerType::get(type), {}, {})));
  }
  return generated.at(type);
}

ReferenceType::ReferenceType(SType t) : underlying(t->getUnderlying()) {
  context.merge(underlying->getContext());
}

shared_ptr<MutableReferenceType> MutableReferenceType::get(SType type) {
  static map<SType, shared_ptr<MutableReferenceType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<MutableReferenceType>(type);
    generated.insert({type, ret});
    CHECK(!ret->context.addFunction(
        FunctionType(Operator::GET_ADDRESS, FunctionCallType::FUNCTION, MutablePointerType::get(type), {}, {})));
    CHECK(!ret->context.addFunction(
        FunctionType(Operator::ASSIGNMENT, FunctionCallType::FUNCTION, ret, {{type}}, {})));
  }
  return generated.at(type);
}

MutableReferenceType::MutableReferenceType(SType t) : underlying(t) {
  context.merge(underlying->getContext());
}

shared_ptr<PointerType> PointerType::get(SType type) {
  static map<SType, shared_ptr<PointerType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<PointerType>(type);
    generated.insert({type, ret});
    CHECK(!ret->context.addFunction(
        FunctionType(Operator::POINTER_DEREFERENCE, FunctionCallType::FUNCTION, ReferenceType::get(type), {}, {})));
  }
  return generated.at(type);
}

PointerType::PointerType(SType t) : underlying(t->getUnderlying()) {
}

shared_ptr<MutablePointerType> MutablePointerType::get(SType type) {
  static map<SType, shared_ptr<MutablePointerType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<MutablePointerType>(type);
    generated.insert({type, ret});
    CHECK(!ret->context.addFunction(
        FunctionType(Operator::POINTER_DEREFERENCE, FunctionCallType::FUNCTION, MutableReferenceType::get(type), {}, {})));
  }
  return generated.at(type);
}

MutablePointerType::MutablePointerType(SType t) : underlying(t->getUnderlying()) {
}

bool Type::canAssign(SType from) const{
  return false;
}

bool Type::isBuiltinCopyable(const Context&) const {
  return false;
}

bool PointerType::isBuiltinCopyable(const Context&) const {
  return true;
}

bool EnumType::isBuiltinCopyable(const Context&) const {
  return true;
}

bool ReferenceType::canAssign(SType from) const {
  return false;
}

bool MutableReferenceType::canAssign(SType from) const {
  return underlying == from->getUnderlying();
}

static bool checkMembers(set<SType> &visited, const SType& t, int maxDepth) {
  if (auto s = t.dynamicCast<StructType>()) {
    if (visited.count(t))
      return true;
    visited.insert(t);
    for (auto& member : s->context.getBottomLevelVariables()) {
      auto type = s->context.getTypeOfVariable(member).get_value()->getUnderlying();
      if (checkMembers(visited, type, maxDepth - 1))
        return true;
    }
  }
  return false;
}


bool StructType::hasInfiniteSize() const {
  set<SType> visited;
  return checkMembers(visited, get_this().get(), 500);
}

shared_ptr<StructType> StructType::get(Kind kind, string name) {
  auto ret = shared<StructType>();
  ret->kind = kind;
  ret->name = name;
  ret->parent = ret;
  return ret;
}

void ReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc, bool isReference) const {
  underlying->handleSwitchStatement(statement, context, codeLoc, false);
}

void MutableReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc, bool isReference) const {
  underlying->handleSwitchStatement(statement, context, codeLoc, true);
}

void StructType::handleSwitchStatement(SwitchStatement& statement, Context& outsideContext, CodeLoc codeLoc, bool isReference) const {
  codeLoc.check(kind == StructType::VARIANT, "Expected a variant or enum type");
  statement.type = SwitchStatement::VARIANT;
  statement.subtypesPrefix = name;
  if (!templateParams.empty()) {
    statement.subtypesPrefix += "<";
    for (auto& t : templateParams)
      statement.subtypesPrefix += t->getCodegenName() + ",";
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
    if (realType != ArithmeticType::VOID)
      caseElem.varType = caseElem.VALUE;
    if (caseElem.type) {
      auto t = outsideContext.getTypeFromString(*caseElem.type).get(caseElem.type->codeLoc);
      caseElem.type->codeLoc.check(t == realType || t == MutablePointerType::get(realType)
           || t == PointerType::get(realType),
          "Can't handle variant element "
          + quote(caseElem.id) + " of type " + quote(realType->getName()) + " as type " + quote(t->getName()));
      if (t == MutablePointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        caseElem.type->codeLoc.check(isReference,
            "Can't bind element to mutable pointer when switching on a non-reference variant");
        caseElem.type->codeLoc.check(realType != ArithmeticType::VOID, "Can't bind void element to pointer");
        caseBodyContext.addVariable(caseElem.id, MutablePointerType::get(realType));
      } else
      if (t == PointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        caseElem.type->codeLoc.check(realType != ArithmeticType::VOID, "Can't bind void element to pointer");
        caseBodyContext.addVariable(caseElem.id, PointerType::get(realType));
      }
    }
    if (caseElem.varType == caseElem.VALUE)
      caseBodyContext.addVariable(caseElem.id, ReferenceType::get(realType));
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
  for (auto type1 : copyOf(instantations)) {
    auto type = type1.dynamicCast<StructType>();
    type->context.deepCopyFrom(context);
    type->staticContext.deepCopyFrom(staticContext);
    type->alternatives = alternatives;
    for (int i = 0; i < templateParams.size(); ++i) {
      type->context.replace(templateParams[i], type->templateParams[i]);
      type->staticContext.replace(templateParams[i], type->templateParams[i]);
      for (auto& alternative : type->alternatives)
        alternative.type = alternative.type->replace(templateParams[i], type->templateParams[i]);
    }
  }
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

SType MutableReferenceType::replace(SType from, SType to) const {
  return MutableReferenceType::get(underlying->replace(from, to));
}

SType PointerType::replace(SType from, SType to) const {
  return PointerType::get(underlying->replace(from, to));
}

SType MutablePointerType::replace(SType from, SType to) const {
  return MutablePointerType::get(underlying->replace(from, to));
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
      auto memberType = *ret->context.getTypeOfVariable(member);
      checkNonVoidMember(memberType, to);
    }
    ret->alternatives = alternatives;
    for (auto& alternative : ret->alternatives) {
      checkNonVoidMember(alternative.type, to);
      alternative.type = alternative.type->replace(from, to);
    }
    ret->requirements = requirements;
    for (auto& concept : ret->requirements)
      concept = concept->replace(from, to);
    ret->context.replace(from, to);
    ret->staticContext.replace(from, to);
  } else
    INFO << "Found instantiated: " << ret->getName();
  return ret;
}

void replaceInFunction(FunctionType& in, SType from, SType to) {
  in.retVal = in.retVal->replace(from, to);
  if (in.parentType)
    in.parentType = in.parentType->replace(from, to);
  for (auto& param : in.params)
    param.type = param.type->replace(from, to);
  //for (auto& param : in.templateParams)
  //  param = param->replace(from, to);
  for (auto& concept : in.requirements)
    concept = concept->replace(from, to);
  in.name.visit(
      [&](SType& type) { type = type->replace(from, to); },
      [&](const auto&) {}
  );
}

WithError<SType> Type::instantiate(const Context& context, vector<SType> templateArgs) const {
  if (templateArgs.empty())
    return get_this().get();
  else
    return "Type " + quote(getName()) + " is not a template";
}

Context& Type::getContext() {
  return context;
}

Context& Type::getStaticContext() {
  return staticContext;
}

void Type::handleSwitchStatement(SwitchStatement&, Context&, CodeLoc codeLoc, bool isReference) const {
  codeLoc.error("Can't switch on the value of type " + quote(getName()));
}

WithError<SType> StructType::instantiate(const Context& context, vector<SType> templateArgs) const {
  if (templateArgs.size() != templateParams.size())
    return "Wrong number of template parameters for type " + getName();
  auto ret = get_this().get();
  for (int i = 0; i < templateParams.size(); ++i)
    ret = ret->replace(templateParams[i], templateArgs[i]);
  for (auto& concept : ret.dynamicCast<StructType>()->requirements) {
    auto missing = context.getMissingFunctions(concept->getContext(), {});
    if (!missing.empty())
      return "Required function not implemented: " +
          missing[0].toString() + ", required by concept: " + quote(concept->getName());
  }
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

static string getCantBindError(const SType& from, const SType& to) {
  return "Can't bind type " + quote(from->getName()) + " to parameter of type " + quote(to->getName());
}

static optional<string> getDeductionError(const Context& context, TypeMapping& mapping, SType paramType, SType argType) {
  if ((!paramType.dynamicCast<ReferenceType>() && !paramType.dynamicCast<MutableReferenceType>()) &&
      (argType.dynamicCast<ReferenceType>() || argType.dynamicCast<MutableReferenceType>())) {
    if (context.canCopyConstruct(argType->getUnderlying()))
      argType = argType->getUnderlying();
    else
      return "Type " + quote(argType->getUnderlying()->getName()) + " cannot be copied.";
  }
  if (auto index = mapping.getParamIndex(paramType)) {
    auto& arg = mapping.templateArgs.at(*index);
    if (arg && arg != argType)
      return getCantBindError(argType, arg.get());
    arg = argType;
    return none;
  } else
    return paramType->getMappingError(context, mapping, argType);
}

optional<string> StructType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  auto argStruct = argType.dynamicCast<StructType>();
  if (!argStruct || parent.get() != argStruct->parent.get())
    return "Can't bind non-struct type " + quote(argType->getName()) + " to struct type " + quote(getName());
  for (int i = 0; i < templateParams.size(); ++i)
    if (auto error = ::getDeductionError(context, mapping, templateParams[i], argStruct->templateParams[i]))
      return error;
  return none;
}

optional<string> PointerType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<PointerType>())
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  return "Can't bind type " + quote(argType->getName()) + " to type " + quote(getName());
}

optional<string> MutablePointerType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<MutablePointerType>())
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  return "Can't bind type " + quote(argType->getName()) + " to type " + quote(getName());
}

bool MutablePointerType::isBuiltinCopyable(const Context&) const {
  return true;
}

optional<string> ReferenceType::getMappingError(const Context&, TypeMapping& mapping, SType from) const {
  return ::getDeductionError(context, mapping, underlying, from->getUnderlying());
}

optional<string> MutableReferenceType::getMappingError(const Context&, TypeMapping& mapping, SType from) const {
  if (auto argRef = from.dynamicCast<MutableReferenceType>())
    return ::getDeductionError(context, mapping, underlying, argRef->underlying);
  else
    return getCantBindError(from, get_this().get());
}

optional<string> Type::getMappingError(const Context&, TypeMapping&, SType argType) const {
  if (argType == get_this().get())
    return none;
  else
    return getCantBindError(argType, get_this().get());
}

bool isNameAndArgsEqual(const FunctionType& f1, const FunctionType& f2) {
  if (f1.name != f2.name || f1.params.size() != f2.params.size())
    return false;
  for (int i = 0; i < f1.params.size(); ++i)
    if (f1.params[i].type != f2.params[i].type)
      return false;
  return true;
}

WithErrorLine<FunctionType> instantiateFunction(const Context& context, const FunctionType& input, CodeLoc codeLoc,
    vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc, vector<FunctionType> existing) {
  FunctionType type = input;
  vector<SType> funParams = transform(type.params, [](const FunctionType::Param& p) { return p.type; });
  if (templateArgs.size() > type.templateParams.size())
    return codeLoc.getError("Too many template arguments.");
  TypeMapping mapping { type.templateParams, vector<nullable<SType>>(type.templateParams.size()) };
  for (int i = 0; i < templateArgs.size(); ++i)
    mapping.templateArgs[i] = templateArgs[i];
  if (funParams.size() != argTypes.size())
    return codeLoc.getError("Wrong number of function arguments.");
  for (int i = 0; i < argTypes.size(); ++i) {
    optional<ErrorLoc> firstError;
    for (auto tArg : context.getConversions(argTypes[i])) {
      if (auto error = getDeductionError(context, mapping, funParams[i], tArg)) {
        if (!firstError)
          firstError = argLoc[i].getError(*error);
      } else {
        firstError = none;
        break;
      }
    }
    if (firstError)
      return *firstError;
  }
  for (int i = 0; i < type.templateParams.size(); ++i) {
    if (i >= templateArgs.size()) {
      if (auto deduced = mapping.templateArgs[i])
        templateArgs.push_back(deduced.get());
      else
        return codeLoc.getError("Couldn't deduce template argument " + quote(type.templateParams[i]->getName()));
    }
  }
  for (int i = 0; i < type.templateParams.size(); ++i) {
    replaceInFunction(type, type.templateParams[i], templateArgs[i]);
    type.templateParams[i] = templateArgs[i];
  }
  for (auto& fun : existing)
    if (isNameAndArgsEqual(input, fun))
      // To avoid infinite recursion we don't check concept requirements twice for the same >>original<< function
      // (not instantation). If this causes issues then it needs to be revised.
      return type;
  existing.push_back(input);
  //cout << "Instantiating " << type.toString() << " " << existing.size() << endl;
  for (auto& concept : type.requirements) {
    auto missing = context.getMissingFunctions(concept->getContext(), existing);
    if (!missing.empty()) {
      return codeLoc.getError("Required function not implemented: " +
          missing[0].toString() + ", required by concept: " + quote(concept->getName()));
    }
  }
  return type;
}

EnumType::EnumType(string n, vector<string> e) : name(n), elements(e) {}

Concept::Concept(const string& name) : name(name) {
}

string Concept::getName() const {
  return name + joinTemplateParams(params);
}

SConcept Concept::translate(vector<SType> newParams) const {
  auto ret = shared<Concept>(name);
  ret->context.deepCopyFrom(context);
  ret->params = newParams;
  CHECK(params.size() == newParams.size());
  for (int i = 0; i < params.size(); ++i)
    ret->context.replace(params[i], newParams[i]);
  return ret;
}

SConcept Concept::replace(SType from, SType to) const {
  auto ret = shared<Concept>(name);
  ret->context.deepCopyFrom(context);
  ret->params = params;
  for (auto& param : ret->params)
    param = param->replace(from, to);
  ret->context.replace(from, to);
  return ret;
}

const vector<SType>&Concept::getParams() const {
  return params;
}

const Context&Concept::getContext() const {
  return context;
}

vector<SType>& Concept::modParams() {
  return params;
}

Context& Concept::modContext() {
  return context;
}

string joinTypeList(const vector<SType>& types) {
  return combine(transform(types, [](const auto& type) { return type->getName(); }), ", ");
}

string joinTemplateParams(const vector<SType>& params) {
  if (params.empty())
    return "";
  else
    return "<" + joinTypeList(params) + ">";
}

string joinTypeListCodegen(const vector<SType>& types) {
  return combine(transform(types, [](const auto& type) { return type->getCodegenName(); }), ", ");
}

string joinTemplateParamsCodegen(const vector<SType>& params) {
  if (params.empty())
    return "";
  else
    return "<" + joinTypeListCodegen(params) + ">";
}

FunctionType::Param::Param(optional<string> name, SType type) : name(name), type(type) {
}

FunctionType::Param::Param(string name, SType type) : name(name), type(type) {
}

FunctionType::Param::Param(SType type) : type(type) {
}

string ArrayType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "[" + to_string(size) + "]";
}

string ArrayType::getCodegenName() const {
  return "std::array<" + underlying->getCodegenName() + "," + to_string(size) + ">";
}

SType ArrayType::replace(SType from, SType to) const {
  return get(underlying->replace(from, to), size);
}

shared_ptr<ArrayType> ArrayType::get(SType type, int size) {
  static map<pair<SType, int>, shared_ptr<ArrayType>> generated;
  if (!generated.count({type, size})) {
    auto ret = shared<ArrayType>(type, size);
    generated.insert({{type, size}, ret});
  }
  return generated.at({type, size});
}

bool ArrayType::isBuiltinCopyable(const Context& context) const {
  return context.canCopyConstruct(underlying);
}

ArrayType::ArrayType(SType type, int size) : size(size), underlying(type) {
}

optional<string> ArrayType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<ArrayType>())
    if (size == argPointer->size)
      return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  return "Can't bind type " + quote(argType->getName()) + " to type " + quote(getName());
}
