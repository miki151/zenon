#include "type.h"
#include "context.h"
#include "ast.h"

ArithmeticType::DefType ArithmeticType::INT = shared<ArithmeticType>("int");
ArithmeticType::DefType ArithmeticType::DOUBLE = shared<ArithmeticType>("double");
ArithmeticType::DefType ArithmeticType::VOID = shared<ArithmeticType>("void");
ArithmeticType::DefType ArithmeticType::BOOL = shared<ArithmeticType>("bool");
ArithmeticType::DefType ArithmeticType::STRING = shared<ArithmeticType>("string", "zenon_string"s);
ArithmeticType::DefType ArithmeticType::CHAR = shared<ArithmeticType>("char");
ArithmeticType::DefType ArithmeticType::ANY_TYPE = shared<ArithmeticType>("any_type");
ArithmeticType::DefType ArithmeticType::ENUM_TYPE = shared<ArithmeticType>("enum_type");

string ArithmeticType::getName(bool withTemplateArguments) const {
  return name;
}

string ArithmeticType::getCodegenName() const {
  return codegenName;
}

ArithmeticType::ArithmeticType(const string& name, optional<std::string> codegenName)
    : name(name), codegenName(codegenName.value_or(name)) {
}

string ReferenceType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + " const&";
}

optional<string> ReferenceType::getMangledName() const {
  if (auto u = underlying->getMangledName())
    return "R" + *u;
  else {
    return none;
  }
}

string ReferenceType::getCodegenName() const {
  return underlying->getCodegenName() + " const&";
}

string MutableReferenceType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "&";
}

optional<string> MutableReferenceType::getMangledName() const {
  if (auto u = underlying->getMangledName())
    return "MR" + *u;
  else {
    return none;
  }
}

string MutableReferenceType::getCodegenName() const {
  return underlying->getCodegenName() + "&";
}

string PointerType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "*";
}

string PointerType::getCodegenName() const {
  return underlying->getCodegenName() + " const*";
}

optional<string> PointerType::getMangledName() const {
  if (auto u = underlying->getMangledName())
    return "P" + *u;
  else {
    return none;
  }
}

string MutablePointerType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + " mutable*";
}

string MutablePointerType::getCodegenName() const {
  return underlying->getCodegenName() + "*";
}

optional<string> MutablePointerType::getMangledName() const {
  if (auto u = underlying->getMangledName())
    return "MP" + *u;
  else {
    return none;
  }
}

string StructType::getName(bool withTemplateArguments) const {
  return name + (withTemplateArguments ? joinTemplateParams(templateParams) : "");
}

string StructType::getCodegenName() const {
  if (external)
    return name + joinTemplateParamsCodegen(templateParams);
  else
    return *getMangledName();
}

optional<string> StructType::getMangledName() const {
  string suf;
  for (auto& param : templateParams)
    if (auto name = param->getMangledName())
      suf += *name;
    else
      return none;
  return getName(false) + suf;
}

string TemplateParameterType::getName(bool withTemplateArguments) const {
  return name;
}

bool TemplateParameterType::canReplaceBy(SType t) const {
  return type == ArithmeticType::ANY_TYPE || t->getType() == type;
}

optional<string> TemplateParameterType::getMangledName() const {
  return none;
}

SType TemplateParameterType::getType() const {
  return type;
}

bool TemplateParameterType::isBuiltinCopyable(const Context&) const {
  if (type == ArithmeticType::ENUM_TYPE)
    return true;
  else
    return false;
}

TemplateParameterType::TemplateParameterType(string n, CodeLoc l) : name(n), declarationLoc(l), type(ArithmeticType::ANY_TYPE) {}

TemplateParameterType::TemplateParameterType(SType type, string name, CodeLoc l)
    : name(std::move(name)), declarationLoc(l), type(std::move(type)) {
}

string EnumType::getName(bool withTemplateArguments) const {
  return name;
}

void EnumType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc, SwitchArgument) const {
  statement.type = SwitchStatement::ENUM;
  statement.targetType = (SType)get_this();
  unordered_set<string> handledElems;
  for (auto& caseElem : statement.caseElems) {
    caseElem.codeloc.check(caseElem.type.contains<none_t>(), "Expected enum element");
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

FunctionType::FunctionType(SType returnType, vector<Param> p, vector<SType> tpl)
  : retVal(std::move(returnType)), params(std::move(p)), templateParams(tpl) {
}

FunctionType FunctionType::setBuiltin() {
  builtinOperator = true;
  return *this;
}

SFunctionInfo FunctionInfo::getDefined(FunctionId id, FunctionType type, FunctionDefinition* definition) {
  auto args = make_tuple(id, type, definition);
  static map<decltype(args), SFunctionInfo> generated;
  if (!generated.count(args)) {
    generated.insert(make_pair(args, shared<FunctionInfo>(Private{}, id, type, definition)));
  }
  return generated.at(args);
}

SFunctionInfo FunctionInfo::getImplicit(FunctionId id, FunctionType type) {
  auto args = make_tuple(id, type);
  static map<decltype(args), SFunctionInfo> generated;
  if (!generated.count(args)) {
    generated.insert(make_pair(args, shared<FunctionInfo>(Private{}, id, type, nullptr)));
  }
  return generated.at(args);
}

SFunctionInfo FunctionInfo::getInstance(FunctionId id, FunctionType type, SFunctionInfo parent) {
  if (id == parent->id && type == parent->type)
    return parent;
  while (!!parent->parent)
    parent = parent->parent.get();
  auto args = make_tuple(id, type, parent);
  static map<decltype(args), SFunctionInfo> generated;
  if (!generated.count(args)) {
    auto ret = shared<FunctionInfo>(Private{}, id, type, parent);
    parent->instantiations.push_back(ret);
    generated.insert(make_pair(args, std::move(ret)));
  }
  return generated.at(args);
}

string FunctionInfo::prettyString() const {
  return type.retVal->getName() + " " + toString(id) + joinTemplateParams(type.templateParams) + "(" +
      combine(transform(type.params, [](auto& t) { return t.type->getName() + (t.name ? " " + *t.name : ""s); }), ", ") + ")" +
      (type.fromConcept ? " [from concept]" : "");
}

optional<string> FunctionInfo::getMangledName() const {
  string suf;
  for (auto& arg : type.templateParams)
    if (auto name = arg->getMangledName())
      suf += *name;
    else
      return none;
  return suf;
}

SFunctionInfo FunctionInfo::getParent() const {
  if (parent)
    return parent.get();
  else
    return get_this().get();
}

FunctionInfo::FunctionInfo(FunctionInfo::Private, FunctionId id, FunctionType type, nullable<SFunctionInfo> parent)
  : id(std::move(id)), type(std::move(type)), parent(std::move(parent)) {}

FunctionInfo::FunctionInfo(FunctionInfo::Private, FunctionId id, FunctionType type, FunctionDefinition* definition)
  : id(std::move(id)), type(std::move(type)), definition(definition) {}

string Type::getCodegenName() const {
  return getName();
}

optional<string> Type::getMangledName() const {
  return getCodegenName();
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
  }
  return generated.at(type);
}

ReferenceType::ReferenceType(SType t) : underlying(t->getUnderlying()) {
}

shared_ptr<MutableReferenceType> MutableReferenceType::get(SType type) {
  static map<SType, shared_ptr<MutableReferenceType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<MutableReferenceType>(type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

MutableReferenceType::MutableReferenceType(SType t) : underlying(t) {
}

shared_ptr<PointerType> PointerType::get(SType type) {
  static map<SType, shared_ptr<PointerType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<PointerType>(type);
    generated.insert({type, ret});
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

bool ArithmeticType::isBuiltinCopyable(const Context&) const {
  return true;
}

SType Type::removePointer() const {
  return get_this().get();
}

optional<string> Type::getSizeError() const {
  return none;
}

bool PointerType::isBuiltinCopyable(const Context&) const {
  return true;
}

SType PointerType::removePointer() const {
  return underlying;
}

bool EnumType::isBuiltinCopyable(const Context&) const {
  return true;
}

SType EnumType::getType() const {
  return ArithmeticType::ENUM_TYPE;
}

bool ReferenceType::canAssign(SType from) const {
  return false;
}

bool MutableReferenceType::canAssign(SType from) const {
  return underlying == from->getUnderlying();
}

static optional<string> checkMembers(set<SType> &visited, const SType& t) {
  if (auto s = t.dynamicCast<StructType>()) {
    if (visited.count(t))
      return "has infinite size"s;
    if (s->incomplete)
      return "has an incomplete type"s;
    visited.insert(t);
    for (auto& member : s->members)
      if (auto res = checkMembers(visited, member.type))
        return res;
    for (auto& member : s->alternatives)
      if (auto res = checkMembers(visited, member.type))
        return res;
  }
  return none;
}

optional<string> StructType::getSizeError() const {
  set<SType> visited;
  return checkMembers(visited, get_this().get());
}

shared_ptr<StructType> StructType::get(string name) {
  auto ret = shared<StructType>(Private{});
  ret->name = name;
  ret->parent = ret;
  return ret;
}

void ReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc, SwitchArgument) const {
  underlying->handleSwitchStatement(statement, context, codeLoc, SwitchArgument::REFERENCE);
}

SType ReferenceType::removePointer() const {
  return underlying->removePointer();
}

void MutableReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, CodeLoc codeLoc, SwitchArgument) const {
  underlying->handleSwitchStatement(statement, context, codeLoc, SwitchArgument::MUTABLE_REFERENCE);
}

SType MutableReferenceType::removePointer() const {
  return underlying->removePointer();
}

void StructType::handleSwitchStatement(SwitchStatement& statement, Context& outsideContext, CodeLoc codeLoc,
    SwitchArgument argumentType) const {
  codeLoc.check(!alternatives.empty(), "Expected a variant or enum type");
  statement.type = SwitchStatement::VARIANT;
  statement.targetType = (SType)get_this();
  unordered_set<string> handledTypes;
  auto getAlternativeType = [&] (const string& name) -> nullable<SType> {
    for (auto& alternative : alternatives)
      if (alternative.name == name)
        return alternative.type;
    return nullptr;
  };
  vector<Context::MovedVarsSnapshot> allMovedVars;
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
    if (auto caseType = caseElem.getType(outsideContext)) {
      caseElem.codeloc.check(caseType == realType || caseType->removePointer() == realType,
          "Can't handle variant element " + quote(caseElem.id) + " of type " + quote(realType->getName()) +
          " as type " + quote(caseType->getName()));
      if (caseType == MutablePointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        caseElem.codeloc.check(argumentType == SwitchArgument::MUTABLE_REFERENCE,
            "Can only bind element to mutable pointer when switch argument is a mutable reference");
        caseElem.codeloc.check(realType != ArithmeticType::VOID, "Can't bind void element to pointer");
        caseBodyContext.addVariable(caseElem.id, MutableReferenceType::get(MutablePointerType::get(realType)));
      } else
      if (caseType == PointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        caseElem.codeloc.check(argumentType != SwitchArgument::VALUE,
            "Can only bind element to pointer when switch argument is a reference");
        caseElem.codeloc.check(realType != ArithmeticType::VOID, "Can't bind void element to pointer");
        caseBodyContext.addVariable(caseElem.id, MutableReferenceType::get(PointerType::get(realType)));
      }
    }
    if (caseElem.varType == caseElem.VALUE) {
      caseElem.codeloc.check(argumentType == SwitchArgument::VALUE || realType->isBuiltinCopyable(outsideContext),
          "Type " + quote(realType->getName()) + " is not implicitly copyable. "
          "Try binding to a pointer or move from the variable that you are switching on");
      caseBodyContext.addVariable(caseElem.id, ReferenceType::get(realType));
    }
    auto movedBeforeTrueSegment = outsideContext.getMovedVarsSnapshot();
    caseElem.block->check(caseBodyContext);
    allMovedVars.push_back(outsideContext.getMovedVarsSnapshot());
    outsideContext.setMovedVars(std::move(movedBeforeTrueSegment));
  }
  for (auto& elem : allMovedVars)
    outsideContext.mergeMovedVars(elem);
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

WithError<SType> StructType::getTypeOfMember(const string& name) const {
  for (auto& member : members)
    if (member.name == name)
      return member.type;
  return "No member named " + quote(name) + " in struct " + quote(getName());
}

shared_ptr<StructType> StructType::getInstance(vector<SType> newTemplateParams) {
  auto self = get_this().get().dynamicCast<StructType>();
  if (templateParams == newTemplateParams)
    return self;
  for (auto type : instances) {
    if (type->templateParams == newTemplateParams)
      return type;
  }
  auto type = StructType::get(name);
  type->alternatives = alternatives;
  type->members = members;
  type->parent = self;
  type->external = external;
  instances.push_back(type);
  return type;
}

void StructType::updateInstantations() {
  for (auto type1 : copyOf(instances)) {
    auto type = type1.dynamicCast<StructType>();
    type->staticContext.deepCopyFrom(staticContext);
    type->alternatives = alternatives;
    type->members = members;
    for (int i = 0; i < templateParams.size(); ++i) {
      type->staticContext.replace(templateParams[i], type->templateParams[i]);
      for (auto& alternative : type->alternatives)
        alternative.type = alternative.type->replace(templateParams[i], type->templateParams[i]);
      for (auto& member : type->members)
        member.type = member.type->replace(templateParams[i], type->templateParams[i]);
    }
  }
}

SType Type::replace(SType from, SType to) const {
  auto self = get_this().get();
  if (from == self) {
    CHECK(canReplaceBy(to));
    return to;
  } else
    return replaceImpl(from, to);
}

bool Type::canReplaceBy(SType) const {
  return false;
}

SType Type::replaceImpl(SType, SType) const {
  return get_this().get();
}

SType Type::getType() const {
  return ArithmeticType::ANY_TYPE;
}

SType ReferenceType::replaceImpl(SType from, SType to) const {
  return ReferenceType::get(underlying->replace(from, to));
}

SType MutableReferenceType::replaceImpl(SType from, SType to) const {
  return MutableReferenceType::get(underlying->replace(from, to));
}

SType PointerType::replaceImpl(SType from, SType to) const {
  return PointerType::get(underlying->replace(from, to));
}

SType MutablePointerType::replaceImpl(SType from, SType to) const {
  return MutablePointerType::get(underlying->replace(from, to));
}

static void checkNonVoidMember (const SType& type, const SType& to) {
  if (auto param = type.dynamicCast<TemplateParameterType>())
    param->declarationLoc.check(to != ArithmeticType::VOID,
        "Can't instantiate member type with type " + quote(ArithmeticType::VOID->getName()));
}

SType StructType::replaceImpl(SType from, SType to) const {
  vector<SType> newTemplateParams;
  for (auto& param : templateParams)
    newTemplateParams.push_back(param->replace(from, to));
  auto ret = parent->getInstance(newTemplateParams);
  // This is how we check if instantiate gave us a new type to fill
  if (ret->templateParams != newTemplateParams) {
    ret->templateParams = newTemplateParams;
    INFO << "New instantiation: " << ret->getName();
    ret->staticContext.deepCopyFrom(staticContext);
    ret->alternatives = alternatives;
    for (auto& alternative : ret->alternatives) {
      checkNonVoidMember(alternative.type, to);
      alternative.type = alternative.type->replace(from, to);
    }
    ret->members = members;
    for (auto& members : ret->members) {
      checkNonVoidMember(members.type, to);
      members.type = members.type->replace(from, to);
    }
    ret->requirements = requirements;
    for (auto& concept : ret->requirements)
      concept = concept->replace(from, to);
    ret->staticContext.replace(from, to);
  } else
    INFO << "Found instantiated: " << ret->getName();
  return ret;
}

FunctionType replaceInFunction(FunctionType type, SType from, SType to) {
  if (type.parentType)
    type.parentType = type.parentType->replace(from, to);
  type.retVal = type.retVal->replace(from, to);
  for (auto& param : type.params)
    param.type = param.type->replace(from, to);
  for (auto& param : type.templateParams)
    param = param->replace(from, to);
  for (auto& concept : type.requirements)
    concept = concept->replace(from, to);
  return type;
}

SFunctionInfo replaceInFunction(const SFunctionInfo& fun, SType from, SType to) {
  return FunctionInfo::getInstance(fun->id, replaceInFunction(fun->type, from, to), fun);
}

WithError<SType> Type::instantiate(const Context& context, vector<SType> templateArgs) const {
  if (templateArgs.empty())
    return get_this().get();
  else
    return "Type " + quote(getName()) + " is not a template";
}

Context& Type::getStaticContext() {
  return staticContext;
}

void Type::handleSwitchStatement(SwitchStatement&, Context&, CodeLoc codeLoc, SwitchArgument) const {
  codeLoc.error("Can't switch on the value of type " + quote(getName()));
}

static string getCantSubstituteError(SType param, SType with) {
  auto ret = "Can't substitute template parameter " + quote(param->getName()) + " ";
  if (param->getType() != ArithmeticType::ANY_TYPE)
    ret += "of type " + quote(param->getType()->getName()) + " ";
  ret += "with type " + quote(with->getName());
  return ret;
}

WithError<SType> StructType::instantiate(const Context& context, vector<SType> templateArgs) const {
  CHECK(parent == this) << "Struct instatiated a second time?";
  if (templateArgs.size() != templateParams.size())
    return "Wrong number of template parameters for type " + getName();
  auto ret = get_this().get().dynamicCast<StructType>();
  for (int i = 0; i < templateParams.size(); ++i) {
    if (!ret->templateParams[i]->canReplaceBy(templateArgs[i]))
      return getCantSubstituteError(templateParams[i], templateArgs[i]);
    ret = ret->replace(ret->templateParams[i], templateArgs[i]).dynamicCast<StructType>();
  }
  for (auto& concept : ret.dynamicCast<StructType>()->requirements)
    if (auto error = context.getMissingFunctions(*concept, {}))
      return *error;
  return (SType) ret;
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
    if (argType->getUnderlying()->isBuiltinCopyable(context))
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
    return "Can't bind type " + quote(argType->getName()) + " to struct type " + quote(getName());
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

SType MutablePointerType::removePointer() const {
  return underlying;
}

optional<string> ReferenceType::getMappingError(const Context& context, TypeMapping& mapping, SType from) const {
  return ::getDeductionError(context, mapping, underlying, from->getUnderlying());
}

optional<string> MutableReferenceType::getMappingError(const Context& context, TypeMapping& mapping, SType from) const {
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

static bool areParamsTypesEqual(const FunctionType& f1, const FunctionType& f2) {
  if (f1.params.size() != f2.params.size())
    return false;
  for (int i = 0; i < f1.params.size(); ++i)
    if (f1.params[i].type != f2.params[i].type)
      return false;
  return true;
}

WithErrorLine<SFunctionInfo> instantiateFunction(const Context& context, const SFunctionInfo& input, CodeLoc codeLoc,
    vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc, vector<FunctionType> existing) {
  FunctionType type = input->type;
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
    codeLoc.check(type.templateParams[i]->canReplaceBy(templateArgs[i]),
        getCantSubstituteError(type.templateParams[i], templateArgs[i]));
    type = replaceInFunction(type, type.templateParams[i], templateArgs[i]);
    type.templateParams[i] = templateArgs[i];
  }
  for (auto& fun : existing)
    if (areParamsTypesEqual(input->type, fun))
      // To avoid infinite recursion we don't check concept requirements twice for the same >>original<< function
      // (not instantation). If this causes issues then it needs to be revised.
      return FunctionInfo::getInstance(input->id, type, input);
  existing.push_back(input->type);
  //cout << "Instantiating " << type.toString() << " " << existing.size() << endl;
  for (auto& concept : type.requirements)
    if (auto error = context.getMissingFunctions(*concept, existing))
      return codeLoc.getError(*error);
  return FunctionInfo::getInstance(input->id, type, input);
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

const vector<SType>& Concept::getParams() const {
  return params;
}

const Context& Concept::getContext() const {
  return context;
}

vector<SType>& Concept::modParams() {
  return params;
}

Context& Concept::modContext() {
  return context;
}

/*static string to_string(void* ptr) {
  char buf[30];
  sprintf(buf, "%p", ptr);
  return buf;
}*/

string joinTypeList(const vector<SType>& types) {
  return combine(transform(types, [](const SType& type) { return type->getName(); }), ", ");
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
  return underlying->getName(withTemplateArguments) + "[" + size->getName() + "]";
}

string ArrayType::getCodegenName() const {
  return "std::array<" + underlying->getCodegenName() + "," + size->getCodegenName() + ">";
}

optional<string> ArrayType::getMangledName() const {
  if (auto u = underlying->getMangledName())
    if (auto sz = size->getMangledName())
      return "A" + *u + *sz;
  return none;
}

SType ArrayType::replaceImpl(SType from, SType to) const {
  return get(underlying->replace(from, to), size->replace(from, to).dynamicCast<CompileTimeValue>());
}

shared_ptr<ArrayType> ArrayType::get(SType type, SCompileTimeValue size) {
  static map<pair<SType, SType>, shared_ptr<ArrayType>> generated;
  if (!generated.count({type, size})) {
    auto ret = shared<ArrayType>(type, size);
    generated.insert({{type, size}, ret});
  }
  return generated.at({type, size});
}

bool ArrayType::isBuiltinCopyable(const Context& context) const {
  return false;//underlying->isBuiltinCopyable(context);
}

optional<string> ArrayType::getSizeError() const {
  return underlying->getSizeError();
}

ArrayType::ArrayType(SType type, SCompileTimeValue size) : size(std::move(size)), underlying(type) {
}

optional<string> ArrayType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<ArrayType>()) {
    if (auto error = ::getDeductionError(context, mapping, size, argPointer->size))
      return error;
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  }
  return "Can't bind type " + quote(argType->getName()) + " to type " + quote(getName());
}

SCompileTimeValue CompileTimeValue::get(Value value) {
  static map<Value, SCompileTimeValue> generated;
  if (!generated.count(value)) {
    auto ret = shared<CompileTimeValue>(value);
    generated.insert({value, ret});
  }
  return generated.at(value);
}

CompileTimeValue::CompileTimeValue(Value value) : value(std::move(value)) {
}

string CompileTimeValue::getName(bool withTemplateArguments) const {
  return value.visit(
      [](int v) { return to_string(v); },
      [](double v) { return to_string(v); },
      [](bool v) { return v ? "true" : "false"; },
      [](char v) { return "\'" + string(1, v) + "\'"; },
      [](const string& v) { return v; },
      [](const EnumValue& t) { return t.type->getName() + "::" + t.type->elements[t.index]; },
      [](const ArrayValue& t) { return "{" + combine(transform(t.values,
           [](const auto& v) { return v->getName();}), ", ") + "}"; },
      [](const TemplateValue& v) { return v.name; },
      [](const TemplateExpression& v) { return getPrettyString(v.op, v.args); },
      [](const TemplateFunctionCall& v) { return "[function call]"; }
  );
}

string CompileTimeValue::getCodegenName() const {
  return value.visit(
      [this](const auto&) { return getName(); },
      [](const string& v) { return "\"" + v +"\"_lstr"; },
      [](const ArrayValue& t) { return "make_array(" + combine(transform(t.values,
           [](const auto& v) { return v->getCodegenName();}), ", ") + ")"; },
      [](const TemplateValue&) -> string { fail(); },
      [](const TemplateExpression&) -> string { fail(); },
      [](const TemplateFunctionCall&) -> string { fail(); }
  );
}

SType CompileTimeValue::getType() const {
  return value.visit(
      [](int)-> SType {  return ArithmeticType::INT; },
      [](double)-> SType {  return ArithmeticType::DOUBLE; },
      [](bool)-> SType {  return ArithmeticType::BOOL; },
      [](char)-> SType {  return ArithmeticType::CHAR; },
      [](const string&)-> SType {  return ArithmeticType::STRING; },
      [](const EnumValue& v)-> SType {  return v.type; },
      [](const ArrayValue& v)-> SType {  return ArrayType::get(v.type, CompileTimeValue::get((int) v.values.size())); },
      [](const TemplateValue& v)-> SType {  return v.type; },
      [](const TemplateExpression& v)-> SType {  return v.type; },
      [](const TemplateFunctionCall& v)-> SType {  return v.retVal; }
  );
}

template <typename Num>
static string mangleNumber(Num v) {
  return v < 0 ? "M" + to_string(-v) : to_string(v);
}

optional<string> CompileTimeValue::getMangledName() const {
  return value.visit(
      [this](const auto&) -> optional<string> { return getName(); },
      [](int v) -> optional<string> { return mangleNumber(v); },
      [](double v) -> optional<string> { return mangleNumber(v); },
      [](const string& v) -> optional<string> { return "\"" + v +"\"_lstr"; },
      [](const ArrayValue& t) -> optional<string> {
          vector<string> names;
          for (auto& elem : t.values)
            if (auto name = elem->getMangledName())
              names.push_back(*name);
            else
              return none;
          return "Arr"s + combine(names, "");
      },
      [](const TemplateValue& v) -> optional<string> { return none; },
      [](const TemplateExpression&) -> optional<string> { return none; },
      [](const TemplateFunctionCall&) -> optional<string> { return none; }
  );
}

shared_ptr<CompileTimeValue> CompileTimeValue::getTemplateValue(SType type, string name) {
  return get(TemplateValue{std::move(type), std::move(name)});
}

optional<string> CompileTimeValue::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argValue = argType.dynamicCast<CompileTimeValue>()) {
    auto argType = argValue->getType();
    return getDeductionError(context, mapping, getType(), argType);
  } else
    return "Trying to bind type " + quote(argType->getName()) + " to a value template parameter";
}

bool CompileTimeValue::canReplaceBy(SType t) const {
  if (auto myValue = value.getReferenceMaybe<TemplateValue>())
    if (auto v = t.dynamicCast<CompileTimeValue>()) {
      return myValue->type == v->getType();
    }
  return false;
}

SType CompileTimeValue::replaceImpl(SType from, SType to) const {
  return value.visit(
      [&](const TemplateValue& value) {
        return CompileTimeValue::getTemplateValue(value.type->replace(from, to), value.name);
      },
      [&](const TemplateExpression& value) {
        return ::eval(value.op, transform(value.args,
            [&](const SType& t){ return t->replace(from, to); })).get();
      },
      [&](const TemplateFunctionCall& value) {
        return value.functionInfo.invokeFunction(value.name, value.loc, transform(value.args,
            [&](const SType& t){ return t->replace(from, to); }), value.argLoc).get();
      },
      [&](const auto&) { return get_this().get();}
  );
}

string SliceType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "[]";
}

string SliceType::getCodegenName() const {
  return "slice_t<" + underlying->getCodegenName() + ">";
}

optional<string> SliceType::getMangledName() const {
  if (auto u = underlying->getMangledName())
    return "SL" + *u;
  return none;
}

SType SliceType::replaceImpl(SType from, SType to) const {
  return get(underlying->replace(from, to));
}

optional<string> SliceType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<SliceType>())
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  else
    return "Can't bind type " + quote(argType->getName()) + " to type " + quote(getName());
}

shared_ptr<SliceType> SliceType::get(SType type) {
  static map<SType, shared_ptr<SliceType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<SliceType>(Private{}, type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

bool SliceType::isBuiltinCopyable(const Context&) const {
  return true;
}

optional<string> SliceType::getSizeError() const {
  return underlying->getSizeError();
}

SliceType::SliceType(SliceType::Private, SType t) : underlying(std::move(t)) {}
