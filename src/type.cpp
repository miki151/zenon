#include "type.h"
#include "context.h"
#include "ast.h"

ArithmeticType::DefType ArithmeticType::INT = shared<ArithmeticType>("int");
ArithmeticType::DefType ArithmeticType::DOUBLE = shared<ArithmeticType>("double");
ArithmeticType::DefType ArithmeticType::VOID = shared<ArithmeticType>("void");
ArithmeticType::DefType ArithmeticType::BOOL = shared<ArithmeticType>("bool");
ArithmeticType::DefType ArithmeticType::STRING = shared<ArithmeticType>("string", "zenon_string"s);
ArithmeticType::DefType ArithmeticType::CHAR = shared<ArithmeticType>("char");
ArithmeticType::DefType ArithmeticType::NORETURN = shared<ArithmeticType>("noreturn", "[[noreturn]] void"s);
ArithmeticType::DefType ArithmeticType::ANY_TYPE = shared<ArithmeticType>("any_type");
ArithmeticType::DefType ArithmeticType::ENUM_TYPE = shared<ArithmeticType>("enum_type");
ArithmeticType::DefType ArithmeticType::NULL_TYPE = shared<ArithmeticType>("null_type");
ArithmeticType::DefType ArithmeticType::STRUCT_TYPE = shared<ArithmeticType>("struct_type");

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
  return (type == ArithmeticType::ANY_TYPE && (t->getType() == ArithmeticType::ENUM_TYPE || t->getType() == ArithmeticType::STRUCT_TYPE)) ||
      t->getType() == type;
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

WithError<Type::MemberInfo> TemplateParameterType::getTypeOfMember(const SType& value1) const {
  if (auto value = value1.dynamicCast<CompileTimeValue>()) {
    if (auto ref = value->value.getReferenceMaybe<CompileTimeValue::ReferenceValue>())
      value = ref->value;
    if (value->getType() != ArithmeticType::INT)
      return "Member index must be of type: " + quote(ArithmeticType::INT->getName()) + ", got " + value->getType()->getName();
    if (type == ArithmeticType::STRUCT_TYPE)
      return MemberInfo{(SType)TemplateStructMemberType::get(this->get_this().get(), value), "bad_member_name"};
    return Type::getTypeOfMember(value);
  } else
    return "Member index must be of type: " + quote(ArithmeticType::INT->getName()) + ", got " + value1->getType()->getName();
}

WithError<SType> TemplateParameterType::getTypeOfMember(const string& s) const {
  if (type == ArithmeticType::STRUCT_TYPE)
    return "Can only refer to template struct type members by index"s;
  return Type::getTypeOfMember(s);
}

bool TemplateParameterType::canBeValueTemplateParam() const {
  return true;
}

TemplateParameterType::TemplateParameterType(string n, CodeLoc l) : name(n), declarationLoc(l), type(ArithmeticType::ANY_TYPE) {}

TemplateParameterType::TemplateParameterType(SType type, string name, CodeLoc l)
    : name(std::move(name)), declarationLoc(l), type(std::move(type)) {
}

string EnumType::getName(bool withTemplateArguments) const {
  return name;
}

optional<ErrorLoc> EnumType::handleSwitchStatement(SwitchStatement& statement, Context& context, SwitchArgument) const {
  statement.type = SwitchStatement::ENUM;
  statement.targetType = (SType)get_this();
  unordered_set<string> handledElems;
  for (auto& caseElem : statement.caseElems) {
    if (!caseElem.type.contains<none_t>())
      return caseElem.codeloc.getError("Expected enum element");
    for (auto& id : caseElem.ids) {
      if (!contains(elements, id))
        return caseElem.codeloc.getError("Element " + quote(id) + " not present in enum " + quote(name));
      if (handledElems.count(id))
        return caseElem.codeloc.getError("Enum element " + quote(id)
            + " handled more than once in switch statement");
      handledElems.insert(id);
    }
    if (auto err = caseElem.block->check(context))
      return err;
  }
  if (!statement.defaultBlock) {
    vector<string> unhandled;
    for (auto& elem : elements)
      if (!handledElems.count(elem))
        unhandled.push_back(quote(elem));
    if (!unhandled.empty())
      return statement.codeLoc.getError(quote(name) + " elements " + combine(unhandled, ", ")
          + " not handled in switch statement");
  } else {
    if (handledElems.size() == elements.size())
      return statement.defaultBlock->codeLoc.getError(
          "Default switch statement unnecessary when all enum elements are handled");
    if (auto err = statement.defaultBlock->check(context))
      return err;
  }
  return none;
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
    generated.insert(make_pair(args, std::move(ret)));
  }
  return generated.at(args);
}

string FunctionInfo::prettyString() const {
  return type.retVal->getName() + " " + toString(id) + joinTemplateParams(type.templateParams, type.variadicTemplate) + "(" +
      combine(transform(type.params, [](auto& t) { return t.type->getName() + (t.name ? " " + *t.name : ""s); }), ", ") +
      (type.variadicParams ? "...)" : ")") +
      (type.fromConcept ? " [from concept]" : "") +
      (getParent()->definition ? getParent()->definition->codeLoc.toString() : "");
}

string FunctionInfo::getMangledName() const {
  if (isMainFunction())
    return "zenonMain"s;
  if (id == "copy"s)
    return "copy";
  return id.visit(
      [this](const string& s) {
        return s + *getMangledSuffix();
      },
      [this](Operator op) {
        if (auto opName = getCodegenName(op))
          return opName + *getMangledSuffix();
        else
          return "operator "s + getString(op);
      },
      [this](ConstructorTag) {
        if (type.generatedConstructor)
          return type.retVal->getCodegenName();
        else
          return "construct_" + type.retVal->getCodegenName();
      }
  );
}

bool FunctionInfo::isMainFunction() const {
  return id == "main"s;
}

optional<string> FunctionInfo::getMangledSuffix() const {
  string suf;
  if (!type.retVal->getMangledName())
    return none;
  for (auto& param : type.params)
    if (!param.type->getMangledName())
      return none;
  for (auto& arg : type.templateParams)
    if (auto name = arg->getMangledName())
      suf += *name;
    else
      return none;
  if (id != "copy"s && !type.builtinOperator)
    if (auto def = getParent()->definition)
      suf += to_string(def->codeLoc.line);
  return suf;
}

SFunctionInfo FunctionInfo::getWithoutRequirements() const {
  auto newType = type;
  newType.requirements.clear();
  return getInstance(id, std::move(newType), getParent());
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

Type::Type() : staticContext(nullptr) {
}

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

bool ArithmeticType::canBeValueTemplateParam() const {
  return canDeclareVariable() && this != DOUBLE.get();
}

bool ArithmeticType::canDeclareVariable() const {
  return this == INT.get() || this == BOOL.get() || this == CHAR.get() || this == STRING.get() || this == DOUBLE.get();
}

SType Type::removePointer() const {
  return get_this().get();
}

optional<string> Type::getSizeError(const Context&) const {
  return none;
}

bool Type::canBeValueTemplateParam() const {
  return false;
}

bool Type::canDeclareVariable() const {
  return true;
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

static optional<string> checkMembers(const Context& context, set<const Type*> &visited, const SType& t) {
  if (auto s = t.dynamicCast<StructType>()) {
    if (visited.count(t.get()))
      return "Type " + t->getName() + " has infinite size"s;
    if (!context.isFullyDefined(t.get()))
      return "Type " + t->getName() + " is incomplete in this context"s;
    visited.insert(t.get());
    for (auto& member : s->members)
      if (auto res = checkMembers(context, visited, member.type))
        return res;
    for (auto& member : s->alternatives)
      if (auto res = checkMembers(context, visited, member.type))
        return res;
    visited.erase(t.get());
  }
  return none;
}

optional<string> StructType::getSizeError(const Context& context) const {
  set<const Type*> visited;
  return checkMembers(context, visited, get_this().get());
}

optional<ErrorLoc> ReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, SwitchArgument) const {
  return underlying->handleSwitchStatement(statement, context, SwitchArgument::REFERENCE);
}

WithError<Type::MemberInfo> ReferenceType::getTypeOfMember(const SType& value) const {
  if (auto res = underlying->getTypeOfMember(value))
    return MemberInfo{(SType)get(res->type), res->name};
  else
    return res;
}

WithError<SType> ReferenceType::getTypeOfMember(const string& name) const {
  if (auto res = underlying->getTypeOfMember(name))
    return (SType)get(*res);
  else
    return res;
}

SType ReferenceType::removePointer() const {
  return underlying->removePointer();
}

optional<ErrorLoc> MutableReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, SwitchArgument) const {
  return underlying->handleSwitchStatement(statement, context, SwitchArgument::MUTABLE_REFERENCE);
}

WithError<Type::MemberInfo> MutableReferenceType::getTypeOfMember(const SType& value) const {
  if (auto res = underlying->getTypeOfMember(value))
    return MemberInfo{(SType)get(res->type), res->name};
  else
    return res;
}

WithError<SType> MutableReferenceType::getTypeOfMember(const string& name) const {
  if (auto res = underlying->getTypeOfMember(name))
    return (SType)get(*res);
  else
    return res;
}

SType MutableReferenceType::removePointer() const {
  return underlying->removePointer();
}

optional<ErrorLoc> StructType::handleSwitchStatement(SwitchStatement& statement, Context& outsideContext,
    SwitchArgument argumentType) const {
  if (alternatives.empty())
    return statement.codeLoc.getError("Can't switch on a struct type");
  statement.type = SwitchStatement::VARIANT;
  statement.targetType = (SType)get_this();
  unordered_set<string> handledTypes;
  auto getAlternativeType = [&] (const string& name) -> nullable<SType> {
    for (auto& alternative : alternatives)
      if (alternative.name == name)
        return alternative.type;
    return nullptr;
  };
  for (auto& caseElem : statement.caseElems) {
    if (caseElem.ids.size() > 1)
      return caseElem.codeloc.getError("Multiple case elements not allowed in a variant switch");
    auto caseId = caseElem.ids[0];
    caseElem.declaredVar = caseId;
    if (!getAlternativeType(caseId))
      return caseElem.codeloc.getError("Element " + quote(caseId) +
          " not present in variant " + quote(getName()));
    if (handledTypes.count(caseId))
      return caseElem.codeloc.getError("Variant element " + quote(caseId)
        + " handled more than once in switch statement");
    handledTypes.insert(caseId);
    auto caseBodyContext = Context::withParent(outsideContext);
    auto realType = getAlternativeType(caseId).get();
    if (realType != ArithmeticType::VOID)
      caseElem.varType = caseElem.VALUE;
    auto caseTypeTmp = caseElem.getType(outsideContext);
    if (!caseTypeTmp)
      return caseTypeTmp.get_error();
    if (auto caseType = caseTypeTmp.get()) {
      if (caseType != realType && caseType->removePointer() != realType)
        return caseElem.codeloc.getError(
            "Can't handle variant element " + quote(caseId) + " of type " +
            quote(realType->getName()) + " as type " + quote(caseType->getName()));
      if (caseType == MutablePointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        if (argumentType != SwitchArgument::MUTABLE_REFERENCE)
          return caseElem.codeloc.getError(
              "Can only bind element to mutable pointer when switch argument is a mutable reference");
        if (realType == ArithmeticType::VOID)
          return caseElem.codeloc.getError("Can't bind void element to pointer");
        caseBodyContext.addVariable(caseId, MutableReferenceType::get(MutablePointerType::get(realType)));
      } else
      if (caseType == PointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        if (argumentType == SwitchArgument::VALUE)
          return caseElem.codeloc.getError(
              "Can only bind element to pointer when switch argument is a reference");
        if (realType == ArithmeticType::VOID)
          return caseElem.codeloc.getError("Can't bind void element to pointer");
        caseBodyContext.addVariable(caseId, MutableReferenceType::get(PointerType::get(realType)));
      }
    }
    if (caseElem.varType == caseElem.VALUE) {
      if (argumentType != SwitchArgument::VALUE && !realType->isBuiltinCopyable(outsideContext))
        return caseElem.codeloc.getError(
            "Type " + quote(realType->getName()) + " is not implicitly copyable. "
            "Try binding to a pointer or move from the variable that you are switching on");
      caseBodyContext.addVariable(caseId, ReferenceType::get(realType));
    }
    if (auto err = caseElem.block->check(caseBodyContext))
      return err;
  }
  if (!statement.defaultBlock) {
    vector<string> unhandled;
    for (auto& alternative : alternatives)
      if (!handledTypes.count(alternative.name))
        unhandled.push_back(quote(alternative.name));
    if (!unhandled.empty())
      return statement.codeLoc.getError(quote(name) + " subtypes " + combine(unhandled, ", ")
        + " not handled in switch statement");
  } else {
    if (handledTypes.size() == alternatives.size())
      return statement.defaultBlock->codeLoc.getError(
          "Default switch statement unnecessary when all variant cases are handled");
    if (auto err = statement.defaultBlock->check(outsideContext))
      return err;
  }
  return none;
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
  auto type = shared<StructType>(name, Private{});
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
    ErrorBuffer errors;
    for (int i = 0; i < templateParams.size(); ++i) {
      type->staticContext.replace(templateParams[i], type->templateParams[i], errors);
      for (auto& alternative : type->alternatives)
        alternative.type = alternative.type->replace(templateParams[i], type->templateParams[i], errors);
      for (auto& member : type->members)
        member.type = member.type->replace(templateParams[i], type->templateParams[i], errors);
    }
    CHECK(errors.empty());
  }
}

WithError<Type::MemberInfo> StructType::getTypeOfMember(const SType& v1) const {
  if (auto v = v1.dynamicCast<CompileTimeValue>()) {
    if (auto ref = v->value.getReferenceMaybe<CompileTimeValue::ReferenceValue>())
      v = ref->value;
    if (auto intValue = v->value.getValueMaybe<int>()) {
      if (*intValue >= 0 && *intValue < members.size())
        return MemberInfo{members[*intValue].type, members[*intValue].name};
      else
        return "Member index for type " + quote(getName()) + " must be between 0 and " + to_string(members.size() - 1) +
            ", got " + to_string(*intValue);
    }
    if (v->getType() == ArithmeticType::INT)
      return MemberInfo{(SType)TemplateStructMemberType::get(this->get_this().get(), v), "bad_member_name"};
  }
  return "Member index must be of type: " + quote(ArithmeticType::INT->getName()) + ", got " + v1->getType()->getName();
}

SType StructType::getType() const {
  return ArithmeticType::STRUCT_TYPE;
}

StructType::StructType(string name, StructType::Private) : name(std::move(name)) {
}

SType Type::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto self = get_this().get();
  if (from == self) {
    if (!canReplaceBy(to))
      errors.push_back("Can't substitute type " + from->getName() + " by " + to->getName());
    return to;
  } else
    return replaceImpl(from, to, errors);
}

bool Type::canReplaceBy(SType) const {
  return false;
}

SType Type::replaceImpl(SType, SType, ErrorBuffer&) const {
  return get_this().get();
}

SType Type::getType() const {
  return ArithmeticType::ANY_TYPE;
}

WithError<Type::MemberInfo> Type::getTypeOfMember(const SType&) const {
  return "Type " + quote(getName()) + " doesn't support dot operator"s;
}

WithError<SType> Type::getTypeOfMember(const string&) const {
  return "Type " + quote(getName()) + " doesn't support dot operator"s;
}

SType ReferenceType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return ReferenceType::get(underlying->replace(from, to, errors));
}

SType MutableReferenceType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return MutableReferenceType::get(underlying->replace(from, to, errors));
}

SType PointerType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return PointerType::get(underlying->replace(from, to, errors));
}

SType MutablePointerType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return MutablePointerType::get(underlying->replace(from, to, errors));
}

static void checkNonVoidMember(const SType& type, const SType& to, ErrorBuffer& errors) {
  if (auto param = type.dynamicCast<TemplateParameterType>())
    if (to == ArithmeticType::VOID)
      errors.push_back(
          "Can't instantiate member type with type " + quote(ArithmeticType::VOID->getName()));
}

namespace {
struct RequirementVisitor {
  SType from, to;
  ErrorBuffer& errors;
  void operator()(SConcept& concept) {
    concept = concept->replace(from, to, errors);
  }
  void operator()(shared_ptr<Expression>& expr) {
    ErrorLocBuffer errors2;
    expr = expr->replace(from, to, errors2);
    if (auto constant = expr.dynamicCast<Constant>())
      if (auto value = constant->value->value.getValueMaybe<bool>())
        if (!*value)
          errors.push_back(expr->codeLoc.toString() + ": Requirement evaluates to false");
    if (!errors2.empty())
      errors.push_back(errors2[0].error);
  }
};
}

SType StructType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  vector<SType> newTemplateParams;
  for (auto& param : templateParams)
    newTemplateParams.push_back(param->replace(from, to, errors));
  auto ret = parent->getInstance(newTemplateParams);
  // This is how we check if instantiate gave us a new type to fill
  if (ret->templateParams != newTemplateParams) {
    ret->templateParams = newTemplateParams;
    INFO << "New instantiation: " << ret->getName();
    ret->staticContext.deepCopyFrom(staticContext);
    ret->alternatives = alternatives;
    for (auto& alternative : ret->alternatives) {
      checkNonVoidMember(alternative.type, to, errors);
      alternative.type = alternative.type->replace(from, to, errors);
    }
    ret->members = members;
    for (auto& members : ret->members) {
      checkNonVoidMember(members.type, to, errors);
      members.type = members.type->replace(from, to, errors);
    }
    ret->requirements = requirements;
    for (auto& req : ret->requirements)
      req.base.visit(RequirementVisitor{from, to, errors});
    ret->staticContext.replace(from, to, errors);
  } else
    INFO << "Found instantiated: " << ret->getName();
  return ret;
}

FunctionType replaceInFunction(FunctionType type, SType from, SType to, ErrorBuffer& errors) {
  if (type.parentType)
    type.parentType = type.parentType->replace(from, to, errors);
  type.retVal = type.retVal->replace(from, to, errors);
  for (auto& param : type.params)
    param.type = param.type->replace(from, to, errors);
  for (auto& param : type.templateParams)
    param = param->replace(from, to, errors);
  for (auto& req : type.requirements)
    req.base.visit(RequirementVisitor{from, to, errors});
  return type;
}

SFunctionInfo replaceInFunction(const SFunctionInfo& fun, SType from, SType to, ErrorBuffer& errors) {
  return FunctionInfo::getInstance(fun->id, replaceInFunction(fun->type, from, to, errors), fun);
}

WithErrorLine<SType> Type::instantiate(const Context& context, vector<SType> templateArgs, CodeLoc loc) const {
  if (templateArgs.empty())
    return get_this().get();
  else
    return loc.getError("Type " + quote(getName()) + " is not a template");
}

Context& Type::getStaticContext() {
  return staticContext;
}

optional<ErrorLoc> Type::handleSwitchStatement(SwitchStatement& s, Context&, SwitchArgument) const {
  return s.codeLoc.getError("Can't switch on the value of type " + quote(getName()));
}

static string getCantSubstituteError(SType param, SType with) {
  auto ret = "Can't substitute template parameter " + quote(param->getName()) + " ";
  if (param->getType() != ArithmeticType::ANY_TYPE)
    ret += "of type " + quote(param->getType()->getName()) + " ";
  ret += "with type " + quote(with->getName());
  return ret;
}

WithErrorLine<SType> StructType::instantiate(const Context& context, vector<SType> templateArgs, CodeLoc loc) const {
  CHECK(parent == this) << "Struct instatiated a second time?";
  if (templateArgs.size() != templateParams.size())
    return loc.getError("Wrong number of template parameters for type " + getName());
  auto ret = get_this().get().dynamicCast<StructType>();
  ErrorBuffer errors;
  for (int i = 0; i < templateParams.size(); ++i) {
    if (!ret->templateParams[i]->canReplaceBy(templateArgs[i]))
      return loc.getError(getCantSubstituteError(templateParams[i], templateArgs[i]));
    ret = ret->replace(ret->templateParams[i], templateArgs[i], errors).dynamicCast<StructType>();
  }
  for (auto& arg : templateArgs)
    if (arg.dynamicCast<CompileTimeValue>() && !arg->getType()->canBeValueTemplateParam())
      return loc.getError("Value template parameter cannot have type " + quote(arg->getType()->getName()));
  for (auto& req : ret.dynamicCast<StructType>()->requirements)
    if (auto concept = req.base.getReferenceMaybe<SConcept>()) {
      if (auto res = context.getRequiredFunctions(**concept, {}); !res)
        return loc.getError(res.get_error());
    } else
    if (auto expr1 = req.base.getReferenceMaybe<shared_ptr<Expression>>()) {
      if (expr1->get()->eval(context)->value == CompileTimeValue::get(false))
        return loc.getError("Unable to insantiate " + quote(ret->getName()) + ": predicate requirement evaluates to false");
    }
  if (!errors.empty())
    return loc.getError(errors[0]);
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

string getExpandedParamName(const string& packName, int index) {
  return "_" + packName + to_string(index);
}

static optional<ErrorLoc> expandVariadicTemplate(FunctionType& type, CodeLoc codeLoc, vector<SType> templateArgs,
    vector<SType> argTypes) {
  vector<SType> expandedTypes;
  nullable<SType> lastTemplateParam;
  if (!type.templateParams.empty())
    lastTemplateParam = type.templateParams.back();
  if (type.variadicTemplate) {
    type.templateParams.pop_back();
    optional<FunctionType::Param> lastParam;
    int cnt = 0;
    while (templateArgs.size() > type.templateParams.size()) {
      type.templateParams.push_back(shared<TemplateParameterType>(lastTemplateParam->getType(),
          getExpandedParamName(lastTemplateParam->getName(), cnt), codeLoc));
      expandedTypes.push_back(type.templateParams.back());
      if (type.variadicParams) {
        if (!lastParam) {
          lastParam = type.params.back();
          type.params.pop_back();
        }
        ErrorBuffer errors;
        type.params.push_back(FunctionType::Param(
            lastParam->name.map([cnt](const string& name) { return getExpandedParamName(name, cnt); }),
            lastParam->type->replace(lastTemplateParam.get(), type.templateParams.back(), errors)));
        if (!errors.empty())
          return codeLoc.getError(errors[0]);
      }
      ++cnt;
    }
    if (lastParam)
      type.variadicParams = false;
  }
  if (type.variadicParams) {
    auto lastParam = type.params.back();
    type.params.pop_back();
    int cnt = 0;
    while (argTypes.size() > type.params.size()) {
      auto thisType = lastParam.type;
      if (type.variadicTemplate) {
        if (expandedTypes.size() <= cnt) {
          type.templateParams.push_back(shared<TemplateParameterType>(lastTemplateParam->getType(),
              getExpandedParamName(lastTemplateParam->getName(), cnt), codeLoc));
          expandedTypes.push_back(type.templateParams.back());
        }
        ErrorBuffer errors;
        thisType = lastParam.type->replace(lastTemplateParam.get(), type.templateParams.back(), errors);
        if (!errors.empty())
          return codeLoc.getError(errors[0]);
      }
      type.params.push_back(FunctionType::Param(
          lastParam.name.map([cnt](const string& name) { return getExpandedParamName(name, cnt); }),
          thisType));
      ++cnt;
    }
  }
  vector<TemplateRequirement> newRequirements;
  ErrorBuffer errors;
  ErrorLocBuffer errors2;
  for (auto requirement : type.requirements)
    if (!requirement.variadic) {
      newRequirements.push_back(requirement);
      if (auto concept = newRequirements.back().base.getReferenceMaybe<SConcept>())
        if (concept->get()->isVariadic() && concept->get()->getParams().back() == lastTemplateParam.get())
          newRequirements.back().base = concept->get()->expand(lastTemplateParam.get(), expandedTypes, errors);
    } else {
      for (auto& expanded : expandedTypes)
        newRequirements.push_back(requirement.base.visit(
            [&](const SConcept& r) {
              return TemplateRequirement(r->replace(lastTemplateParam.get(), expanded, errors), false);
            },
            [&](const shared_ptr<Expression>& r) {
              return TemplateRequirement(shared_ptr<Expression>(r->replace(lastTemplateParam.get(), expanded, errors2)), false);
            }
        ));
      break;
    }
  type.requirements = std::move(newRequirements);
  return none;
}

static optional<ErrorLoc> checkImplicitCopies(const Context& context, const vector<SType>& paramTypes, vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc) {
  for (int i = 0; i < paramTypes.size(); ++i) {
    auto& paramType = paramTypes[i];
    auto& argType = argTypes[i];
    if ((!paramType.dynamicCast<ReferenceType>() && !paramType.dynamicCast<MutableReferenceType>()) &&
        (argType.dynamicCast<ReferenceType>() || argType.dynamicCast<MutableReferenceType>())) {
      if (argType->getUnderlying()->isBuiltinCopyable(context))
        argType = argType->getUnderlying();
      else
        return argLoc[i].getError("Type " + quote(argType->getUnderlying()->getName()) + " cannot be copied.");
    }
  }
  return none;
}

static optional<ErrorLoc> checkRequirements(const Context& context, const vector<TemplateRequirement>& requirements, CodeLoc codeLoc,
    vector<FunctionType> existing) {
  for (auto& req : requirements)
    if (auto concept = req.base.getReferenceMaybe<SConcept>()) {
      if (auto res = context.getRequiredFunctions(**concept, existing); !res)
        return codeLoc.getError(res.get_error());
    } else
    if (auto expr1 = req.base.getReferenceMaybe<shared_ptr<Expression>>()) {
      if (expr1->get()->eval(context)->value == CompileTimeValue::get(false))
        return expr1->get()->codeLoc.getError("Predicate evaluates to false");
    }
  return none;
}

static optional<ErrorLoc> getConversionError(const Context& context, const SFunctionInfo& input, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc, const vector<SType>& funParams, TypeMapping& mapping) {
  for (int i = 0; i < argTypes.size(); ++i) {
    optional<ErrorLoc> firstError;
    if (argTypes[i] != ArithmeticType::NULL_TYPE)
      for (auto tArg : context.getConversions(argTypes[i]))
        if (!input->id.contains<Operator>() || tArg == argTypes[i] ||
            (tArg.dynamicCast<ArithmeticType>() && argTypes[i].dynamicCast<ArithmeticType>())) {
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
  return none;
}

WithErrorLine<SFunctionInfo> instantiateFunction(const Context& context, const SFunctionInfo& input, CodeLoc codeLoc,
    vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc, vector<FunctionType> existing) {
  FunctionType type = input->type;
  if (auto error = expandVariadicTemplate(type, codeLoc, templateArgs, argTypes))
    return *error;
  const vector<SType> funParams = transform(type.params, [](const FunctionType::Param& p) { return p.type; });
  if (funParams.size() != argTypes.size())
    return codeLoc.getError("Wrong number of function arguments. Expected " +
        to_string(funParams.size()) + " got " + to_string(argTypes.size()));
  auto implicitCopyError = checkImplicitCopies(context, funParams, argTypes, argLoc);
  if (templateArgs.size() > type.templateParams.size())
    return codeLoc.getError("Too many template arguments.");
  TypeMapping mapping { type.templateParams, vector<nullable<SType>>(type.templateParams.size()) };
  for (int i = 0; i < templateArgs.size(); ++i)
    mapping.templateArgs[i] = templateArgs[i];
  if (auto err = getConversionError(context, input, argTypes, argLoc, funParams, mapping))
    return *err;
  for (int i = 0; i < type.templateParams.size(); ++i) {
    if (i >= templateArgs.size()) {
      if (auto deduced = mapping.templateArgs[i])
        templateArgs.push_back(deduced.get());
      else
        return codeLoc.getError("Couldn't deduce template argument " + quote(type.templateParams[i]->getName()));
    }
    if (templateArgs[i].dynamicCast<CompileTimeValue>() && !templateArgs[i]->getType()->canBeValueTemplateParam())
      return codeLoc.getError("Value template parameter cannot have type " + quote(templateArgs[i]->getType()->getName()));
  }
  ErrorBuffer errors;
  for (int i = 0; i < type.templateParams.size(); ++i) {
    if (!type.templateParams[i]->canReplaceBy(templateArgs[i]))
      return codeLoc.getError(getCantSubstituteError(type.templateParams[i], templateArgs[i]));
    type = replaceInFunction(type, type.templateParams[i], templateArgs[i], errors);
    type.templateParams[i] = templateArgs[i];
  }
  if (errors.empty())
    for (auto& fun : existing)
      if (areParamsTypesEqual(input->type, fun))
        // To avoid infinite recursion we don't check concept requirements twice for the same >>original<< function
        // (not instantation). If this causes issues then it needs to be revised.
        return FunctionInfo::getInstance(input->id, type, input);
  existing.push_back(input->type);
  //cout << "Instantiating " << type.toString() << " " << existing.size() << endl;
  if (auto err = checkRequirements(context, type.requirements, codeLoc, existing))
    return *err;
  // The replace() errors need to be checked after returning potential requirement errors.
  if (!errors.empty())
    return codeLoc.getError(errors[0]);
  if (implicitCopyError)
    return *implicitCopyError;
  return FunctionInfo::getInstance(input->id, type, input);
}

EnumType::EnumType(string n, Private) : name(std::move(n)) {}

Concept::Concept(const string& name, Context context, bool variadic) : name(name), context(std::move(context)), variadic(variadic) {
}

string Concept::getName() const {
  return name + joinTemplateParams(params);
}

SConcept Concept::translate(vector<SType> newParams, bool variadicParams, ErrorBuffer& errors) const {
  auto ret = shared<Concept>(name, Context(context.typeRegistry), variadicParams);
  ret->context.deepCopyFrom(context);
  ret->params = newParams;
  if (!variadic || variadicParams) {
    CHECK(params.size() == newParams.size());
    for (int i = 0; i < params.size(); ++i)
      ret->context.replace(params[i], newParams[i], errors);
  } else {
    CHECK(params.size() - 1 <= newParams.size());
    for (int i = 0; i < params.size() - 1; ++i)
      ret->context.replace(params[i], newParams[i], errors);
    ret->context.expand(params.back(), getSubsequence(newParams, params.size() - 1), errors);
  }
  return ret;
}

SConcept Concept::expand(SType from, vector<SType> newParams, ErrorBuffer& errors) const {
  auto ret = shared<Concept>(name, Context(context.typeRegistry), variadic);
  ret->context.deepCopyFrom(context);
  ret->params = params;
  ret->variadic = false;
  ret->context.expand(params.back(), newParams, errors);
  ret->params.pop_back();
  return ret;
}

SConcept Concept::replace(SType from, SType to, ErrorBuffer& errors) const {
  auto ret = shared<Concept>(name, Context(context.typeRegistry), variadic);
  ret->context.deepCopyFrom(context);
  ret->params = params;
  ret->variadic = variadic;
  for (auto& param : ret->params)
    param = param->replace(from, to, errors);
  ret->context.replace(from, to, errors);
  return ret;
}

const vector<SType>& Concept::getParams() const {
  return params;
}

bool Concept::isVariadic() const {
  return variadic;
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

string joinTemplateParams(const vector<SType>& params, bool variadic) {
  if (params.empty())
    return "";
  else
    return "<" + joinTypeList(params) + (variadic ? "...>" : ">");
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

SType ArrayType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  if (from == size)
    if (auto value = to.dynamicCast<CompileTimeValue>())
      if (auto intValue = value->value.getValueMaybe<int>())
        if (*intValue < 0)
          errors.push_back("Can't have negative array size");
  return get(underlying->replace(from, to, errors), size->replace(from, to, errors).dynamicCast<CompileTimeValue>());
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

optional<string> ArrayType::getSizeError(const Context& context) const {
  return underlying->getSizeError(context);
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
    auto ret = shared<CompileTimeValue>(Private{}, value);
    generated.insert({value, ret});
  }
  for (auto& elem : generated)
    CHECK(elem.first == elem.second->value);
  return generated.at(value);
}

CompileTimeValue::CompileTimeValue(Private, Value value) : value(std::move(value)) {
}

string CompileTimeValue::getName(bool withTemplateArguments) const {
  return value.visit(
      [](int v) { return to_string(v); },
      [](double v) { return to_string(v); },
      [](bool v) { return v ? "true" : "false"; },
      [](char v) { return "\'" + string(1, v) + "\'"; },
      [](const ReferenceValue& ref) { return ref.value->getName(); },
      [](NullValue v) { return "null"; },
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
      [](const ReferenceValue& ref) { return ref.value->getName(); },
      [](const string& v) { return "\"" + v +"\"_lstr"; },
      [](const ArrayValue& t) { return "make_array<" + t.type->getCodegenName() + ">(" + combine(transform(t.values,
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
      [](const ReferenceValue& ref)-> SType {  return MutableReferenceType::get(ref.value->getType()); },
      [](NullValue)-> SType {  return ArithmeticType::NULL_TYPE; },
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
      [](char c) -> optional<string> { return "C" + to_string(int(c)); },
      [](const string& v) -> optional<string> {
        string ret;
        for (auto c : v)
          if (isalpha(c))
            ret += c;
          else
            ret += "_" + to_string(int(c));
        return std::move(ret);
      },
      [](const ArrayValue& t) -> optional<string> {
        vector<string> names;
        for (auto& elem : t.values)
          if (auto name = elem->getMangledName())
            names.push_back(*name);
          else
            return none;
        return "Arr"s + combine(names, "");
      },
      [](const ReferenceValue& ref)-> optional<string> {
        if (auto name = ref.value->getMangledName())
          return "Ref" + *name;
        else
          return none;
        },
      [](const TemplateValue& v) -> optional<string> { return none; },
      [](const TemplateExpression&) -> optional<string> { return none; },
      [](const TemplateFunctionCall&) -> optional<string> { return none; }
  );
}

shared_ptr<CompileTimeValue> CompileTimeValue::getReference(SCompileTimeValue value) {
  return get(ReferenceValue{std::move(value)});
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
    if (auto v = t.dynamicCast<CompileTimeValue>())
      return myValue->type == v->getType();
  return false;
}

SType CompileTimeValue::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return value.visit(
      [&](const TemplateValue& value) {
        return CompileTimeValue::getTemplateValue(value.type->replace(from, to, errors), value.name);
      },
      [&](const TemplateExpression& value) {
        if (auto ret = ::eval(value.op, transform(value.args,
            [&](const SType& t){ return t->replace(from, to, errors); })))
          return ret.get();
        else {
          errors.push_back("Can't evaluate operator " + quote(getString(value.op)));
          return get_this().get();
        }
      },
      [&](const TemplateFunctionCall& value) {
        if (auto ret = value.functionInfo.invokeFunction(value.name, value.loc, transform(value.args,
            [&](const SType& t){ return t->replace(from, to, errors); }), value.argLoc))
          return ret.get();
        else {
          errors.push_back("Bad arguments to function " + quote(value.name));
          return get_this().get();
        }
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

SType SliceType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return get(underlying->replace(from, to, errors));
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

/*optional<string> SliceType::getSizeError(const Context& context) const {
  return underlying->getSizeError();
}*/

SliceType::SliceType(SliceType::Private, SType t) : underlying(std::move(t)) {}

string TemplateStructMemberType::getName(bool withTemplateArguments) const {
  return "typeof(" + structType->getName(withTemplateArguments) + "." + memberIndex->getName(withTemplateArguments) + ")";
}

optional<string> TemplateStructMemberType::getMangledName() const {
  return none;
}

shared_ptr<TemplateStructMemberType> TemplateStructMemberType::get(SType structType, SCompileTimeValue index) {
  static map<pair<SType, SCompileTimeValue>, shared_ptr<TemplateStructMemberType>> cache;
  if (!cache.count(make_pair(structType, index))) {
    cache.insert(make_pair(make_pair(structType, index), shared<TemplateStructMemberType>(Private{}, structType, index)));
  }
  return cache.at(make_pair(structType, index));
}

TemplateStructMemberType::TemplateStructMemberType(Private, SType structType, SCompileTimeValue index)
  : structType(std::move(structType)), memberIndex(std::move(index)) {}

string OptionalType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "?";
}

optional<string> OptionalType::getMangledName() const {
  return underlying->getMangledName().map([](const string& name) { return "OP" + name;});
}

string OptionalType::getCodegenName() const {
  return "std::optional<" + underlying->getCodegenName() + ">";
}

optional<string> OptionalType::getMappingError(const Context& context, TypeMapping& mapping, SType from) const {
  if (auto argPointer = from.dynamicCast<OptionalType>())
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  return "Can't bind type " + quote(from->getName()) + " to type " + quote(getName());
}

SType OptionalType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return OptionalType::get(underlying->replace(from, to, errors));
}

bool OptionalType::isBuiltinCopyable(const Context& context) const {
  return underlying->isBuiltinCopyable(context);
}

shared_ptr<OptionalType> OptionalType::get(SType type) {
  static map<SType, shared_ptr<OptionalType>> generated;
  if (!generated.count(type)) {
    auto ret = shared<OptionalType>(type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

OptionalType::OptionalType(SType t) : underlying(std::move(t)) {

}


string LambdaType::getName(bool withTemplateArguments) const {
  return name;
}

optional<string> LambdaType::getMangledName() const {
  auto& functionType = functionInfo->type;
  string suf;
  if (auto name = functionType.retVal->getMangledName())
    suf = *name;
  else
    return none;
  for (int i = 1; i < functionType.params.size(); ++i)
    if (auto name = functionType.params[i].type->getMangledName())
      suf += *name;
    else
      return none;
  for (auto& arg : functionType.templateParams)
    if (auto name = arg->getMangledName())
      suf += *name;
    else
      return none;
  return name + suf;
}

string LambdaType::getCodegenName() const {
  return *getMangledName();
}

SType LambdaType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  ErrorLocBuffer errors2;
  auto newBody = body->replace(from, to, errors2);
  for (auto& e : errors2)
    errors.push_back(e.error);
  auto ret = shared<LambdaType>(Private{}, name);
  ret->body = cast<StatementBlock>(std::move(newBody));
  auto tmpType = functionInfo->type;
  tmpType.params = getSubsequence(tmpType.params, 1);
  tmpType = replaceInFunction(tmpType, from, to, errors);
  tmpType.params = concat({FunctionType::Param(PointerType::get(ret))}, tmpType.params);
  ret->functionInfo = FunctionInfo::getImplicit(functionInfo->id, std::move(tmpType));
  for (auto& capture : captures)
    ret->captures.push_back(LambdaCapture{capture.name, capture.type->replace(from, to, errors)});
  return ret;
}

optional<string> LambdaType::getMappingError(const Context&, TypeMapping&, SType argType) const {
  return none;
}

bool LambdaType::isBuiltinCopyable(const Context& c) const {
  for (auto& t : captures)
    if (!t.type->isBuiltinCopyable(c))
      return false;
  return true;
}

static int getNewLambdaId() {
  static int allIds = 0;
  return ++allIds;
}

LambdaType::LambdaType() : LambdaType(Private{}, "LAMBDA" + to_string(getNewLambdaId())) {
}

LambdaType::LambdaType(Private, string name) : name(std::move(name)) {
}

LambdaType::~LambdaType() {
}

CompileTimeValue::ReferenceValue::ReferenceValue(SCompileTimeValue v) : value(std::move(v)) {
  static int idCounter = 0;
  id = ++idCounter;
}

string VariablePack::getName(bool withTemplateArguments) const {
  return identifier + "...";
}

VariablePack::VariablePack(SType p, string id) : identifier(id), packType(std::move(p)) {
}
