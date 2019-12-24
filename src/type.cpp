#include "type.h"
#include "context.h"
#include "ast.h"

BuiltinType::DefType BuiltinType::INT = shared<BuiltinType>("int");
BuiltinType::DefType BuiltinType::DOUBLE = shared<BuiltinType>("double");
BuiltinType::DefType BuiltinType::VOID = shared<BuiltinType>("void");
BuiltinType::DefType BuiltinType::BOOL = shared<BuiltinType>("bool");
BuiltinType::DefType BuiltinType::STRING = shared<BuiltinType>("string", "zenon_string"s);
BuiltinType::DefType BuiltinType::CHAR = shared<BuiltinType>("char");
BuiltinType::DefType BuiltinType::NORETURN = shared<BuiltinType>("noreturn", "[[noreturn]] void"s);
BuiltinType::DefType BuiltinType::ANY_TYPE = shared<BuiltinType>("any_type");
BuiltinType::DefType BuiltinType::ENUM_TYPE = shared<BuiltinType>("enum_type");
BuiltinType::DefType BuiltinType::NULL_TYPE = shared<BuiltinType>("null_type");
BuiltinType::DefType BuiltinType::STRUCT_TYPE = shared<BuiltinType>("struct_type");
BuiltinType::DefType BuiltinType::VARIANT_TYPE = shared<BuiltinType>("variant_type");

string BuiltinType::getName(bool withTemplateArguments) const {
  return name;
}

string BuiltinType::getCodegenName() const {
  return codegenName;
}

BuiltinType::BuiltinType(const string& name, optional<std::string> codegenName)
    : name(name), codegenName(codegenName.value_or(name)) {
}

string ReferenceType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + " lvalue";
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
  return underlying->getName(withTemplateArguments) + " mutable lvalue";
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
  return (type == BuiltinType::ANY_TYPE && t->getType()->isMetaType()) || t->getType() == type;
}

optional<string> TemplateParameterType::getMangledName() const {
  return none;
}

SType TemplateParameterType::getType() const {
  return type;
}

bool TemplateParameterType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  if (type == BuiltinType::ENUM_TYPE)
    return true;
  else
    return false;
}

WithError<Type::MemberInfo> TemplateParameterType::getTypeOfMember(const SType& value1) const {
  if (auto value = value1.dynamicCast<CompileTimeValue>()) {
    if (auto ref = value->value.getReferenceMaybe<CompileTimeValue::ReferenceValue>())
      value = ref->value;
    if (value->getType() != BuiltinType::INT)
      return "Member index must be of type: " + quote(BuiltinType::INT->getName()) + ", got " + value->getType()->getName();
    if (type == BuiltinType::STRUCT_TYPE || type == BuiltinType::VARIANT_TYPE)
      return MemberInfo{(SType)TemplateStructMemberType::get(this->get_this().get(), value), "bad_member_name"};
    return Type::getTypeOfMember(value);
  } else
    return "Member index must be of type: " + quote(BuiltinType::INT->getName()) + ", got " + value1->getType()->getName();
}

WithError<SType> TemplateParameterType::getTypeOfMember(const string& s) const {
  if (type == BuiltinType::STRUCT_TYPE)
    return "Can only refer to template struct type members by index"s;
  return Type::getTypeOfMember(s);
}

bool TemplateParameterType::canBeValueTemplateParam() const {
  return true;
}

TemplateParameterType::TemplateParameterType(string n, CodeLoc l) : name(n), declarationLoc(l), type(BuiltinType::ANY_TYPE) {}

TemplateParameterType::TemplateParameterType(SType type, string name, CodeLoc l)
    : name(std::move(name)), declarationLoc(l), type(std::move(type)) {
}

string EnumType::getName(bool withTemplateArguments) const {
  return name;
}

JustError<ErrorLoc> EnumType::handleSwitchStatement(SwitchStatement& statement, Context& context, SwitchArgument) const {
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
    TRY(caseElem.block->check(context));
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
    TRY(statement.defaultBlock->check(context));
  }
  return success;
}

FunctionType::FunctionType(SType returnType, vector<SType> p, vector<SType> tpl)
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
      combine(transform(type.params, [](auto& t) { return t->getName(); }), ", ") +
      (type.variadicParams ? "...)" : ")") +
      (type.fromConcept ? " [from concept]" : "") +
      (getParent()->definition ? getParent()->definition->codeLoc.toString() : "");
}

string FunctionInfo::getMangledName() const {
  if (isMainFunction())
    return "zenonMain"s;
  if (id == "copy"s || id == "implicit_copy"s)
    return id.get<string>();
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
    if (!param->getMangledName())
      return none;
  for (auto& arg : type.templateParams)
    if (auto name = arg->getMangledName())
      suf += *name;
    else
      return none;
  if (id != "copy"s && id != "invoke"s && !type.builtinOperator)
    if (auto def = getParent()->definition)
      suf += to_string(def->codeLoc.line);
  return suf;
}

optional<string> FunctionInfo::getParamName(int index) const {
  if (auto def = getParent()->definition) {
    if ((index < def->parameters.size() && (!def->isVariadicParams || type.variadicParams)) || index < def->parameters.size() - 1)
      return def->parameters[index].name;
    else if (auto& name = def->parameters.back().name)
      return getExpandedParamName(*name, index - def->parameters.size() + 1);
  }
    return none;
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

FunctionDefinition* FunctionInfo::getDefinition() const {
  return getParent()->definition;
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

bool Type::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return false;
}

bool BuiltinType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

bool BuiltinType::canBeValueTemplateParam() const {
  return canDeclareVariable() && DOUBLE != this;
}

bool BuiltinType::canDeclareVariable() const {
  return INT == this  || BOOL == this || CHAR == this || STRING == this || DOUBLE == this;
}

bool BuiltinType::isMetaType() const {
  return ANY_TYPE == this || STRUCT_TYPE == this || VARIANT_TYPE == this|| ENUM_TYPE == this;
}

SType Type::removePointer() const {
  return get_this().get();
}

JustError<string> Type::getSizeError(const Context&) const {
  return success;
}

bool Type::canBeValueTemplateParam() const {
  return false;
}

bool Type::canDeclareVariable() const {
  return true;
}

bool Type::isMetaType() const {
  return false;
}

bool Type::isBuiltinCopyable(const Context& context, unique_ptr<Expression>& expr) const {
  if (isBuiltinCopyableImpl(context, expr))
    return true;
  else
  if (auto fun = getImplicitCopyFunction(context, CodeLoc(), get_this().get())) {
    if (expr) {
      auto tmpContext = Context::withParent(context);
      auto codeLoc = expr->codeLoc;
      expr = unique<FunctionCall>(expr->codeLoc, IdentifierInfo("implicit_copy", expr->codeLoc),
          unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false);
      auto err = expr->getTypeImpl(tmpContext);
      CHECK(!!err) << err.get_error();
    }
    return true;
  }
  return false;
}

bool Type::isBuiltinCopyable(const Context& context) const {
  unique_ptr<Expression> expr;
  return isBuiltinCopyable(context, expr);
}

bool PointerType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

SType PointerType::removePointer() const {
  return underlying;
}

bool EnumType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

SType EnumType::getType() const {
  return BuiltinType::ENUM_TYPE;
}

bool ReferenceType::canAssign(SType from) const {
  return false;
}

bool MutableReferenceType::canAssign(SType from) const {
  return underlying == from->getUnderlying();
}

static JustError<string> checkMembers(const Context& context, set<const Type*> &visited, const SType& t) {
  if (auto s = t.dynamicCast<StructType>()) {
    if (visited.count(t.get()))
      return "Type " + t->getName() + " has infinite size"s;
    if (!context.isFullyDefined(t.get()))
      return "Type " + t->getName() + " is incomplete in this context"s;
    visited.insert(t.get());
    for (auto& member : s->members)
      TRY(checkMembers(context, visited, member.type));
    for (auto& member : s->alternatives)
      TRY(checkMembers(context, visited, member.type));
    visited.erase(t.get());
  }
  return success;
}

JustError<string> StructType::getSizeError(const Context& context) const {
  set<const Type*> visited;
  return checkMembers(context, visited, get_this().get());
}

JustError<ErrorLoc> ReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, SwitchArgument) const {
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

JustError<ErrorLoc> MutableReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, SwitchArgument) const {
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

JustError<ErrorLoc> StructType::handleSwitchStatement(SwitchStatement& statement, Context& outsideContext,
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
    if (realType != BuiltinType::VOID)
      caseElem.varType = caseElem.VALUE;
    if (auto caseType = TRY(caseElem.getType(outsideContext))) {
      if (caseType != realType && caseType->removePointer() != realType)
        return caseElem.codeloc.getError(
            "Can't handle variant element " + quote(caseId) + " of type " +
            quote(realType->getName()) + " as type " + quote(caseType->getName()));
      if (caseType == MutablePointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        if (argumentType != SwitchArgument::MUTABLE_REFERENCE)
          return caseElem.codeloc.getError(
              "Can only bind element to mutable pointer when switch argument is a mutable reference");
        if (realType == BuiltinType::VOID)
          return caseElem.codeloc.getError("Can't bind void element to pointer");
        caseBodyContext.addVariable(caseId, MutableReferenceType::get(MutablePointerType::get(realType)), caseElem.codeloc);
      } else
      if (caseType == PointerType::get(realType)) {
        caseElem.varType = caseElem.POINTER;
        if (argumentType == SwitchArgument::VALUE)
          return caseElem.codeloc.getError(
              "Can only bind element to pointer when switch argument is a reference");
        if (realType == BuiltinType::VOID)
          return caseElem.codeloc.getError("Can't bind void element to pointer");
        caseBodyContext.addVariable(caseId, MutableReferenceType::get(PointerType::get(realType)), caseElem.codeloc);
      }
    }
    if (caseElem.varType == caseElem.VALUE) {
      if (argumentType != SwitchArgument::VALUE && !realType->isBuiltinCopyable(outsideContext))
        return caseElem.codeloc.getError(
            "Type " + quote(realType->getName()) + " is not implicitly copyable. "
            "Try binding to a pointer or move from the variable that you are switching on");
      caseBodyContext.addVariable(caseId, ReferenceType::get(realType), caseElem.codeloc);
    }
    TRY(caseElem.block->check(caseBodyContext));
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
    TRY(statement.defaultBlock->check(outsideContext));
  }
  return success;
}

WithError<SType> StructType::getTypeOfMember(const string& name) const {
  for (auto& member : (alternatives.empty() ? members : alternatives))
    if (member.name == name)
      return member.type;
  return "No " + (alternatives.empty() ? "member"s : "alternative"s) + " named " + quote(name) + " in type " + quote(getName());
}

shared_ptr<StructType> StructType::getInstance(vector<SType> newTemplateParams) {
  CHECK(newTemplateParams.size() == templateParams.size());
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
  type->definition = definition;
  instances.push_back(type);
  return type;
}

void StructType::updateInstantations() {
  for (auto type1 : copyOf(instances)) {
    auto type = type1.dynamicCast<StructType>();
    type->staticContext.deepCopyFrom(staticContext);
    type->alternatives = alternatives;
    type->members = members;
    type->destructor = destructor;
    ErrorBuffer errors;
    for (int i = 0; i < templateParams.size(); ++i) {
      type->staticContext.replace(templateParams[i], type->templateParams[i], errors);
      for (auto& alternative : type->alternatives)
        alternative.type = alternative.type->replace(templateParams[i], type->templateParams[i], errors);
      for (auto& member : type->members)
        member.type = member.type->replace(templateParams[i], type->templateParams[i], errors);
      if (destructor) {
        type->destructor = replaceInFunction(type->destructor.get(), templateParams[i], type->templateParams[i], errors);
        auto res = type->destructor->getDefinition()->addInstance(nullptr, type->destructor.get());
        CHECK(!!res) << res.get_error();
      }
    }
    CHECK(errors.empty());
  }
}

WithError<Type::MemberInfo> StructType::getTypeOfMember(const SType& v1) const {
  if (auto v = v1.dynamicCast<CompileTimeValue>()) {
    if (auto ref = v->value.getReferenceMaybe<CompileTimeValue::ReferenceValue>())
      v = ref->value;
    if (auto intValue = v->value.getValueMaybe<int>()) {
      auto& membersOrAlt = (alternatives.empty() ? members : alternatives);
      if (*intValue >= 0 && *intValue < membersOrAlt.size())
        return MemberInfo{membersOrAlt[*intValue].type, membersOrAlt[*intValue].name};
      else
        return (alternatives.empty() ? "Member"s : "Alternative"s) + " index for type "
            + quote(getName()) + " must be between 0 and " + to_string(alternatives.size() - 1) +
            ", got " + to_string(*intValue);
    }
    if (v->getType() == BuiltinType::INT)
      return MemberInfo{(SType)TemplateStructMemberType::get(this->get_this().get(), v), "bad_member_name"};
  }
  return "Member index must be of type: " + quote(BuiltinType::INT->getName()) + ", got " + v1->getType()->getName();
}

SType StructType::getType() const {
  return alternatives.empty() ? BuiltinType::STRUCT_TYPE : BuiltinType::VARIANT_TYPE;
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
  return BuiltinType::ANY_TYPE;
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
    if (to == BuiltinType::VOID)
      errors.push_back(
          "Can't instantiate member type with type " + quote(BuiltinType::VOID->getName()));
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
    if (destructor) {
      ret->destructor = destructor;
      ErrorBuffer errors;
      ret->destructor = replaceInFunction(ret->destructor.get(), from, to, errors);
      auto res = ret->destructor->getDefinition()->addInstance(nullptr, ret->destructor.get());
      CHECK(!!res) << res.get_error();
      CHECK(errors.empty());
    }
  } else
    INFO << "Found instantiated: " << ret->getName();
  return ret;
}

FunctionType replaceInFunction(FunctionType type, SType from, SType to, ErrorBuffer& errors) {
  if (type.parentType)
    type.parentType = type.parentType->replace(from, to, errors);
  type.retVal = type.retVal->replace(from, to, errors);
  for (auto& param : type.params)
    param = param->replace(from, to, errors);
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

JustError<ErrorLoc> Type::handleSwitchStatement(SwitchStatement& s, Context&, SwitchArgument) const {
  return s.codeLoc.getError("Can't switch on the value of type " + quote(getName()));
}

static string getCantSubstituteError(SType param, SType with) {
  auto ret = "Can't substitute template parameter " + quote(param->getName()) + " ";
  if (param->getType() != BuiltinType::ANY_TYPE)
    ret += "of type " + quote(param->getType()->getName()) + " ";
  ret += "with type " + quote(with->getName());
  return ret;
}

static JustError<ErrorLoc> checkRequirements(const Context& context, const vector<TemplateRequirement>& requirements, CodeLoc codeLoc,
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
  return success;
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
  TRY(checkRequirements(context, ret.dynamicCast<StructType>()->requirements, loc, {}));
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

static JustError<string> getDeductionError(const Context& context, TypeMapping& mapping, SType paramType, SType argType) {
  if (auto index = mapping.getParamIndex(paramType)) {
    auto& arg = mapping.templateArgs[*index];
    if (arg && arg != argType)
      return getCantBindError(argType, arg.get());
    arg = argType;
    return success;
  } else
    return paramType->getMappingError(context, mapping, argType);
}

JustError<string> StructType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  auto argStruct = argType.dynamicCast<StructType>();
  if (!argStruct || parent.get() != argStruct->parent.get())
    return "Can't bind type " + quote(argType->getName()) + " to struct type " + quote(getName());
  for (int i = 0; i < templateParams.size(); ++i)
    TRY(::getDeductionError(context, mapping, templateParams[i], argStruct->templateParams[i]));
  return success;
}

JustError<string> PointerType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<PointerType>())
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  return "Can't bind type " + quote(argType->getName()) + " to type " + quote(getName());
}

JustError<string> MutablePointerType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<MutablePointerType>())
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  return "Can't bind type " + quote(argType->getName()) + " to type " + quote(getName());
}

bool MutablePointerType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

SType MutablePointerType::removePointer() const {
  return underlying;
}

JustError<string> ReferenceType::getMappingError(const Context& context, TypeMapping& mapping, SType from) const {
  return ::getDeductionError(context, mapping, underlying, from->getUnderlying());
}

JustError<string> MutableReferenceType::getMappingError(const Context& context, TypeMapping& mapping, SType from) const {
  if (auto argRef = from.dynamicCast<MutableReferenceType>())
    return ::getDeductionError(context, mapping, underlying, argRef->underlying);
  else
    return getCantBindError(from, get_this().get());
}

JustError<string> Type::getMappingError(const Context&, TypeMapping&, SType argType) const {
  if (argType == get_this().get())
    return success;
  else
    return getCantBindError(argType, get_this().get());
}

static bool areParamsTypesEqual(const FunctionType& f1, const FunctionType& f2) {
  if (f1.params.size() != f2.params.size())
    return false;
  for (int i = 0; i < f1.params.size(); ++i)
    if (f1.params[i] != f2.params[i])
      return false;
  return true;
}

string getExpandedParamName(const string& packName, int index) {
  CHECK(index >= 0);
  return "_" + packName + to_string(index);
}

static JustError<ErrorLoc> expandVariadicTemplate(FunctionType& type, CodeLoc codeLoc, vector<SType> templateArgs,
    vector<SType> argTypes) {
  vector<SType> expandedTypes;
  nullable<SType> lastTemplateParam;
  if (!type.templateParams.empty())
    lastTemplateParam = type.templateParams.back();
  if (type.variadicTemplate) {
    type.templateParams.pop_back();
    nullable<SType> lastParam;
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
        type.params.push_back(lastParam->replace(lastTemplateParam.get(), type.templateParams.back(), errors));
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
      auto thisType = lastParam;
      if (type.variadicTemplate) {
        if (expandedTypes.size() <= cnt) {
          type.templateParams.push_back(shared<TemplateParameterType>(lastTemplateParam->getType(),
              getExpandedParamName(lastTemplateParam->getName(), cnt), codeLoc));
          expandedTypes.push_back(type.templateParams.back());
        }
        ErrorBuffer errors;
        thisType = lastParam->replace(lastTemplateParam.get(), type.templateParams.back(), errors);
        if (!errors.empty())
          return codeLoc.getError(errors[0]);
      }
      type.params.push_back(thisType);
      ++cnt;
    }
    type.variadicParams = false;
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
  return success;
}

void generateConversions(const Context& context, const vector<SType>& paramTypes, const vector<SType>& argTypes,
    vector<unique_ptr<Expression>>& expr) {
  for (int i = 0; i < paramTypes.size(); ++i) {
    auto& paramType = paramTypes[i];
    auto& argType = argTypes[i];
    if ((!paramType.dynamicCast<ReferenceType>() && !paramType.dynamicCast<MutableReferenceType>()) &&
        (argType.dynamicCast<ReferenceType>() || argType.dynamicCast<MutableReferenceType>())) {
      unique_ptr<Expression> emptyExpr;
      CHECK(argType->getUnderlying()->isBuiltinCopyable(context, expr[i]));
    }
  }
}

static JustError<ErrorLoc> checkImplicitCopies(const Context& context, const vector<SType>& paramTypes, vector<SType>& argTypes,
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
  return success;
}

static JustError<ErrorLoc> getConversionError(const Context& context, const SFunctionInfo& input, const vector<SType>& argTypes,
    const vector<CodeLoc>& argLoc, const vector<SType>& funParams, TypeMapping& mapping) {
  for (int i = 0; i < argTypes.size(); ++i) {
    optional<ErrorLoc> firstError;
    if (argTypes[i] != BuiltinType::NULL_TYPE || !funParams[i].dynamicCast<OptionalType>())
      for (auto tArg : context.getConversions(argTypes[i]))
        if (!input->id.contains<Operator>() || tArg == argTypes[i] ||
            (tArg.dynamicCast<BuiltinType>() && argTypes[i].dynamicCast<BuiltinType>())) {
          if (auto res = getDeductionError(context, mapping, funParams[i], tArg); !res) {
            if (!firstError)
              firstError = argLoc[i].getError(res.get_error());
          } else {
            firstError = none;
            break;
          }
        }
    if (firstError)
      return *firstError;
  }
  return success;
}

WithErrorLine<SFunctionInfo> instantiateFunction(const Context& context, const SFunctionInfo& input, CodeLoc codeLoc,
    vector<SType> templateArgs, vector<SType> argTypes, vector<CodeLoc> argLoc, vector<FunctionType> existing) {
  FunctionType type = input->type;
  TRY(expandVariadicTemplate(type, codeLoc, templateArgs, argTypes));
  if (type.params.size() != argTypes.size())
    return codeLoc.getError("Wrong number of function arguments. Expected " +
        to_string(type.params.size()) + " got " + to_string(argTypes.size()));
  auto implicitCopySuccess = checkImplicitCopies(context, type.params, argTypes, argLoc);
  if (templateArgs.size() > type.templateParams.size())
    return codeLoc.getError("Too many template arguments.");
  TypeMapping mapping { type.templateParams, vector<nullable<SType>>(type.templateParams.size()) };
  for (int i = 0; i < templateArgs.size(); ++i)
    mapping.templateArgs[i] = templateArgs[i];
  TRY(getConversionError(context, input, argTypes, argLoc, type.params, mapping));
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
  TRY(checkRequirements(context, type.requirements, codeLoc, existing));
  // The replace() errors need to be checked after returning potential requirement errors.
  if (!errors.empty())
    return codeLoc.getError(errors[0]);
  if (!implicitCopySuccess)
    return implicitCopySuccess.get_error();
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

JustError<string> ArrayType::getSizeError(const Context& context) const {
  return underlying->getSizeError(context);
}

ArrayType::ArrayType(SType type, SCompileTimeValue size) : size(std::move(size)), underlying(type) {
}

JustError<string> ArrayType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
  if (auto argPointer = argType.dynamicCast<ArrayType>()) {
    TRY(::getDeductionError(context, mapping, size, argPointer->size));
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
      [](int)-> SType {  return BuiltinType::INT; },
      [](double)-> SType {  return BuiltinType::DOUBLE; },
      [](bool)-> SType {  return BuiltinType::BOOL; },
      [](char)-> SType {  return BuiltinType::CHAR; },
      [](const ReferenceValue& ref)-> SType {  return MutableReferenceType::get(ref.value->getType()); },
      [](NullValue)-> SType {  return BuiltinType::NULL_TYPE; },
      [](const string&)-> SType {  return BuiltinType::STRING; },
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

JustError<string> CompileTimeValue::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
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
          return *ret;
        else {
          errors.push_back("Can't evaluate operator " + quote(getString(value.op)));
          return get_this().get();
        }
      },
      [&](const TemplateFunctionCall& value) {
        if (auto ret = value.functionInfo.invokeFunction(value.name, value.loc, transform(value.args,
            [&](const SType& t){ return t->replace(from, to, errors); }), value.argLoc))
          return *ret;
        else {
          errors.push_back(ret.get_error().error);
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

JustError<string> SliceType::getMappingError(const Context& context, TypeMapping& mapping, SType argType) const {
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

bool SliceType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
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

JustError<string> OptionalType::getMappingError(const Context& context, TypeMapping& mapping, SType from) const {
  if (auto argPointer = from.dynamicCast<OptionalType>())
    return ::getDeductionError(context, mapping, underlying, argPointer->underlying);
  return "Can't bind type " + quote(from->getName()) + " to type " + quote(getName());
}

SType OptionalType::replaceImpl(SType from, SType to, ErrorBuffer& errors) const {
  return OptionalType::get(underlying->replace(from, to, errors));
}

void OptionalType::codegenDefinitionImpl(set<const Type*>& visited, Accu& accu) const {
  underlying->codegenDefinition(visited, accu);
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
  string suf;
  for (auto& arg : templateParams)
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
  vector<SType> newTemplateParams;
  for (auto& param : templateParams)
    newTemplateParams.push_back(param->replace(from, to, errors));
  auto ret = get(name, newTemplateParams);
  if (!ret->body) { // this is how we check that this is a newly created lambda
    ret->body = cast<StatementBlock>(body->replace(from, to, errors2));
    for (auto& e : errors2)
      errors.push_back(e.error);
    auto tmpType = functionInfo->type;
    tmpType.params = getSubsequence(tmpType.params, 1);
    tmpType = replaceInFunction(tmpType, from, to, errors);
    tmpType.params = concat({PointerType::get(ret)}, tmpType.params);
    ret->functionInfo = FunctionInfo::getImplicit(functionInfo->id, std::move(tmpType));
    for (auto& capture : captures)
      ret->captures.push_back(LambdaCapture{capture.name, capture.type->replace(from, to, errors), capture.captureType});
    ret->parameterNames = parameterNames;
  }
  return ret;
}

JustError<string> LambdaType::getMappingError(const Context&, TypeMapping&, SType argType) const {
  return success;
}

static int getNewLambdaId() {
  static int allIds = 0;
  return ++allIds;
}

shared_ptr<LambdaType> LambdaType::get(string name, vector<SType> templateParams) {
  auto key = make_pair(std::move(name), std::move(templateParams));
  static map<decltype(key), shared_ptr<LambdaType>> generated;
  if (!generated.count(key)) {
    auto ret = shared<LambdaType>(Private{});
    ret->name = key.first;
    ret->templateParams = key.second;
    generated.insert({key, ret});
  }
  return generated.at(key);
}

shared_ptr<LambdaType> LambdaType::get(vector<SType> templateParams) {
  return get("LAMBDA" + to_string(getNewLambdaId()), std::move(templateParams));
}

LambdaType::LambdaType(Private) {
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

nullable<SType> convertPointerToReferenceStrict(SType type) {
  if (auto p = type.dynamicCast<PointerType>())
    return SType(ReferenceType::get(p->underlying));
  else if (auto p = type.dynamicCast<MutablePointerType>())
    return SType(MutableReferenceType::get(p->underlying));
  else
    return nullptr;
}

nullable<SType> convertReferenceToPointerStrict(SType type) {
  if (auto p = type.dynamicCast<ReferenceType>())
    return SType(PointerType::get(p->underlying));
  else if (auto p = type.dynamicCast<MutableReferenceType>())
    return SType(MutablePointerType::get(p->underlying));
  else
    return nullptr;
}

SType convertPointerToReference(SType type) {
  if (auto p = convertPointerToReferenceStrict(type))
    return p.get();
  else
    return type;
}

SType convertReferenceToPointer(SType type) {
  if (auto p = convertReferenceToPointerStrict(type))
    return p.get();
  else
    return type;
}
