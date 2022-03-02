#include "type.h"
#include "context.h"
#include "ast.h"

BuiltinType::DefType BuiltinType::INT = new BuiltinType("int");
BuiltinType::DefType BuiltinType::DOUBLE = new BuiltinType("double");
BuiltinType::DefType BuiltinType::LONG = new BuiltinType("long", "long long"s);
BuiltinType::DefType BuiltinType::VOID = new BuiltinType("void", "void_t"s);
BuiltinType::DefType BuiltinType::BOOL = new BuiltinType("bool");
BuiltinType::DefType BuiltinType::STRING = new BuiltinType("string", "zenon_string"s);
BuiltinType::DefType BuiltinType::CHAR = new BuiltinType("char");
BuiltinType::DefType BuiltinType::NORETURN = new BuiltinType("noreturn", "[[noreturn]] void"s);
BuiltinType::DefType BuiltinType::ANY_TYPE = new BuiltinType("any_type");
BuiltinType::DefType BuiltinType::ANYTHING = new BuiltinType("anything");
BuiltinType::DefType BuiltinType::ENUM_TYPE = new BuiltinType("enum_type");
BuiltinType::DefType BuiltinType::NULL_TYPE = new BuiltinType("null_type");
BuiltinType::DefType BuiltinType::STRUCT_TYPE = new BuiltinType("struct_type");
BuiltinType::DefType BuiltinType::UNION_TYPE = new BuiltinType("union_type");
BuiltinType::DefType BuiltinType::ATTRIBUTE_TYPE = new BuiltinType("attribute_type");
BuiltinType::DefType BuiltinType::CONCEPT_TYPE = new BuiltinType("concept_type");

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
  if (auto t = dynamic_cast<ConceptType*>(underlying))
    return "const_fat_ref<" + *t->getMangledName() + "_vtable>";
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
  if (auto t = dynamic_cast<ConceptType*>(underlying))
    return "fat_ref<" + *t->getMangledName() + "_vtable>";
  return underlying->getCodegenName() + "&";
}

string PointerType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "*";
}

string PointerType::getCodegenName() const {
  if (auto t = dynamic_cast<ConceptType*>(underlying))
    return "const_fat_ptr<" + *t->getMangledName() + "_vtable>";
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
  if (auto t = dynamic_cast<ConceptType*>(underlying))
    return "fat_ptr<" + *t->getMangledName() + "_vtable>";
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
  if (auto suf = mangleTemplateParams(templateParams))
    return getName(false) + *suf;
  return none;
}

string TemplateParameterType::getName(bool withTemplateArguments) const {
  return name;
}

bool TemplateParameterType::canReplaceBy(Type* t) const {
  return !t->isMetaType() && ((type == BuiltinType::ANY_TYPE && t->getType()->isMetaType())
      || type == BuiltinType::ANYTHING || t->getType() == type);
}

optional<string> TemplateParameterType::getMangledName() const {
  return none;
}

Type* TemplateParameterType::getType() const {
  return type;
}

bool TemplateParameterType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  if (type == BuiltinType::ENUM_TYPE)
    return true;
  else
    return false;
}

WithError<Type*> TemplateParameterType::getTypeOfMember(const string& s, ArgumentType arg) const {
  if (type == BuiltinType::STRUCT_TYPE)
    return "Can only refer to template struct type members by name"s;
  return Type::getTypeOfMember(s, arg);
}

bool TemplateParameterType::canBeValueTemplateParam() const {
  return true;
}

TemplateParameterType::TemplateParameterType(string n, CodeLoc l) : name(n), declarationLoc(l), type(BuiltinType::ANY_TYPE) {}

TemplateParameterType::TemplateParameterType(Type* type, string name, CodeLoc l)
    : name(std::move(name)), declarationLoc(l), type(std::move(type)) {
}

string EnumType::getName(bool withTemplateArguments) const {
  return name;
}

JustError<ErrorLoc> EnumType::handleSwitchStatement(SwitchStatement& statement, Context& context, ArgumentType argType) const {
  statement.type = SwitchStatement::ENUM;
  statement.targetType = (Type*)this;
  unordered_set<string> handledElems;
  for (auto& caseElem : statement.caseElems) {
    for (auto& id : caseElem.ids) {
      if (!contains(elements, id))
        return caseElem.codeloc.getError("Element " + quote(id) + " not present in enum " + quote(name));
      if (handledElems.count(id))
        return caseElem.codeloc.getError("Enum element " + quote(id)
            + " handled more than once in switch statement");
      handledElems.insert(id);
    }
    auto caseContext = context.getChild();
    caseContext.setIsInBranch();
    TRY(caseElem.block->check(caseContext));
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
    auto caseContext = context.getChild();
    caseContext.setIsInBranch();
    TRY(statement.defaultBlock->check(caseContext));
  }
  return success;
}

FunctionSignature::FunctionSignature(Type* returnType, vector<Type*> p, vector<Type*> tpl)
  : retVal(std::move(returnType)), params(std::move(p)), templateParams(std::move(tpl)) {
}

FunctionSignature FunctionSignature::setBuiltin() {
  builtinOperator = true;
  return *this;
}

FunctionInfo* FunctionInfo::getDefined(FunctionId id, FunctionSignature type, FunctionDefinition* definition) {
  auto args = make_tuple(id, type, definition);
  static unordered_map<decltype(args), FunctionInfo*> generated;
  if (!generated.count(args)) {
    generated.insert(make_pair(args, new FunctionInfo(Private{}, id, type, definition)));
  }
  return generated.at(args);
}

FunctionInfo* FunctionInfo::getImplicit(FunctionId id, FunctionSignature type) {
  auto args = make_tuple(id, type);
  static unordered_map<decltype(args), FunctionInfo*> generated;
  if (!generated.count(args)) {
    generated.insert(make_pair(args, new FunctionInfo(Private{}, id, type, (FunctionInfo*)nullptr)));
  }
  return generated.at(args);
}

FunctionInfo* FunctionInfo::getInstance(FunctionId id, FunctionSignature type, FunctionInfo* parent) {
  if (id == parent->id && type == parent->type)
    return parent;
  while (!!parent->parent)
    parent = parent->parent;
  auto args = make_tuple(id, type, parent);
  static unordered_map<decltype(args), FunctionInfo*> generated;
  if (!generated.count(args)) {
    auto ret = new FunctionInfo(Private{}, id, type, parent);
    generated.insert(make_pair(args, std::move(ret)));
  }
  return generated.at(args);
}

const string& FunctionInfo::prettyString() const {
  return pretty;
}

string FunctionInfo::getMangledName() {
  if (isMainFunction())
    return "zenonMain"s;
  if (id == "copy"s || id == "implicit_copy"s)
    return id.get<string>();
  return id.visit(
      [this](const string& s) {
        if (type.builtinOperator)
          return s;
        auto suf = getMangledSuffix();
        CHECK(!!suf) << prettyString();
        return s + *suf;
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
          return "construct_" + *type.retVal->getMangledName();
      },
      [](auto&) -> string {
        fail();
      }
  );
}

bool FunctionInfo::isMainFunction() const {
  return id == "main"s;
}

bool FunctionInfo::isConceptTypeFunction() const {
  if (!type.concept)
    return false;
  for (auto& p : type.params)
    if (dynamic_cast<ConceptType*>(p->removePointer()))
      return true;
  return !!dynamic_cast<ConceptType*>(type.retVal->removePointer());
}

optional<string> FunctionInfo::getMangledSuffix() {
  if (type.concept)
    return "C" + type.concept->getName(false);
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
  if (id == "invoke"s)
    return ""s;
  if (!type.builtinOperator)
    if (auto def = getParent()->definition)
      suf += to_string(def->codeLoc.line);
  return suf;
}

optional<string> FunctionInfo::getParamName(int index, const FunctionDefinition* def) const {
  if ((index < def->parameters.size() && (!def->isVariadicParams || type.variadicParams)) || index < def->parameters.size() - 1)
    return def->parameters[index].name;
  else if (auto& name = def->parameters.back().name)
    return getExpandedParamName(*name, index - def->parameters.size() + 1);
  return none;
}

FunctionInfo* FunctionInfo::getWithoutRequirements() {
  if (!noRequirements) {
    if (type.requirements.empty())
      noRequirements = this;
    else {
      auto newType = type;
      newType.requirements.clear();
      noRequirements = getInstance(id, std::move(newType), getParent());
    }
  }
  return noRequirements;
}

FunctionInfo* FunctionInfo::getParent() {
  if (parent)
    return parent;
  else
    return this;
}

FunctionDefinition* FunctionInfo::getDefinition() {
  return getParent()->definition;
}

JustError<ErrorLoc> FunctionInfo::addInstance(const Context& context) {
  if (auto def = getDefinition())
    return def->addInstance(context, this);
  return success;
}

void FunctionInfo::genPrettyString() {
  pretty = type.retVal->getName() + " " + toString(id) + joinTemplateParams(type.templateParams, type.variadicTemplate) + "(" +
      combine(transform(type.params, [](auto& t) { return t->getName(); }), ", ") +
      (type.variadicParams ? "...)" : ")") +
      (!!type.concept ? " [from concept]" : "") +
      (parent && parent->definition ? parent->definition->codeLoc.toString() : "") +
      (definition ? definition->codeLoc.toString() : "");
}

FunctionInfo::FunctionInfo(FunctionInfo::Private, FunctionId id, FunctionSignature type, FunctionInfo* parent)
  : id(std::move(id)), type(std::move(type)), parent(std::move(parent)) {
  genPrettyString();
}

FunctionInfo::FunctionInfo(FunctionInfo::Private, FunctionId id, FunctionSignature type, FunctionDefinition* definition)
  : id(std::move(id)), type(std::move(type)), definition(definition) {
  genPrettyString();
}

Type::Type() : staticContext(nullptr) {
}

string Type::getCodegenName() const {
  return getName();
}

optional<string> Type::getMangledName() const {
  return getCodegenName();
}

Type* Type::removeReference() {
  return this;
}

bool Type::isPointer() {
  return removePointer() != this;
}

bool Type::isReference() {
  return removeReference() != this;
}

Type* Type::removeValueReference() {
  auto type = getType();
  if (type->isReference())
    return *convertTo(type->removeReference());
  return this;
}

Type* ReferenceType::removeReference() {
  return underlying;
}

Type* MutableReferenceType::removeReference() {
  return underlying;
}

ReferenceType* ReferenceType::get(Type* type) {
  type = type->removeReference();
  static unordered_map<Type*, ReferenceType*> generated;
  if (!generated.count(type)) {
    auto ret = new ReferenceType(type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

ReferenceType::ReferenceType(Type* t) : underlying(t->removeReference()) {
}

MutableReferenceType* MutableReferenceType::get(Type* type) {
  type = type->removeReference();
  static unordered_map<Type*, MutableReferenceType*> generated;
  if (!generated.count(type)) {
    auto ret = new MutableReferenceType(Private{}, type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

MutableReferenceType::MutableReferenceType(Private, Type* t) : underlying(t) {
}

PointerType* PointerType::get(Type* type) {
  type = type->removeReference();
  static unordered_map<Type*, PointerType*> generated;
  if (!generated.count(type)) {
    auto ret = new PointerType(Private{}, type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

PointerType::PointerType(Private, Type* t) : underlying(t) {
}

MutablePointerType* MutablePointerType::get(Type* type) {
  type = type->removeReference();
  static unordered_map<Type*, MutablePointerType*> generated;
  if (!generated.count(type)) {
    auto ret = new MutablePointerType(Private{}, type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

MutablePointerType::MutablePointerType(Private, Type* t) : underlying(t) {
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
  return INT == this || LONG == this || BOOL == this || CHAR == this || STRING == this || DOUBLE == this || VOID == this;
}

bool BuiltinType::isMetaType() const {
  return ANY_TYPE == this || STRUCT_TYPE == this || UNION_TYPE == this || ENUM_TYPE == this
      || ATTRIBUTE_TYPE == this || CONCEPT_TYPE == this;
}

Type* Type::removePointer() {
  return this;
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

WithError<Type*> Type::convertTo(Type* t) {
  return "Can't convert " + quote(getName()) + " to type " + quote(t->getName());
}

JustError<string> Type::isBuiltinCopyable(const Context& context, unique_ptr<Expression>& expr) {
  if (isBuiltinCopyableImpl(context, expr))
    return success;
  else
  if (auto fun = getImplicitCopyFunction(context, CodeLoc(), this)) {
    if (expr) {
      auto tmpContext = context.getChild();
      auto codeLoc = expr->codeLoc;
      expr = unique<FunctionCall>(IdentifierInfo("implicit_copy", codeLoc),
          unique<UnaryExpression>(codeLoc, Operator::GET_ADDRESS, std::move(expr)), false);
      if (auto res = expr->getTypeImpl(tmpContext); !res)
        return res.get_error().error;
    }
    return success;
  }
  return "Type " + quote(getName()) + " cannot be implicitly copied";
}

bool Type::isBuiltinCopyable(const Context& context) {
  unique_ptr<Expression> expr;
  return !!isBuiltinCopyable(context, expr);
}

bool PointerType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

Type* PointerType::removePointer() {
  return underlying;
}

bool EnumType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

Type* EnumType::getType() const {
  return BuiltinType::ENUM_TYPE;
}

static JustError<string> checkMembers(const Context& context, set<const Type*> &visited, const Type* t,
    bool onlyIncomplete) {
  if (auto s = dynamic_cast<const StructType*>(t)) {
    if (!onlyIncomplete && visited.count(t))
      return "Type " + quote(t->getName()) + " has infinite size"s;
    if (!context.isFullyDefined(t))
      return "Type " + quote(t->getName()) + " is incomplete in this context"s;
    visited.insert(t);
    for (auto& member : s->members)
      TRY(checkMembers(context, visited, member.type, onlyIncomplete));
    for (auto& member : s->alternatives)
      TRY(checkMembers(context, visited, member.type, onlyIncomplete));
    for (auto index : s->parent->memberTemplateParams) {
      auto param = s->templateParams[index];
      if (!visited.count(param))
        TRY(checkMembers(context, visited, param, true));
    }
    visited.erase(t);
  }
  return success;
}

JustError<string> StructType::getSizeError(const Context& context) const {
  if (context.isTemplateInstance())
    return success;
  set<const Type*> visited;
  return checkMembers(context, visited, this, false);
}

JustError<ErrorLoc> ReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, ArgumentType) const {
  return underlying->handleSwitchStatement(statement, context, ArgumentType::REFERENCE);
}

WithError<Type*> ReferenceType::getTypeOfMember(const string& name, ArgumentType) const {
  return underlying->getTypeOfMember(name, ArgumentType::REFERENCE);
}

Type* ReferenceType::removePointer() {
  return underlying->removePointer();
}

JustError<ErrorLoc> MutableReferenceType::handleSwitchStatement(SwitchStatement& statement, Context& context, ArgumentType) const {
  return underlying->handleSwitchStatement(statement, context, ArgumentType::MUTABLE_REFERENCE);
}

WithError<Type*> MutableReferenceType::getTypeOfMember(const string& name, ArgumentType) const {
  return underlying->getTypeOfMember(name, ArgumentType::MUTABLE_REFERENCE);
}

Type* MutableReferenceType::removePointer() {
  return underlying->removePointer();
}

JustError<ErrorLoc> StructType::handleSwitchStatement(SwitchStatement& statement, Context& outsideContext,
    ArgumentType argumentType) const {
  if (alternatives.empty())
    return statement.codeLoc.getError("Can't switch on a struct type");
  statement.type = SwitchStatement::UNION;
  statement.targetType = (Type*)this;
  unordered_set<string> handledTypes;
  auto getAlternativeType = [&] (const string& name) -> Type* {
    for (auto& alternative : alternatives)
      if (alternative.name == name)
        return alternative.type;
    return nullptr;
  };
  for (auto& caseElem : statement.caseElems) {
    if (caseElem.ids.size() > 1)
      return caseElem.codeloc.getError("Multiple case elements not allowed in a union switch");
    auto caseId = caseElem.ids[0];
    if (!getAlternativeType(caseId))
      return caseElem.codeloc.getError("Element " + quote(caseId) +
          " not present in union " + quote(getName()));
    if (handledTypes.count(caseId))
      return caseElem.codeloc.getError("Union member " + quote(caseId)
        + " handled more than once in switch statement");
    handledTypes.insert(caseId);
    auto caseBodyContext = outsideContext.getChild();
    auto realType = getAlternativeType(caseId);
    caseElem.declaredVar = caseId;
    switch (argumentType) {
      case ArgumentType::VALUE:
        caseBodyContext.addVariable(caseId, ReferenceType::get(realType), caseElem.codeloc);
        break;
      case ArgumentType::REFERENCE:
        caseBodyContext.addVariable(caseId, ReferenceType::get(realType), caseElem.codeloc);
        caseBodyContext.setNonMovable(caseId);
        break;
      case ArgumentType::MUTABLE_REFERENCE:
        caseBodyContext.addVariable(caseId, MutableReferenceType::get(realType), caseElem.codeloc);
        caseBodyContext.setNonMovable(caseId);
        break;
    }
    caseBodyContext.setIsInBranch();
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
          "Default switch statement unnecessary when all union members are handled");
    TRY(statement.defaultBlock->check(outsideContext));
  }
  return success;
}

static Type* modifyMemberType(Type* t, Type::ArgumentType argType, bool hasDestructor, bool constMember) {
  if (!!hasDestructor && argType == Type::ArgumentType::VALUE)
    argType = Type::ArgumentType::REFERENCE;
  if (constMember && argType == Type::ArgumentType::MUTABLE_REFERENCE)
    argType = Type::ArgumentType::REFERENCE;
  switch (argType) {
    case Type::ArgumentType::VALUE:
      return t;
    case Type::ArgumentType::REFERENCE:
      return ReferenceType::get(t);
    case Type::ArgumentType::MUTABLE_REFERENCE:
      return MutableReferenceType::get(t);
  }
}

WithError<Type*> StructType::getTypeOfMember(const string& name, ArgumentType argType) const {
  for (auto& member : (alternatives.empty() ? members : alternatives))
    if (member.name == name)
      return modifyMemberType(member.type, argType, !!destructor, member.isConst);
  return "No " + (alternatives.empty() ? "member"s : "alternative"s) + " named " + quote(name) + " in type " + quote(getName());
}

bool StructType::hasDestructor() const {
  if (!!destructor)
    return true;
  for (auto& m : members)
    if (m.type->hasDestructor())
      return true;
  for (auto& m : alternatives)
    if (m.type->hasDestructor())
      return true;
  return false;
}

StructType* StructType::getInstance(vector<Type*> newTemplateParams) {
  CHECK(newTemplateParams.size() == templateParams.size());
  if (templateParams == newTemplateParams)
    return this;
  for (auto type : instances) {
    if (type->templateParams == newTemplateParams)
      return type;
  }
  auto type = new StructType(name, Private{});
  type->alternatives = alternatives;
  type->members = members;
  type->parent = this;
  type->external = external;
  type->definition = definition;
  instances.push_back(type);
  return type;
}

void StructType::updateInstantations(const Context& context) {
  for (auto type1 : copyOf(instances)) {
    auto type = dynamic_cast<StructType*>(type1);
    type->staticContext.deepCopyFrom(staticContext);
    type->alternatives = alternatives;
    type->members = members;
    type->destructor = destructor;
    ErrorBuffer errors;
    for (int i = 0; i < templateParams.size(); ++i) {
      type->staticContext.replace(context, templateParams[i], type->templateParams[i], errors);
      for (auto& alternative : type->alternatives)
        alternative.type = alternative.type->replace(context, templateParams[i], type->templateParams[i], errors);
      for (auto& member : type->members)
        member.type = member.type->replace(context, templateParams[i], type->templateParams[i], errors);
      if (destructor) {
        type->destructor = replaceInFunction(context, type->destructor, templateParams[i],
            type->templateParams[i], errors);
      }
    }
    CHECK(errors.empty()) << "While updating " << type1->getName() << ": " << errors[0];
  }
}

Type* StructType::getType() const {
  return alternatives.empty() ? BuiltinType::STRUCT_TYPE : BuiltinType::UNION_TYPE;
}

StructType::StructType(string name, StructType::Private) : name(std::move(name)) {
}

Type* Type::replace(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  auto self = this;
  if (from == self) {
    if (!canReplaceBy(to))
      errors.push_back("Can't substitute type " + from->getName() + " by " + to->getName());
    return to;
  } else
    return replaceImpl(context, from, to, errors);
}

bool Type::canReplaceBy(Type*) const {
  return false;
}

Type* Type::transform(function<Type*(Type*)>) {
  return this;
}

Type* Type::replaceImpl(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  return transform([&](Type* t) { return t->replace(context, from, to, errors); });
}

Type* Type::expand(const Context& context, Type* from, vector<Type*> to, ErrorBuffer& errors) {
  return transform([&](Type* t) { return t->expand(context, from, to, errors); });
}

Type* Type::getType() const {
  return BuiltinType::ANY_TYPE;
}

WithError<Type*> Type::getTypeOfMember(const string&, ArgumentType) const {
  return "Type " + quote(getName()) + " doesn't support dot operator"s;
}

bool Type::hasDestructor() const {
  return false;
}

Type* ReferenceType::transform(function<Type*(Type*)> fun) {
  return ReferenceType::get(fun(underlying));
}

Type* MutableReferenceType::transform(function<Type*(Type*)> fun) {
  return MutableReferenceType::get(fun(underlying));
}

Type* PointerType::transform(function<Type*(Type*)> fun) {
  return PointerType::get(fun(underlying));
}

Type* MutablePointerType::transform(function<Type*(Type*)> fun) {
  return MutablePointerType::get(fun(underlying));
}

namespace {
struct RequirementVisitor {
  const Context& context;
  Type* from = nullptr;
  Type* to = nullptr;
  ErrorBuffer& errors;
  void operator()(SConcept& concept) {
    concept = concept->replace(context, from, to, errors);
  }
  void operator()(shared_ptr<Expression>& expr) {
    ErrorLocBuffer errors2;
    auto res = expr->eval(context);
    if (!res) {
      if (!res.get_error().canEval)
        errors.push_back("Cannot evaluate expression at compile time ");
      else
        errors.push_back("Error evaluating expression: " + res.get_error().error);
    } else
    if (res->isConstant)
      if (auto cValue = res->value->asCompileTimeValue())
      if (auto value = cValue->value.getValueMaybe<bool>())
        if (!*value)
          errors.push_back(expr->codeLoc.toString() + ": Requirement evaluates to false");
    if (!errors2.empty())
      errors.push_back(errors2[0].error);
  }
};
}

Type* StructType::replaceImpl(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  vector<Type*> newTemplateParams;
  for (auto& param : templateParams)
    newTemplateParams.push_back(param->replace(context, from, to, errors));
  auto ret = parent->getInstance(newTemplateParams);
  // This is how we check if instantiate gave us a new type to fill
  if (ret->templateParams != newTemplateParams) {
    ret->templateParams = newTemplateParams;
    ret->staticContext.deepCopyFrom(staticContext);
    ret->alternatives = alternatives;
    for (auto& alternative : ret->alternatives) {
      alternative.type = alternative.type->replace(context, from, to, errors);
    }
    ret->members = members;
    for (auto& members : ret->members) {
      members.type = members.type->replace(context, from, to, errors);
    }
    ret->requirements = requirements;
    auto reqContext = context.getChild();
    for (int i = 0; i < newTemplateParams.size(); ++i)
      reqContext.addType(parent->templateParams[i]->getName(), newTemplateParams[i]);
    for (auto& req : ret->requirements)
      req.base.visit(RequirementVisitor{reqContext, from, to, errors});
    ret->staticContext.replace(context, from, to, errors);
    if (destructor) {
      // We'll ignore errors coming from destructor requirements, they'll pop up when trying
      // to use the destructor.
      ErrorBuffer destErrors;
      ret->destructor = destructor;
      ret->destructor = replaceInFunction(context, ret->destructor, from, to, destErrors);
      CHECK(ret->destructor->type.params[0] == PointerType::get(ret));
    }
  }
  return std::move(ret);
}

Type* StructType::expand(const Context& context, Type* pack, vector<Type*> to, ErrorBuffer& errors) {
  vector<Type*> newTemplateParams;
  for (auto& param : templateParams)
    newTemplateParams.push_back(param->expand(context, pack, to, errors));
  auto ret = parent->getInstance(newTemplateParams);
  // This is how we check if instantiate gave us a new type to fill
  if (ret->templateParams != newTemplateParams) {
    ret->templateParams = newTemplateParams;
    ret->staticContext.deepCopyFrom(staticContext);
    ret->alternatives = alternatives;
    for (auto& alternative : ret->alternatives) {
      alternative.type = alternative.type->expand(context, pack, to, errors);
    }
    ret->members = members;
    for (auto& members : ret->members) {
      members.type = members.type->expand(context, pack, to, errors);
    }
    ret->requirements = requirements;
    /*for (auto& req : ret->requirements)
      req.base.visit(RequirementVisitor{from, to, errors});*/
    ret->staticContext.expand(pack, to, errors);
    /*if (destructor) {
      ret->destructor = destructor;
      ret->destructor = replaceInFunction(ret->destructor.get(), from, to, errors);
      CHECK(ret->destructor->type.params[0] == PointerType::get(ret));
    }*/
  }
  return std::move(ret);
}

FunctionSignature replaceInFunction(const Context& context, FunctionSignature type, Type* from, Type* to, ErrorBuffer& errors,
    const vector<Type*>& origParams) {
  if (type.parentType)
    type.parentType = type.parentType->replace(context, from, to, errors);
  type.retVal = type.retVal->replace(context, from, to, errors);
  for (auto& param : type.params)
    param = param->replace(context, from, to, errors);
  for (auto& param : type.templateParams)
    param = param->replace(context, from, to, errors);
  auto reqContext = context.getChild();
  for (int i = 0; i < origParams.size(); ++i) {
    if (i == origParams.size() - 1 && type.variadicTemplate)
      reqContext.addExpandedTypePack(origParams[i]->getName(),
          type.templateParams.getSubsequence(i, type.templateParams.size() - 1));
    else
      reqContext.addType(origParams[i]->getName(), type.templateParams[i]);
  }
  for (auto& req : type.requirements)
    req.base.visit(RequirementVisitor{reqContext, from, to, errors});
  return type;
}

FunctionInfo* replaceInFunction(const Context& context, FunctionInfo* fun, Type* from, Type* to,
    ErrorBuffer& errors) {
  return FunctionInfo::getInstance(fun->id, replaceInFunction(context, fun->type, from, to, errors,
      fun->getParent()->type.templateParams), fun);
}

FunctionInfo* addTemplateParams(FunctionInfo* fun, vector<Type*> params, bool variadic) {
  auto type = fun->type;
  CHECK(type.templateParams.empty());
  type.templateParams = params;
  type.variadicTemplate = variadic;
  return FunctionInfo::getInstance(fun->id, type, fun);
}

WithErrorLine<Type*> Type::instantiate(const Context& context, vector<Type*> templateArgs, CodeLoc loc) {
  if (templateArgs.empty())
    return this;
  else
    return loc.getError("Type " + quote(getName()) + " is not a template");
}

Context& Type::getStaticContext() {
  return staticContext;
}

JustError<ErrorLoc> Type::handleSwitchStatement(SwitchStatement& s, Context&, ArgumentType) const {
  return s.codeLoc.getError("Can't switch on the value of type " + quote(getName()));
}

static string getCantSubstituteError(Type* param, Type* with) {
  auto ret = "Can't substitute template parameter " + quote(param->getName()) + " ";
  if (param->getType() != BuiltinType::ANY_TYPE)
    ret += "of type " + quote(param->getType()->getName()) + " ";
  ret += "with type " + quote(with->getName());
  return ret;
}

static JustError<ErrorLoc> checkRequirements(const Context& context,
    const vector<TemplateRequirement>& requirements, CodeLoc codeLoc,
    vector<FunctionSignature> existing) {
  for (auto& req : requirements)
    if (auto concept = req.base.getReferenceMaybe<SConcept>())
      TRY(context.getRequiredFunctions(**concept, existing).addCodeLoc(codeLoc));
    else if (auto expr1 = req.base.getReferenceMaybe<shared_ptr<Expression>>()) {
      if (expr1->get()->eval(context)->value == CompileTimeValue::get(false))
        return expr1->get()->codeLoc.getError("Predicate evaluates to false");
    }
  return success;
}

WithErrorLine<Type*> StructType::instantiate(const Context& context, vector<Type*> templateArgs, CodeLoc loc) {
  if (parent != this)
    return this;
  if (templateArgs.size() != templateParams.size())
    return loc.getError("Wrong number of template parameters for type " + getName());
  auto ret = this;
  ErrorBuffer errors;
  auto reqContext = context.getChild();
  for (int i = 0; i < templateParams.size(); ++i)
    reqContext.addType(templateParams[i]->getName(), templateArgs[i]);
  for (int i = 0; i < templateParams.size(); ++i) {
    if (!ret->templateParams[i]->canReplaceBy(templateArgs[i]))
      return loc.getError(getCantSubstituteError(templateParams[i], templateArgs[i]));
    ret = dynamic_cast<StructType*>(ret->replace(reqContext, ret->templateParams[i], templateArgs[i], errors));
  }
  for (auto& arg : templateArgs)
    if (dynamic_cast<CompileTimeValue*>(arg) && !arg->getType()->removeReference()->canBeValueTemplateParam())
      return loc.getError("Value template parameter cannot have type " + quote(arg->getType()->getName()));
  TRY(checkRequirements(reqContext, (ret)->requirements, loc, {}));
  if (!errors.empty())
    return loc.getError(errors[0]);
  return (Type*) ret;
}

struct TypeMapping {
  const vector<Type*>& templateParams;
  vector<Type*> templateArgs;
  optional<int> getParamIndex(Type* t) {
    for (int i = 0; i < templateParams.size(); ++i)
      if (templateParams[i] == t)
        return i;
    return none;
  }
};

static MappingError getCantBindError(const Type* from, const Type* to) {
  return MappingError { from, to};
}

static JustError<MappingError> getDeductionError(TypeMapping& mapping, Type* paramType, Type* argType) {
  if (auto index = mapping.getParamIndex(paramType)) {
    auto& arg = mapping.templateArgs[*index];
    if (arg && arg != argType)
      return getCantBindError(argType, arg);
    arg = argType;
    return success;
  } else
    return paramType->getMappingError(mapping, argType);
}

JustError<MappingError> StructType::getMappingError(TypeMapping& mapping, Type* argType) const {
  auto argStruct = argType->asStructType();
  if (!argStruct || parent != argStruct->parent)
    return MappingError{argType, this};
  for (int i = 0; i < templateParams.size(); ++i)
    TRY(::getDeductionError(mapping, templateParams[i], argStruct->templateParams[i]));
  return success;
}

JustError<MappingError> PointerType::getMappingError(TypeMapping& mapping, Type* argType) const {
  if (auto argPointer = argType->asPointerType())
    return ::getDeductionError(mapping, underlying, argPointer->underlying);
  return MappingError{argType, this};
}

JustError<MappingError> MutablePointerType::getMappingError(TypeMapping& mapping, Type* argType) const {
  if (auto argPointer = argType->asMutablePointerType())
    return ::getDeductionError(mapping, underlying, argPointer->underlying);
  return MappingError{argType, this};
}

bool MutablePointerType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

Type* MutablePointerType::removePointer() {
  return underlying;
}

JustError<MappingError> ReferenceType::getMappingError(TypeMapping& mapping, Type* from) const {
  return ::getDeductionError(mapping, underlying, from->removeReference());
}

JustError<MappingError> MutableReferenceType::getMappingError(TypeMapping& mapping, Type* from) const {
  if (auto argRef = from->asMutableReferenceType())
    return ::getDeductionError(mapping, underlying, argRef->underlying);
  else
    return getCantBindError(from, this);
}

JustError<MappingError> Type::getMappingError(TypeMapping&, Type* argType) const {
  if (argType == this)
    return success;
  else
    return getCantBindError(argType, this);
}

static bool areParamsTypesEqual(const FunctionSignature& f1, const FunctionSignature& f2) {
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

static JustError<ErrorLoc> expandVariadicTemplate(const Context& context, FunctionSignature& type, CodeLoc codeLoc,
    vector<Type*> templateArgs, vector<Type*> argTypes) {
  vector<Type*> expandedTypes;
  Type* lastTemplateParam = nullptr;
  if (!type.templateParams.empty())
    lastTemplateParam = type.templateParams.back();
  if (type.variadicTemplate) {
    type.templateParams.pop_back();
    Type* lastParam = nullptr;
    int cnt = 0;
    while (templateArgs.size() > type.templateParams.size()) {
      if (dynamic_cast<TemplateParameterType*>(lastTemplateParam))
        type.templateParams.push_back(new TemplateParameterType(lastTemplateParam->getType(),
            getExpandedParamName(lastTemplateParam->getName(), cnt), codeLoc));
      else
        type.templateParams.push_back(CompileTimeValue::getTemplateValue(lastTemplateParam->getType(),
            getExpandedParamName(lastTemplateParam->getName(), cnt)));
      expandedTypes.push_back(type.templateParams.back());
      if (type.variadicParams) {
        if (!lastParam) {
          lastParam = type.params.back();
          type.params.pop_back();
        }
        ErrorBuffer errors;
        type.params.push_back(lastParam->replace(context, lastTemplateParam, type.templateParams.back(), errors));
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
          type.templateParams.push_back(new TemplateParameterType(lastTemplateParam->getType(),
              getExpandedParamName(lastTemplateParam->getName(), cnt), codeLoc));
          expandedTypes.push_back(type.templateParams.back());
        }
        ErrorBuffer errors;
        thisType = lastParam->replace(context, lastTemplateParam, type.templateParams.back(), errors);
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
        if (concept->get()->isVariadic() && concept->get()->getParams().back() == lastTemplateParam)
          newRequirements.back().base = concept->get()->expand(context, lastTemplateParam, expandedTypes, errors);
    } else {
      for (auto& expanded : expandedTypes)
        newRequirements.push_back(requirement.base.visit(
            [&](const SConcept& r) {
              return TemplateRequirement(r->replace(context, lastTemplateParam, expanded, errors), false);
            },
            [&](const shared_ptr<Expression>& r) {
              return TemplateRequirement(r, true);
            }
        ));
      break;
    }
  if (lastTemplateParam)
    for (auto& param : type.params)
      param = param->expand(context, lastTemplateParam, expandedTypes, errors);
  type.requirements = std::move(newRequirements);
  if (!errors.empty())
    return codeLoc.getError(errors.front());
  if (!errors2.empty())
    return errors2.front();
  return success;
}

void generateConversions(const Context& context, const vector<Type*>& paramTypes, const vector<Type*>& argTypes,
    vector<unique_ptr<Expression>>& expr) {
  for (int i = 0; i < paramTypes.size(); ++i) {
    auto& paramType = paramTypes[i];
    auto& argType = argTypes[i];
    CHECK(context.canConvert(argType, paramType, expr[i]));
  }
}

static JustError<ErrorLoc> checkImplicitCopies(const Context& context, const vector<Type*>& paramTypes,
    vector<Type*>& argTypes, const vector<CodeLoc>& argLoc) {
  for (int i = 0; i < paramTypes.size(); ++i) {
    auto& paramType = paramTypes[i];
    auto& argType = argTypes[i];
    if (!paramType->isReference() && argType->isReference()) {
      if (argType->removeReference()->isBuiltinCopyable(context))
        argType = argType->removeReference();
      else
        return argLoc[i].getError("Type " + quote(argType->removeReference()->getName()) + " cannot be copied.");
    }
  }
  return success;
}

static JustError<ErrorLoc> getConversionError(const Context& context, FunctionInfo* input,
    const vector<Type*>& argTypes, const vector<CodeLoc>& argLoc, const vector<Type*>& funParams, TypeMapping& mapping) {
  for (int i = 0; i < argTypes.size(); ++i) {
    optional<ErrorLoc> firstError;
    if (argTypes[i] != BuiltinType::NULL_TYPE || !funParams[i]->asOptionalType()) {
      auto asConceptType = funParams[i]->removePointer()->asConceptType();
      // If it's the concept type parameter we don't try to convert to it from types satisfying the concept.
      bool conceptParam = input->type.concept && asConceptType && input->type.concept == asConceptType->concept;
      for (auto tArg : context.getConversions(argTypes[i], funParams[i], !conceptParam))
        if (!input->id.contains<Operator>() || tArg == argTypes[i] ||
            (tArg->asBuiltinType() && argTypes[i]->asBuiltinType())) {
          if (auto res = getDeductionError(mapping, funParams[i], tArg); !res) {
            if (!firstError)
              firstError = argLoc[i].getError(res.get_error().toString());
          } else {
            firstError = none;
            break;
          }
        }
    }
    if (firstError)
      return *firstError;
  }
  return success;
}

static FunctionInfo* getWithRetval(const FunctionInfo& info) {
  auto type = info.type;
  type.params.push_back(type.retVal);
  return FunctionInfo::getImplicit(info.id, type);
}

static bool isTemplatedWith(const Context& context, FunctionInfo* function, Type* type) {
  ErrorBuffer errors;
  return replaceInFunction(context, function, type, BuiltinType::INT, errors) != function;
}


vector<FunctionInfo*> getSpecialOverloads(const FunctionId& id, const vector<Type*>& argTypes) {
  vector<FunctionInfo*> ret;
  if (id == "invoke"s && !argTypes.empty()) {
    auto& argType = argTypes[0];
    if (argType->isPointer())
      if (auto lambda = dynamic_cast<LambdaType*>(argType->removePointer()))
        ret.push_back(lambda->functionInfo);
    if (auto type = dynamic_cast<FunctionType*>(argType->removePointer()))
      for (auto& overload : type->overloads) {
        auto sig = overload->type;
        sig.params.push_front(PointerType::get(type));
        ret.push_back(FunctionInfo::getDefined(overload->id, std::move(sig), overload->getDefinition()));
      }
  }
  return ret;
}

static JustError<MappingError> deduceTemplateArgsFromConcepts(const Context& context,
    TypeMapping& mapping, const vector<TemplateRequirement>& requirements, ErrorBuffer& errors) {
  for (auto& elem : requirements)
    if (auto req1 = elem.base.getReferenceMaybe<SConcept>()) {
      auto req = *req1;
      for (int i = 0; i < mapping.templateParams.size(); ++i)
        if (!!mapping.templateArgs[i])
          req = req->replace(context, mapping.templateParams[i], mapping.templateArgs[i], errors);
      for (auto function : req->getContext().getAllFunctions()) {
        vector<Type*> funTemplateParams;
        for (int i = 0; i < mapping.templateParams.size(); ++i)
          if (!mapping.templateArgs[i] && isTemplatedWith(context, function, mapping.templateParams[i])) {
            funTemplateParams.push_back(mapping.templateParams[i]);
          }
        if (funTemplateParams.empty())
          continue;
        function = addTemplateParams(function, funTemplateParams, req->isVariadic());
        vector<FunctionInfo*> found = getSpecialOverloads(function->id, function->type.params);
        for (auto& candidate : context.getFunctions(function->id, false))
          if (!candidate->type.concept && context.isGeneralization(getWithRetval(*function), getWithRetval(*candidate)))
            found.push_back(candidate);
        if (found.size() == 1) {
          for (int i = 0; i < function->type.params.size(); ++i)
            TRY(getDeductionError(mapping, function->type.params[i], found[0]->type.params[i]));
          TRY(getDeductionError(mapping, function->type.retVal, found[0]->type.retVal));
        }
      }
    }
  return success;
}

static WithError<vector<Type*>> deduceTemplateArgs(const Context& context,
    TypeMapping& mapping, const vector<TemplateRequirement>& requirements, ErrorBuffer& errors) {
  vector<Type*> ret;
  for (auto& arg : mapping.templateArgs)
    if (!arg) {
      TRY(deduceTemplateArgsFromConcepts(context, mapping, requirements, errors)
          .transform_error([](auto& error) {return error.toString(); }));
      break;
    }
  for (int i = 0; i < mapping.templateParams.size(); ++i) {
    if (!!mapping.templateArgs[i])
      ret.push_back(mapping.templateArgs[i]);
    else
      return "Couldn't deduce template argument " + quote(mapping.templateParams[i]->getName());
    if (ret.back()->asCompileTimeValue() &&
        !ret.back()->getType()->removeReference()->canBeValueTemplateParam())
      return "Value template parameter cannot have type " + quote(ret.back()->getType()->getName());
  }
  return ret;
}

WithErrorLine<FunctionInfo*> instantiateFunction(const Context& context1, FunctionInfo* input, CodeLoc codeLoc,
    vector<Type*> templateArgs, vector<Type*> argTypes, vector<CodeLoc> argLoc, vector<FunctionSignature> existing) {
  FunctionSignature type = input->type;
  auto context = context1.getTopLevel();
  auto origParams = type.templateParams;
  TRY(expandVariadicTemplate(context, type, codeLoc, templateArgs, argTypes));
  if (type.params.size() != argTypes.size())
    return codeLoc.getError("Wrong number of function arguments. Expected " +
        to_string(argTypes.size()) + " got " + to_string(type.params.size()));
  auto implicitCopySuccess = checkImplicitCopies(context, type.params, argTypes, argLoc);
  if (templateArgs.size() > type.templateParams.size())
    return codeLoc.getError("Too many template arguments.");
  TypeMapping mapping { type.templateParams, vector<Type*>(type.templateParams.size()) };
  for (int i = 0; i < templateArgs.size(); ++i)
    mapping.templateArgs[i] = templateArgs[i];
  TRY(getConversionError(context, input, argTypes, argLoc, type.params, mapping));
  ErrorBuffer errors;
  templateArgs = TRY(deduceTemplateArgs(context, mapping, type.requirements, errors).addCodeLoc(codeLoc));
  auto reqContext = context.getChild();
  for (int i = 0; i < type.templateParams.size(); ++i)
    reqContext.addType(type.templateParams[i]->getName(), templateArgs[i]);
  for (int i = 0; i < type.templateParams.size(); ++i) {
    if (!type.templateParams[i]->canReplaceBy(templateArgs[i]))
      return codeLoc.getError(getCantSubstituteError(type.templateParams[i], templateArgs[i]));
    type = replaceInFunction(reqContext, type, type.templateParams[i], templateArgs[i], errors,
        input->type.templateParams);
    type.templateParams[i] = templateArgs[i];
  }
  for (int i = 0; i < argTypes.size(); ++i)
    TRY(context.canConvert(argTypes[i], type.params[i]).addCodeLoc(argLoc[i]));
  if (errors.empty())
    for (auto& fun : existing)
      if (areParamsTypesEqual(input->type, fun))
        // To avoid infinite recursion we don't check concept requirements twice for the same >>original<< function
        // (not instantation). If this causes issues then it needs to be revised.
        return FunctionInfo::getInstance(input->id, type, input);
  existing.push_back(input->type);
  TRY(checkRequirements(reqContext, type.requirements, codeLoc, existing));
  // The replace() errors need to be checked after returning potential requirement errors.
  if (!errors.empty())
    return codeLoc.getError(errors[0]);
  if (!implicitCopySuccess)
    return implicitCopySuccess.get_error();
  return FunctionInfo::getInstance(input->id, type, input);
}

EnumType::EnumType(string n, Private) : name(std::move(n)) {}

Concept::Concept(const string& name, ConceptDefinition* def, Context context, bool variadic)
    : def(def), name(name), context(std::move(context)), variadic(variadic) {
}

string Concept::getName(bool withTemplateParams) const {
  if (withTemplateParams)
    return name + joinTemplateParams(params);
  return name;
}

SConcept Concept::translate(vector<Type*> newParams, bool variadicParams, ErrorBuffer& errors) const {
  auto ret = shared<Concept>(name, def, Context(context.typeRegistry, false), variadicParams);
  ret->context.deepCopyFrom(context);
  ret->params = newParams;
  if (!variadic || variadicParams) {
    CHECK(params.size() == newParams.size());
    for (int i = 0; i < params.size(); ++i)
      ret->context.replace(ret->context, params[i], newParams[i], errors);
  } else {
    CHECK(params.size() - 1 <= newParams.size());
    for (int i = 0; i < params.size() - 1; ++i)
      ret->context.replace(ret->context, params[i], newParams[i], errors);
    ret->context.expand(params.back(), getSubsequence(newParams, params.size() - 1), errors);
  }
  return ret;
}

SConcept Concept::expand(const Context&, Type* from, vector<Type*> newParams, ErrorBuffer& errors) {
  auto ret = shared<Concept>(name, def, Context(context.typeRegistry, false), variadic);
  ret->context.deepCopyFrom(context);
  ret->params = params;
  ret->variadic = false;
  ret->context.expand(params.back(), newParams, errors);
  ret->params.pop_back();
  return ret;
}

SConcept Concept::replace(const Context& repContext, Type* from, Type* to, ErrorBuffer& errors) const {
  auto ret = shared<Concept>(name, def, Context(context.typeRegistry), variadic);
  ret->context.deepCopyFrom(context);
  ret->params = params;
  ret->variadic = variadic;
  for (auto& param : ret->params)
    param = param->replace(repContext, from, to, errors);
  ret->context.replace(repContext, from, to, errors);
  return ret;
}

const vector<Type*>& Concept::getParams() const {
  return params;
}

bool Concept::isVariadic() const {
  return variadic;
}

JustError<ErrorLoc> Concept::canCreateConceptType() const {
  auto mainType = params[0];
  auto isTemplated = [&] (Type* type) {
    ErrorBuffer errors;
    auto res = type->replace(context, mainType, BuiltinType::INT, errors) != type;
    CHECK(errors.empty());
    return res;
  };
  for (auto& f : context.getAllFunctions()) {
    int numTemplatedParams = 0;
    for (int i = 0; i < f->type.params.size(); ++i) {
      auto& param = f->type.params[i];
      if (isTemplated(param)) {
        ++numTemplatedParams;
        if (!param->isPointer() || param->removePointer() != params[0])
          return f->getDefinition()->parameters[i].codeLoc.getError("Concept type parameter must be passed by pointer");
      }
    }
    if (numTemplatedParams != 1)
      return f->getDefinition()->codeLoc.getError("Function must take exactly one concept type parameter by pointer");
    if (isTemplated(f->type.retVal))
      return f->getDefinition()->codeLoc.getError("Function must not return values of concept type");
  }
  return success;
}

const Context& Concept::getContext() const {
  return context;
}

vector<Type*>& Concept::modParams() {
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

string joinTypeList(const vector<Type*>& types) {
  return combine(transform(types, [](Type* type) { return type->getName(); }), ", ");
}

string joinTemplateParams(const vector<Type*>& params, bool variadic) {
  if (params.empty())
    return "";
  else
    return "<" + joinTypeList(params) + (variadic ? "...>" : ">");
}

string joinTypeListCodegen(const vector<Type*>& types) {
  return combine(transform(types, [](auto& type) { return type->getCodegenName(); }), ", ");
}

string joinTemplateParamsCodegen(const vector<Type*>& params) {
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

Type* ArrayType::replaceImpl(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  if (from == size)
    if (auto value = dynamic_cast<CompileTimeValue*>(to))
      if (auto intValue = value->value.getValueMaybe<int>())
        if (*intValue < 0)
          errors.push_back("Can't have negative array size");
  return get(underlying->replace(context, from, to, errors),
      dynamic_cast<CompileTimeValue*>(size->replace(context, from, to, errors)));
}

Type* ArrayType::expand(const Context& context, Type* pack, vector<Type*> to, ErrorBuffer& errors) {
  return get(underlying->expand(context, pack, to, errors), size);
}

ArrayType* ArrayType::get(Type* type, SCompileTimeValue size) {
  static map<pair<Type*, Type*>, ArrayType*> generated;
  if (!generated.count({type, size})) {
    auto ret = new ArrayType(type, size);
    generated.insert({{type, size}, ret});
  }
  return generated.at({type, size});
}

JustError<string> ArrayType::getSizeError(const Context& context) const {
  return underlying->getSizeError(context);
}

ArrayType::ArrayType(Type* type, SCompileTimeValue size) : size(std::move(size)), underlying(type) {
}

JustError<MappingError> ArrayType::getMappingError(TypeMapping& mapping, Type* argType) const {
  if (auto argPointer = dynamic_cast<ArrayType*>(argType)) {
    TRY(::getDeductionError(mapping, size, argPointer->size));
    return ::getDeductionError(mapping, underlying, argPointer->underlying);
  }
  return MappingError{argType, this};
}

bool ArrayType::hasDestructor() const {
  return underlying->hasDestructor();
}

SCompileTimeValue CompileTimeValue::get(Value value) {
  static map<Value, SCompileTimeValue> generated;
  if (!generated.count(value)) {
    auto ret = new CompileTimeValue(Private{}, value);
    generated.insert({value, ret});
  }
  return generated.at(value);
}

CompileTimeValue::CompileTimeValue(Private, Value value) : value(std::move(value)) {
}

string CompileTimeValue::getName(bool withTemplateArguments) const {
  return value.visit(
      [](int v) { return to_string(v); },
      [](double v) { return to_string(v); },
      [](bool v) { return v ? "true" : "false"; },
      [](CharLiteral v) { return "\'" + v.literal + "\'"; },
      [](const ReferenceValue& ref) { return ref.value->getName() + " reference"; },
      [](NullValue v) { return "null"; },
      [](VoidValue v) { return "void_value"; },
      [](const string& v) { return v; },
      [](const EnumValue& t) { return t.type->getName() + "::" + t.type->elements[t.index]; },
      [](const TemplateValue& v) { return v.name; },
      [](const TemplateExpression& v) { return getPrettyString(v.op, v.args); },
      [](const TemplateFunctionCall& v) { return "[function call]"; },
      [](FunctionType* v) { return v->getName(); }
  );
}

string CompileTimeValue::getCodegenName() const {
  return value.visit(
      [this](const auto&) { return getName(); },
      [](const ReferenceValue& ref) { return ref.value->getCodegenName(); },
      [](const string& v) { return "\"" + v +"\"_lstr"; },
      [](const TemplateValue&) -> string { fail(); },
      [](const TemplateExpression&) -> string { fail(); },
      [](const TemplateFunctionCall&) -> string { fail(); },
      [](FunctionType* v) { return v->getCodegenName() + "{}"; }
  );
}

Type* CompileTimeValue::getType() const {
  return value.visit(
      [](int)-> Type* {  return BuiltinType::INT; },
      [](double)-> Type* {  return BuiltinType::DOUBLE; },
      [](bool)-> Type* {  return BuiltinType::BOOL; },
      [](CharLiteral)-> Type* {  return BuiltinType::CHAR; },
      [](const ReferenceValue& ref)-> Type* { return MutableReferenceType::get(ref.value->getType()); },
      [](NullValue)-> Type* {  return BuiltinType::NULL_TYPE; },
      [](VoidValue)-> Type* {  return BuiltinType::VOID; },
      [](const string&)-> Type* {  return BuiltinType::STRING; },
      [](const EnumValue& v)-> Type* {  return v.type; },
      [](const TemplateValue& v)-> Type* {  return v.type; },
      [](const TemplateExpression& v)-> Type* {  return v.type; },
      [](const TemplateFunctionCall& v)-> Type* {  return v.retVal; },
      [](FunctionType* v) -> Type* { return v; }
  );
}

template <typename Num>
static string mangleNumber(Num v) {
  return v < 0 ? "M" + to_string(-v) : to_string(v);
}

static string mangleString(const string& v) {
  string ret;
  for (auto c : v)
    if (isalpha(c))
      ret += c;
    else
      ret += "_" + to_string(int(c));
  return ret;
}

optional<string> CompileTimeValue::getMangledName() const {
  return value.visit(
      [this](const auto&) -> optional<string> { return getName(); },
      [](int v) -> optional<string> { return mangleNumber(v); },
      [](double v) -> optional<string> { return mangleNumber(v); },
      [](CharLiteral c) -> optional<string> { return "C" + mangleString(c.literal); },
      [](const string& v) -> optional<string> { return mangleString(v); },
      [](const ReferenceValue& ref)-> optional<string> {
        if (auto name = ref.value->getMangledName())
          return "Ref" + *name;
        else
          return none;
        },
      [](const TemplateValue& v) -> optional<string> { return none; },
      [](const TemplateExpression&) -> optional<string> { return none; },
      [](const TemplateFunctionCall&) -> optional<string> { return none; },
      [](const FunctionType*& v) -> optional<string> { return v->getName(); }
  );
}

WithError<Type*> CompileTimeValue::convertTo(Type* t) {
  if (auto ref = value.getReferenceMaybe<ReferenceValue>())
    return ref->value->convertTo(std::move(t));
  // For now int -> double is supported.
  auto myType = getType();
  if (myType == t)
    return this;
  if (t == BuiltinType::DOUBLE && myType == BuiltinType::INT)
    return value.visit(
        [](int d)-> Type* {  return CompileTimeValue::get(double(d)); },
        [t](TemplateValue& v)-> Type* {  return getTemplateValue(std::move(t), v.name); },
        // Switching the return type for expressions and function call looks iffy but works. May need to revisit this.
        [t](TemplateExpression& v)-> Type* {
          return CompileTimeValue::get(CompileTimeValue::TemplateExpression{v.op, v.args, t}); },
        [t](TemplateFunctionCall& v)-> Type* {
          return CompileTimeValue::get(CompileTimeValue::TemplateFunctionCall{
              v.name, v.args, t, v.loc, v.argLoc, v.functionInfo}); },
        [](auto&) -> Type* { fail(); }
    );
  return Type::convertTo(t);
}

CompileTimeValue* CompileTimeValue::getReference(Type* value) {
  return get(ReferenceValue{std::move(value)});
}

CompileTimeValue* CompileTimeValue::getTemplateValue(Type* type, string name) {
  return get(TemplateValue{std::move(type), std::move(name)});
}

JustError<MappingError> CompileTimeValue::getMappingError(TypeMapping& mapping, Type* argType) const {
  if (auto argValue = dynamic_cast<CompileTimeValue*>(argType)) {
    auto argType = argValue->getType();
    return getDeductionError(mapping, getType(), argType);
  } else
    return MappingError{argType, this};
}

bool CompileTimeValue::canReplaceBy(Type* t) const {
  if (auto myValue = value.getReferenceMaybe<TemplateValue>())
    if (auto v = dynamic_cast<CompileTimeValue*>(t))
      return myValue->type == v->getType()->removeReference();
  return false;
}

template <typename Fun>
static Type* compileTimeValueVisit(CompileTimeValue* v, const Context& context, Fun f, ErrorBuffer& errors) {
  return v->value.visit(
      [&](const CompileTimeValue::TemplateValue& value) {
        return CompileTimeValue::getTemplateValue(f(value.type), value.name);
      },
      [&](const CompileTimeValue::TemplateExpression& value) ->Type* {
        if (auto ret = ::eval(value.op, ::transform(value.args,
            [&](Type* t){ return f(t); })))
          return *ret;
        else {
          errors.push_back("Can't evaluate operator " + quote(getString(value.op)));
          return v;
        }
      },
      [&](const CompileTimeValue::TemplateFunctionCall& value) ->Type*{
        if (auto ret = value.functionInfo.invokeFunction(context, value.name, value.loc, ::transform(value.args,
            [&](Type* t){ return f(t); }), value.argLoc))
          return *ret;
        else {
          errors.push_back(ret.get_error().error);
          return v;
        }
      },
      [&](const auto&) ->Type* { return v;}
  );
}

Type* CompileTimeValue::replaceImpl(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  return compileTimeValueVisit(this, context,
      [&](Type* type) { return type->replace(context, from, to, errors); }, errors);
}

Type* CompileTimeValue::expand(const Context& context, Type* pack, vector<Type*> to, ErrorBuffer& errors) {
  return compileTimeValueVisit(this, context,
      [&](Type* type) { return type->expand(context, pack, to, errors); }, errors);
}

string OptionalType::getName(bool withTemplateArguments) const {
  return underlying->getName(withTemplateArguments) + "?";
}

optional<string> OptionalType::getMangledName() const {
  return underlying->getMangledName().map([](const string& name) { return "OP" + name;});
}

string OptionalType::getCodegenName() const {
  return "zenon::optional<" + underlying->getCodegenName() + ">";
}

JustError<MappingError> OptionalType::getMappingError(TypeMapping& mapping, Type* from) const {
  if (auto argPointer = from->asOptionalType())
    return ::getDeductionError(mapping, underlying, argPointer->underlying);
  return MappingError{from, this};
}

Type* OptionalType::transform(function<Type*(Type*)> fun) {
  return OptionalType::get(fun(underlying));
}

bool OptionalType::hasDestructor() const {
  return underlying->hasDestructor();
}

OptionalType* OptionalType::get(Type* type) {
  type = type->removeReference();
  static unordered_map<Type*, OptionalType*> generated;
  if (!generated.count(type)) {
    auto ret = new OptionalType(Private{}, type);
    generated.insert({type, ret});
  }
  return generated.at(type);
}

OptionalType::OptionalType(Private, Type* t) : underlying(std::move(t)) {
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

Type* LambdaType::replaceImpl(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  ErrorLocBuffer errors2;
  vector<Type*> newTemplateParams;
  for (auto& param : templateParams)
    newTemplateParams.push_back(param->replace(context, from, to, errors));
  auto ret = get(name, newTemplateParams);
  if (!ret->body) { // this is how we check that this is a newly created lambda
    ret->body = cast<StatementBlock>(body->deepCopy());
    for (auto& e : errors2)
      errors.push_back(e.error);
    auto tmpType = functionInfo->type;
    tmpType.params = getSubsequence(tmpType.params, 1);
    tmpType = replaceInFunction(context, tmpType, from, to, errors, templateParams);
    tmpType.params = concat({PointerType::get(ret)}, tmpType.params);
    ret->functionInfo = FunctionInfo::getImplicit(functionInfo->id, std::move(tmpType));
    for (auto& capture : captures)
      ret->captures.push_back(
          LambdaCapture{capture.name, capture.type->replace(context, from, to, errors), capture.captureType});
    ret->parameterNames = parameterNames;
  }
  return std::move(ret);
}

JustError<MappingError> LambdaType::getMappingError(TypeMapping&, Type* argType) const {
  return success;
}

static int getNewLambdaId() {
  static int allIds = 0;
  return ++allIds;
}

LambdaType* LambdaType::get(string name, vector<Type*> templateParams) {
  auto key = make_pair(std::move(name), std::move(templateParams));
  static map<decltype(key), LambdaType*> generated;
  if (!generated.count(key)) {
    auto ret = new LambdaType(Private{});
    ret->name = key.first;
    ret->templateParams = key.second;
    generated.insert({key, ret});
  }
  return generated.at(key);
}

bool LambdaType::hasDestructor() const {
  for (auto& capture : captures)
    if (capture.captureType != LambdaCaptureType::REFERENCE && capture.type->hasDestructor())
      return true;
  return false;
}

LambdaType* LambdaType::get(vector<Type*> templateParams) {
  return get("LAMBDA" + to_string(getNewLambdaId()), std::move(templateParams));
}

LambdaType::LambdaType(Private) {
}

CompileTimeValue::ReferenceValue::ReferenceValue(Type* v) : value(std::move(v)) {
  static int idCounter = 0;
  id = ++idCounter;
}

string VariablePack::getName(bool withTemplateArguments) const {
  return identifier + "...";
}

VariablePack::VariablePack(Type* p, string id) : identifier(id), packType(std::move(p)) {
}

Type* convertPointerToReferenceStrict(Type* type) {
  if (auto p = dynamic_cast<PointerType*>(type))
    return (Type*)ReferenceType::get(p->underlying);
  else if (auto p = dynamic_cast<MutablePointerType*>(type))
    return (Type*)MutableReferenceType::get(p->underlying);
  else
    return nullptr;
}

Type* convertReferenceToPointerStrict(Type* type) {
  if (auto p = dynamic_cast<ReferenceType*>(type))
    return (Type*)PointerType::get(p->underlying);
  else if (auto p = dynamic_cast<MutableReferenceType*>(type))
    return (Type*)MutablePointerType::get(p->underlying);
  else
    return nullptr;
}

Type* convertPointerToReference(Type* type) {
  if (auto p = convertPointerToReferenceStrict(type))
    return p;
  else
    return type;
}

Type* convertReferenceToPointer(Type* type) {
  if (auto p = convertReferenceToPointerStrict(type))
    return p;
  else
    return type;
}

ConceptType* ConceptType::get(SConcept c, vector<Type*> params, bool variadic) {
  static map<tuple<Concept*, vector<Type*>, bool>, ConceptType*> cache;
  if (!cache.count({c.get(), params, variadic})) {
    cache.insert(make_pair(make_tuple(c.get(), params, variadic), new ConceptType(Private{}, c, params, variadic)));
  }
  return cache.at({c.get(), params, variadic});
}

ConceptType::ConceptType(ConceptType::Private, SConcept c, vector<Type*> params, bool variadic)
    : concept(std::move(c)), params(std::move(params)), variadic(variadic) {
}

optional<std::string> ConceptType::getMangledName() const {
  if (auto suf = mangleTemplateParams(params))
    return concept->getName(false) + *suf;
  return none;
}

Type* ConceptType::replaceImpl(const Context& context, Type* from, Type* to, ErrorBuffer& errors) {
  return get(concept, params.transform(
      [&](Type* t){ return t->replace(context, from, to, errors); }), variadic);
}

JustError<string> ConceptType::getSizeError(const Context&) const {
  return "Concept types cannot be passed by value or used to declare a variable"s;
}

JustError<MappingError> ConceptType::getMappingError(TypeMapping& mapping, Type* argType) const {
  auto arg = dynamic_cast<ConceptType*>(argType);
  if (!arg || concept != arg->concept)
    return MappingError{argType, this};
  for (int i = 0; i < min(params.size(), arg->params.size()); ++i)
    TRY(::getDeductionError(mapping, params[i], arg->params[i]));
  return success;
}

Type* ConceptType::expand(const Context& context, Type* pack, vector<Type*> to, ErrorBuffer& errors) {
  if (variadic && params.back() == pack) {
    auto p = params;
    p.pop_back();
    p.append(to);
    return get(concept, std::move(p), false);
  }
  return Type::expand(context, std::move(pack), std::move(to), errors);
}

bool ConceptType::hasDestructor() const {
  for (auto& function : concept->getContext().getFunctions("destruct"s, false))
    if (function->type.retVal == BuiltinType::VOID && function->type.params.size() == 1
        && function->type.params[0] == PointerType::get(concept->getParams()[0]))
      return true;
  return false;
}

Type* ConceptType::getType() const {
  return BuiltinType::CONCEPT_TYPE;
}

SConcept ConceptType::getConceptFor(Type* t) const {
  ErrorBuffer errors;
  auto res = concept->translate(concat({t}, params), false, errors);
  CHECK(errors.empty());
  return res;
}

string ConceptType::getName(bool withTemplateArguments) const {
  if (withTemplateArguments)
    return concept->getName(false) + joinTemplateParams(params, variadic);
  return concept->getName(false);
}

optional<string> mangleTemplateParams(const vector<Type*>& params) {
  string ret;
  for (auto& param : params)
    if (auto name = param->getMangledName())
      ret += *name;
    else
      return none;
  return std::move(ret);
}

string AttributeType::getName(bool withTemplateArguments) const {
  return name;
}

bool AttributeType::canBeValueTemplateParam() const {
  return false;
}

bool AttributeType::canDeclareVariable() const {
  return false;
}

Type* AttributeType::getType() const {
  return BuiltinType::ATTRIBUTE_TYPE;
}

AttributeType* AttributeType::get(const string& name) {
  static map<string, AttributeType*> generated;
  if (!generated.count(name)) {
    auto ret = new AttributeType(Private{}, name);
    generated.insert({name, ret});
  }
  return generated.at(name);
}

AttributeType::AttributeType(AttributeType::Private, const string& name) : name(name) {}

string FunctionType::getName(bool withTemplateArguments) const {
  return name;
}

string FunctionType::getCodegenName() const {
  return "overloads_t";
}

bool FunctionType::isBuiltinCopyableImpl(const Context&, unique_ptr<Expression>&) const {
  return true;
}

FunctionType* FunctionType::get(const string& name, vector<FunctionInfo*> overloads) {
  static map<string, FunctionType*> generated;
  if (!generated.count(name))
    generated.insert(make_pair(name, new FunctionType(Private{}, name, std::move(overloads))));
  return generated.at(name);
}

FunctionType::FunctionType(FunctionType::Private, string name, vector<FunctionInfo*> overloads)
    : name(std::move(name)), overloads(std::move(overloads)) {
}

string MappingError::toString() const {
  return "Can't bind type " + quote(from->getName()) + " to type " + quote(to->getName());
}
