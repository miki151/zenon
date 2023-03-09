#include <typeinfo>

#include "codegen.h"
#include "ast.h"
#include "type_registry.h"

struct Buffer {
  public:
  Buffer(bool l) : includeLineNumbers(l) {}

  void add(const string& s) {
    buf += s;
  }

  const int indentSize = 2;

  void newLine(const string& s = "") {
    add("\n" + string(indent * indentSize, ' ') + s);
  }

  void newLine(CodeLoc codeLoc) {
    add("\n"  + string(indent * indentSize, ' '));
    if (includeLineNumbers)
      add("#line " + to_string(codeLoc.line) + " \"" + codeLoc.file + "\"\n");
  }

  void pop_back() {
    buf.pop_back();
  }

  string generate() {
    return std::move(buf);
  }

  int indent = 0;
  string buf;
  bool includeLineNumbers;
  unordered_set<const FunctionInfo*> instances;
  unordered_set<const ConceptType*> conceptTypes;
};

enum class Section {
  HEADER,
  TYPE_FORWARD,
  TYPES,
  DECLARATIONS,
  DEFINITIONS,
  FOOTER
};

static string getVTableName(const string& concept) {
  return concept + "_vtable";
}

void addVTableParams(FunctionInfo* fun, Buffer* buffer) {
  for (auto& param : fun->type.params) {
    if (param->getMangledName())
      buffer->add(param->getCodegenName());
    else if (dynamic_cast<PointerType*>(param))
      buffer->add("void const*");
    else
      buffer->add("void*");
    buffer->add(",");
  }
  buffer->pop_back();
}

static string getFunctionParams(FunctionInfo& functionInfo, const FunctionDefinition* definition) {
  string ret;
  for (int i = 0; i < functionInfo.type.params.size(); ++i) {
    auto& param = functionInfo.type.params[i];
    auto paramName = definition ? functionInfo.getParamName(i, definition) : "param" + to_string(i);
    string argText = param->getCodegenName() + " " + paramName.value_or("") + ", ";
    if (functionInfo.id.contains<Operator>()) {
      if (auto p = dynamic_cast<ReferenceType*>(param)) {
        auto name = paramName ? *paramName + "_ptr" : "";
        argText = p->underlying->getCodegenName() + " const& " + name + ", ";
      }
      if (auto p = dynamic_cast<MutableReferenceType*>(param)) {
        auto name = paramName ? *paramName + "_ptr" : "";
        argText = p->underlying->getCodegenName() + "& " + name + ", ";
      }
    }
    ret.append(argText);
  }
  if (!functionInfo.type.params.empty()) {
    ret.pop_back();
    ret.pop_back();
  }
  return ret;
}

static string getVTableBinder(FunctionInfo& function) {
  auto ret = "+[](" + getFunctionParams(function, nullptr) + ") { return " + function.getMangledName() + "(";
  for (int i = 0; i < function.type.params.size(); ++i) {
    if (i > 0)
      ret += ",";
    ret += "std::move(param" + to_string(i) + ")";
  }
  return ret + ");}";
}

struct Sections {
  Sections(bool includeLineNumbers) : includeLineNumbers(includeLineNumbers) {
    sections[Section::TYPES] = {};
    sections[Section::DECLARATIONS] = {};
    sections[Section::DEFINITIONS] = {};
  }
  Buffer* newBuffer(Section section) {
    sections[section].push_back(make_unique<Buffer>(includeLineNumbers));
    return sections[section].back().get();
  }
  void registerFunctionCall(FunctionInfo* functionInfo) {
    if (auto def = functionInfo->getDefinition()) {
      if (!emitted.count(functionInfo->getWithoutRequirements())) {
        emitted.insert(functionInfo->getWithoutRequirements());
        def->codegenInstance(newBuffer(Section::DEFINITIONS), this, functionInfo);
      }
    } else
    if (functionInfo->id.contains<ConstructorTag>())
      registerTypeAccess(functionInfo->type.retVal);
  }
  void registerTypeAccess(Type* type) {
    if (type->getMangledName() && !typeDefEmitted.count(type)) {
      typeDefEmitted.insert(type);
      auto buf = make_unique<Buffer>(includeLineNumbers);
      type->codegenDefinition(buf.get(), this);
      sections[Section::TYPES].push_back(std::move(buf));
    }
  }
  void registerTypePointer(Type* type) {
    auto buffer = newBuffer(Section::TYPE_FORWARD);
    if (auto s = dynamic_cast<StructType*>(type)) {
      if (!s->external)
        buffer->newLine("struct " + type->getCodegenName() + ";");
    } else
    if (dynamic_cast<StructType*>(type))
      buffer->newLine("enum " + type->getCodegenName() + ";");
  }
  void registerFatPointer(ConceptType* conceptType, Type* type, vector<FunctionInfo*> vTable) {
    if (fatPointers.count({conceptType, type}))
      return;
    fatPointers.insert({conceptType, type});
    auto declBuffer = newBuffer(Section::DECLARATIONS);
    auto defBuffer = newBuffer(Section::DEFINITIONS);
    declBuffer->add("extern ");
    auto vTableName = getVTableName(*conceptType->getMangledName());
    defBuffer->newLine(vTableName + " " + vTableName + "_" + *type->getMangledName());
    declBuffer->newLine(vTableName + " " + vTableName + "_" + *type->getMangledName());
    defBuffer->add("{");
    ++defBuffer->indent;
    auto functions = conceptType->getConceptFor(conceptType->concept->getParams()[0])->getContext().getAllFunctions();
    for (int i = 0; i < functions.size(); ++i) {
      registerFunctionCall(vTable[i]);
      defBuffer->newLine("reinterpret_cast<" + functions[i]->type.retVal->getCodegenName() + " (*)(");
      addVTableParams(functions[i], defBuffer);
      defBuffer->add(")>(" + getVTableBinder(*vTable[i]) + "),");
    }
    defBuffer->newLine("reinterpret_cast<void (*)(void const*)>(+[](" + type->getCodegenName() +" param0) { delete param0;})");
    --defBuffer->indent;
    defBuffer->newLine("}");
    defBuffer->newLine(";");
    defBuffer->newLine();
    declBuffer->newLine(";");
    declBuffer->newLine();
  }

  void addImport(ImportStatement* s) {
    if (imports.count(*s->absolutePath))
      return;
    imports.insert(*s->absolutePath);
    s->codegen(newBuffer(Section::HEADER), this);
  }

  void addEmbed(EmbedStatement* s) {
    if (embeds.count(s))
      return;
    embeds.insert(s);
    s->codegen(newBuffer(Section::HEADER), this);
  }

  map<Section, vector<unique_ptr<Buffer>>> sections;
  unordered_set<FunctionInfo*> emitted;
  unordered_set<Type*> forwardDeclEmitted;
  unordered_set<Type*> typeDefEmitted;
  set<pair<ConceptType*, Type*>> fatPointers;
  unordered_set<string> imports;
  unordered_set<EmbedStatement*> embeds;
  bool includeLineNumbers;
  string generate() const {
    string ret;
    for (auto& elem : sections)
      for (auto& buf : elem.second)
        ret.append(buf->buf);
    return ret;
  }
};

void Constant::codegen(Buffer* buffer, Sections* sections) const {
  if (refValue)
    buffer->add(refValue->getCodegenName());
  else if (structMemberName)
    buffer->add(*structMemberName);
  else {
    sections->registerTypeAccess(value->getType());
    buffer->add(value->getCodegenName());
  }
}

auto constexpr lambdaArgName = "lambda_Arg";

void Variable::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add(*identifier.asBasicIdentifier());
}

static string getDestructorName(const string& id) {
  return "destruct_" + id;
}

void MoveExpression::codegen(Buffer* buffer, Sections* sections) const {
  if (hasDestructor)
    buffer->add("moveAndSetMoved(" + identifier +", &" + getDestructorName(identifier) + ".wasMoved)");
  else
    buffer->add("std::move(" + identifier + ")");
}

void BinaryExpression::codegen(Buffer* buffer, Sections* sections) const {
  sections->registerFunctionCall(functionInfo);
  if (functionInfo && !functionInfo->type.builtinOperator)
    if (auto opName = getCodegenName(op)) {
      buffer->add(opName + *functionInfo->getMangledSuffix() + "("s);
      auto handleExpr = [&](int index) {
        if (destructorCall[index])
          buffer->add("*get_temporary_holder(");
        expr[index]->codegen(buffer, sections);
        if (destructorCall[index]) {
          buffer->add(", &::" + destructorCall[index]->getMangledName() + ")");
          sections->registerFunctionCall(destructorCall[index]);
        }
      };
      handleExpr(0);
      buffer->add(", ");
      handleExpr(1);
      buffer->add(")");
      return;
    }
  buffer->add("(");
  expr[0]->codegen(buffer, sections);
  buffer->add(") ");
  if (op == Operator::SUBSCRIPT)
    buffer->add("[");
  else
    buffer->add(getString(op) + " "s);
  buffer->add("(");
  expr[1]->codegen(buffer, sections);
  buffer->add(")");
  if (op == Operator::SUBSCRIPT)
    buffer->add("]");
}

void StatementBlock::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("{");
  ++buffer->indent;
  for (auto& s : elems) {
    buffer->newLine(s->codeLoc);
    s->codegen(buffer, sections);
  }
  --buffer->indent;
  buffer->newLine("}");
}

static void codegenDestructorCall(Buffer* buffer, Sections* sections, const Statement& destructor, const string& variable) {
  buffer->add("auto " + getDestructorName(variable) + " = deferDestruct([&]{ ");
  destructor.codegen(buffer, sections);
  buffer->add(";});");
}

void VariableDeclaration::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add(realType->getCodegenName() + " ");
/*  if (!isMutable)
    accu.add("const ");*/
  buffer->add(identifier);
  if (initExpr) {
    buffer->add(" = ");
    initExpr->codegen(buffer, sections);
  }
  buffer->add(";");
  if (destructorCall)
    codegenDestructorCall(buffer, sections, *destructorCall, identifier);
  sections->registerTypeAccess(realType);
}

void AliasDeclaration::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("auto&& " + identifier + " = ");
  initExpr->codegen(buffer, sections);
  buffer->add(";");
  if (destructorCall)
    codegenDestructorCall(buffer, sections, *destructorCall, identifier);
}

void IfStatement::codegen(Buffer* buffer, Sections* sections) const {
  if (declaration) {
    buffer->newLine("{");
    declaration->codegen(buffer, sections);
  }
  buffer->newLine("if (");
  condition->codegen(buffer, sections);
  buffer->add(")");
  ++buffer->indent;
  buffer->newLine("{");
  ifTrue->codegen(buffer, sections);
  buffer->add("}");
  --buffer->indent;
  if (ifFalse) {
    buffer->newLine("else {");
    ++buffer->indent;
    buffer->newLine();
    ifFalse->codegen(buffer, sections);
    buffer->add("}");
    --buffer->indent;
  }
  if (declaration)
    buffer->newLine("}");
}

void ReturnStatement::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("return ");
  expr->codegen(buffer, sections);
  buffer->add(";");
}

static string getFunctionCallName(FunctionInfo& functionInfo, bool methodCall) {
  if (functionInfo.type.generatedConstructor && functionInfo.type.parentType)
    return functionInfo.type.parentType->getCodegenName();
  string typePrefix;
  auto functionTemplateParams = functionInfo.type.templateParams;
  if (functionInfo.type.parentType)
    typePrefix = functionInfo.type.parentType->getCodegenName() + "::";
  else if (!methodCall && !functionInfo.isMemberFunction())
    typePrefix += "::";
  return typePrefix + functionInfo.getMangledName();
}

void FunctionCall::codegen(Buffer* buffer, Sections* sections) const {
  string suffix;
  string id = getFunctionCallName(*functionInfo, !!callType);
  bool voidConversion =
      functionInfo->type.builtinOperator && functionInfo->type.retVal == BuiltinType::VOID;
  if (voidConversion)
    buffer->add("({");
  if (functionInfo->type.generatedConstructor) {
    buffer->add(id + "{");
    suffix = "}";
  } else {
    if (functionInfo->isMemberFunction()) {
      buffer->add("(");
      if (!methodCall)
        buffer->add("*");
      arguments[0]->codegen(buffer, sections);
      buffer->add(").");
    }
    buffer->add(id + "(");
    suffix = ")";
  }
  bool extractPointer = callType == MethodCallType::FUNCTION_AS_METHOD_WITH_POINTER;
  for (auto& arg : arguments) {
    if (functionInfo->isMemberFunction() && arg == arguments[0]) {
      extractPointer = false;
      continue;
    }
    if (extractPointer) {
      buffer->add("op_get_address(");
      if (destructorCall) {
        buffer->add("*get_temporary_holder(");
        sections->registerFunctionCall(destructorCall);
      }
    }
    arg->codegen(buffer, sections);
    if (extractPointer && destructorCall)
      buffer->add(", &::" + destructorCall->getMangledName() + ")");
    if (extractPointer) {
      buffer->add(")");
      extractPointer = false;
    }
    if (arg != arguments.back())
      buffer->add(", ");
  }
  buffer->add(suffix);
  if (voidConversion)
    buffer->add("; void_value; })");
  sections->registerFunctionCall(functionInfo);
}

static string getFunctionSignatureName(FunctionInfo& function) {
  string typePrefix;
  if (function.type.parentType)
    typePrefix = function.type.parentType->getName() + "::";
  return function.getMangledName();
}

static string getSignature(FunctionInfo& functionInfo, const FunctionDefinition* definition) {
  return functionInfo.type.retVal->getCodegenName() + " "
      + getFunctionSignatureName(functionInfo) + "("
      + getFunctionParams(functionInfo, definition) + ")";
}

void FunctionDefinition::handlePointerParamsInOperator(Buffer* buffer, Sections* sections, const StatementBlock* thisBody) const {
  vector<string> ptrInits;
  for (int i = 0; i < functionInfo->type.params.size(); ++i) {
    auto& param = functionInfo->type.params[i];
    auto name = functionInfo->getParamName(i, this);
    if (name && functionInfo->id.contains<Operator>())
      if (dynamic_cast<ReferenceType*>(param) || dynamic_cast<MutableReferenceType*>(param))
        ptrInits.push_back("auto " + *name + " = &" + *name + "_ptr;");
  }
  if (!ptrInits.empty()) {
    buffer->newLine("{");
    ++buffer->indent;
    for (auto& elem : ptrInits)
      buffer->newLine(elem);
    buffer->newLine();
    thisBody->codegen(buffer, sections);
    --buffer->indent;
    buffer->newLine("}");
  } else
    thisBody->codegen(buffer, sections);
}

void FunctionDefinition::addStacktraceGenerator(Buffer* buffer, Sections* sections, const StatementBlock* thisBody) const {
  buffer->add("F_BEGIN");
  handlePointerReturnInOperator(buffer, sections, thisBody);
  buffer->add("F_END(\"" + functionInfo->prettyString() + "\")");
}

void FunctionDefinition::handlePointerReturnInOperator(Buffer* buffer, Sections* sections, const StatementBlock* thisBody) const {
  if (dynamic_cast<ReferenceType*>(functionInfo->type.retVal) ||
      dynamic_cast<MutableReferenceType*>(functionInfo->type.retVal)) {
    buffer->newLine("{");
    ++buffer->indent;
    buffer->newLine("auto getRef = [&]");
    handlePointerParamsInOperator(buffer, sections, thisBody);
    buffer->add(";");
    buffer->newLine("return *getRef();");
    --buffer->indent;
    buffer->newLine("}");
  } else
    handlePointerParamsInOperator(buffer, sections, thisBody);
}

void FunctionDefinition::codegen(Buffer* buffer, Sections* sections) const {
  codegenInstance(buffer, sections, functionInfo);
}

void FunctionDefinition::codegenInstance(Buffer* buffer, Sections* sections, FunctionInfo* info) const {
  auto addInstance = [&](FunctionInfo& functionInfo, StatementBlock* body,
      const vector<unique_ptr<Statement>>& destructors) {
    if (!external) {
      if (!functionInfo.type.templateParams.empty())
        buffer->add("inline ");
      auto declBuf = sections->newBuffer(Section::DECLARATIONS);
      auto signature = getSignature(functionInfo, this);
      declBuf ->add(signature + ";\n");
      buffer->add(signature);
      if (body) {
        buffer->add("{");
        CHECK(destructors.size() == functionInfo.type.params.size())
            << functionInfo.prettyString();
        for (int i = 0; i < functionInfo.type.params.size(); ++i)
          if (destructors[i]) {
            codegenDestructorCall(buffer, sections, *destructors[i], *parameters[i].name);
            buffer->newLine();
          }
        addStacktraceGenerator(buffer, sections, body);
        buffer->newLine("}");
      } else
        buffer->add(";");
      buffer->newLine("");
      buffer->newLine("");
      sections->registerTypeAccess(functionInfo.type.retVal);
      for (auto& param : functionInfo.type.params)
        sections->registerTypeAccess(param);
    }
  };
  if (info == functionInfo) {
    addInstance(*functionInfo, body.get(), destructorCalls);
    return;
  }
  for (auto& instance : instances) {
    if (instance.functionInfo->getWithoutRequirements() == info->getWithoutRequirements()) {
      addInstance(*instance.functionInfo, instance.body.get(), instance.destructorCalls);
      return;
    }
  }
  if (!external) {
  /*  for (auto& instance : instances)
      std::cout << instance.functionInfo->prettyString() << " " << instance.functionInfo.get() << std::endl;
    FATAL << this << " Instance not found: " << info->prettyString() << " " << info;*/
  }
}

constexpr const char* unionEnumeratorPrefix = "Enum_";
constexpr const char* unionEntryPrefix = "Union_";
constexpr const char* unionDiscriminatorName = "unionElem";

static void codegenUnion(Buffer* buffer, Sections* sections, const StructType* type) {
  for (auto& elem : type->alternatives)
    sections->registerTypeAccess(elem.type);
  auto name = *type->getMangledName();
  buffer->add("struct " + name + " {");
  ++buffer->indent;
  buffer->newLine();
  buffer->add("enum {");
  vector<string> typeNames;
  for (auto& subtype : type->alternatives)
    typeNames.push_back(subtype.name);
  buffer->add(combine(transform(typeNames, [](const string& e){ return unionEnumeratorPrefix + e;}), ", ") + "} "
      + unionDiscriminatorName + ";");
  for (auto& alternative : type->alternatives)
    buffer->newLine("template <typename... Args> static " + name + " " + alternative.name + "(Args... args);");
  buffer->newLine("union {");
  ++buffer->indent;
  buffer->newLine("bool dummy;");
  for (auto& alternative : type->alternatives)
    buffer->newLine(alternative.type->getCodegenName() + " " + unionEntryPrefix + alternative.name + ";");
  --buffer->indent;
  buffer->newLine("};");
  auto visitBody = [&] {
    ++buffer->indent;
    buffer->newLine("switch (unionElem) {");
    ++buffer->indent;
    for (auto& alternative : type->alternatives) {
      buffer->newLine("case "s + unionEnumeratorPrefix + alternative.name + ":");
      ++buffer->indent;
      buffer->newLine("return std::forward<Visitor>(v)("s + unionEntryPrefix + alternative.name + ");");
      --buffer->indent;
    }
    --buffer->indent;
    buffer->newLine("}");
    --buffer->indent;
  };
  buffer->newLine("template <typename Visitor>");
  buffer->newLine("auto visit(Visitor&& v) const {");
  visitBody();
  buffer->newLine("}");
  buffer->newLine("template <typename Visitor>");
  buffer->newLine("auto visit(Visitor&& v) {");
  visitBody();
  buffer->newLine("}");
  buffer->newLine(name + "(" + name + "&& o) { UnionHelper<" + name + ">::move(std::move(o), *this);  }");
  buffer->newLine(name + "& operator = (" + name + "&& o) {UnionHelper<" + name + ">::assign(std::move(o), *this); return *this; }");
  buffer->newLine("~" + name + "() { UnionHelper<" + name + ">::destroy(*this); }");
  buffer->newLine("private:" + name + "() {}");
  --buffer->indent;
  buffer->newLine("};");
  buffer->newLine();
  for (auto& alternative : type->alternatives) {
    auto name = *type->getMangledName();
    auto buf2 = sections->newBuffer(Section::DEFINITIONS);
    buf2->add("template <typename... Args> " + name + " " + name + "::" + alternative.name +
        "(Args... args)" + " {");
    ++buf2->indent;
    buf2->newLine(name + " ret;");
    buf2->newLine("ret."s + unionDiscriminatorName + " = " + unionEnumeratorPrefix + alternative.name + ";");
    if (!(alternative.type == BuiltinType::VOID))
      buf2->newLine("new (&ret."s + unionEntryPrefix + alternative.name + ") " +
          alternative.type->getCodegenName() + "{std::forward<Args>(args)...};");
    buf2->newLine("return ret;");
    --buf2->indent;
    buf2->newLine("}");
    buf2->newLine("");
  }
}

void Type::codegenDefinition(Buffer* buffer, Sections* sections) {
}

void ArrayType::codegenDefinition(Buffer* buffer, Sections* sections) {
  sections->registerTypeAccess(underlying);
}

void ReferenceType::codegenDefinition(Buffer* buffer, Sections* sections) {
  sections->registerTypeAccess(underlying);
}

void MutableReferenceType::codegenDefinition(Buffer* buffer, Sections* sections) {
  sections->registerTypeAccess(underlying);
}

void PointerType::codegenDefinition(Buffer* buffer, Sections* sections) {
  if (dynamic_cast<ConceptType*>(underlying))
    sections->registerTypeAccess(underlying);
  else
    sections->registerTypePointer(underlying);
}

void MutablePointerType::codegenDefinition(Buffer* buffer, Sections* sections) {
  if (dynamic_cast<ConceptType*>(underlying))
    sections->registerTypeAccess(underlying);
  else
    sections->registerTypePointer(underlying);
}

void OptionalType::codegenDefinition(Buffer* buffer, Sections* sections) {
  sections->registerTypeAccess(underlying);
}

void EnumType::codegenDefinition(Buffer* buffer, Sections* sections) {
  if (!external) {
    buffer->add("enum class " + name + " {");
    ++buffer->indent;
    for (auto& elem : elements)
      buffer->newLine(elem + ",");
    --buffer->indent;
    buffer->newLine("};");
  }
    buffer->newLine("template<> struct EnumInfo<" + name + "> {");
    buffer->newLine("  static const char* getString(" + name + " elem) {");
    buffer->newLine("    switch (elem) {");
    for (auto& elem : elements)
      buffer->newLine("      case " + name + "::" + elem + ": return \"" + elem + "\";");
    buffer->newLine("    }");
    buffer->newLine("  }");
    buffer->newLine("};");
}

void StructType::codegenDefinition(Buffer* buffer, Sections* sections) {
  if (external) {
    if (auto name = getMangledName()) {
      for (auto& elem : parent->memberTemplateParams)
        sections->registerTypeAccess(templateParams[elem]);
    }
    return;
  }
  if (auto name = getMangledName()) {
    if (!alternatives.empty()) {
      codegenUnion(buffer, sections, this);
      return;
    }
    for (auto& elem : members)
      sections->registerTypeAccess(elem.type);
    buffer->add("struct " + *name);
    buffer->add(" {");
    ++buffer->indent;
    for (auto& member : members)
      buffer->newLine(member.type->getCodegenName() + " " + member.name + ";");
    --buffer->indent;
    buffer->newLine("};");
    buffer->newLine();
  }
}

static unique_ptr<StatementBlock> generateLambdaBody(Statement* body, const LambdaType& type) {
  auto ret = make_unique<StatementBlock>(body->codeLoc);
  for (auto& capture : type.captures) {
    auto memberExpr = cast<Expression>(MemberAccessExpression::getPointerAccess(body->codeLoc,
        make_unique<Variable>(IdentifierInfo(lambdaArgName, body->codeLoc)), capture.name));
    auto type = capture.type;
    if (capture.captureType == LambdaCaptureType::REFERENCE) {
      type = convertPointerToReference(type->removeReference());
      memberExpr = make_unique<UnaryExpression>(body->codeLoc, Operator::POINTER_DEREFERENCE, std::move(memberExpr));
    } else
      type = ReferenceType::get(type);
    auto decl = make_unique<VariableDeclaration>(body->codeLoc, none, capture.name, std::move(memberExpr));
    decl->realType = type;
    ret->elems.push_back(std::move(decl));
  }
  ret->elems.push_back(make_unique<ExternalStatement>(body));
  return ret;
}

using LambdaSet = unordered_set<const LambdaType*>;

void codegenLambda(LambdaType* lambda, Buffer* buffer, Sections* sections) {
  vector<unique_ptr<FunctionDefinition>> defs;
  if (lambda->functionInfo->getMangledSuffix()) {
    const auto dummyIdent = IdentifierInfo("ignore", lambda->body->codeLoc);
    auto getLambdaBody = [&lambda, &dummyIdent](Statement* body) {
      auto def = make_unique<FunctionDefinition>(body->codeLoc, dummyIdent,
          lambda->functionInfo->id, body->codeLoc);
      def->body = generateLambdaBody(body, *lambda);
      def->parameters.push_back(FunctionParameter{def->body->codeLoc, dummyIdent, string(lambdaArgName), false, false});
      def->wasUsed = true;
      return def;
    };
    auto mainBody = getLambdaBody(lambda->body.get());
    mainBody->destructorCalls.emplace_back();
    const auto& functionType = lambda->functionInfo->type;
    for (int i = 1; i < functionType.params.size(); ++i) {
      mainBody->parameters.push_back(FunctionParameter{mainBody->body->codeLoc, dummyIdent,
          lambda->parameterNames[i - 1], false, false});
      if (auto call = lambda->destructorCalls[i - 1].get())
        mainBody->destructorCalls.push_back(make_unique<ExternalStatement>(call));
      else
        mainBody->destructorCalls.push_back(nullptr);
    }
    mainBody->functionInfo = FunctionInfo::getDefined(lambda->functionInfo->id, std::move(functionType),
        mainBody.get());
    mainBody->codegen(buffer, sections);
    if (lambda->destructor) {
      auto destructorBody = getLambdaBody(lambda->destructor.get());
      destructorBody->destructorCalls.emplace_back();
      destructorBody->functionInfo = FunctionInfo::getImplicit("destruct"s,
          FunctionSignature(BuiltinType::VOID, {PointerType::get(lambda)}, {}));
      destructorBody->body->elems.push_back(make_unique<ReturnStatement>(destructorBody->codeLoc));
      destructorBody->codegen(buffer, sections);
    }
  }
}

string codegen(const vector<const AST*>& astList, TypeRegistry& registry, const string& codegenInclude,
    bool includeLineNumbers) {
  Sections sections(includeLineNumbers);
  auto header = sections.newBuffer(Section::HEADER);
  header->add("#include \"" + codegenInclude + "/all.h\"");
  header->newLine();
  for (auto ast : astList)
    for (auto& elem : ast->elems) {
      if (auto embed = dynamic_cast<EmbedStatement*>(elem.get()))
        sections.addEmbed(embed);
      if (auto import = dynamic_cast<ImportStatement*>(elem.get()))
        sections.addImport(import);
    }
  for (auto ast : astList)
    for (auto& elem : ast->elems) {
      if (auto def = dynamic_cast<FunctionDefinition*>(elem.get()))
        if (def->functionInfo->isEntryPoint())
          elem->codegen(sections.newBuffer(Section::DEFINITIONS), &sections);
    }
  auto footer = sections.newBuffer(Section::FOOTER);
  for (auto ast : astList)
    for (auto& elem : ast->elems) {
      if (auto fun = dynamic_cast<const FunctionDefinition*>(elem.get()))
        if (fun->functionInfo->isMainFunction()) {
          if (fun->parameters.empty()) {
            if (fun->functionInfo->type.retVal == BuiltinType::VOID)
              footer->add("#include \"" + codegenInclude + "/main_body_void.h\"");
            else
              footer->add("#include \"" + codegenInclude + "/main_body.h\"");
          } else {
            if (fun->functionInfo->type.retVal == BuiltinType::VOID)
              footer->add("#include \"" + codegenInclude + "/main_body_args_void.h\"");
            else
              footer->add("#include \"" + codegenInclude + "/main_body_args.h\"");
          }
        }
    }
  return sections.generate();
}

void ExpressionStatement::codegen(Buffer* buffer, Sections* sections) const {
  if (!isConstant) {
    expr->codegen(buffer, sections);
    buffer->add(";");
  }
}

void UnionDefinition::codegen(Buffer* buffer, Sections* sections) const {
}

void SwitchStatement::codegen(Buffer* buffer, Sections* sections) const {
  switch (type) {
    case UNION:
      codegenUnion(buffer, sections);
      break;
    case ENUM:
      codegenEnum(buffer, sections);
      break;
  }
}

void SwitchStatement::codegenEnum(Buffer* buffer, Sections* sections) const {
  buffer->add("switch (");
  expr->codegen(buffer, sections);
  buffer->add(") {");
  ++buffer->indent;
  for (auto& caseElem : caseElems) {
    for (auto& id : caseElem.ids)
      buffer->newLine("case " + *targetType->getMangledName() + "::" + id.first + ":");
    buffer->newLine("{");
    ++buffer->indent;
    buffer->newLine();
    caseElem.block->codegen(buffer, sections);
    buffer->newLine("break;");
    --buffer->indent;
    buffer->newLine("}");
  }
  if (defaultBlock) {
    buffer->newLine("default: {");
    ++buffer->indent;
    buffer->newLine();
    defaultBlock->codegen(buffer, sections);
    buffer->newLine("break;");
    --buffer->indent;
    buffer->newLine("}");
  }
  --buffer->indent;
  buffer->newLine("}");
}

constexpr const char* unionTmpRef = "unionTmpRef";

void SwitchStatement::codegenUnion(Buffer* buffer, Sections* sections) const {
  buffer->add("{ auto&& "s + unionTmpRef + " = ");
  expr->codegen(buffer, sections);
  buffer->add(";");
  sections->registerTypeAccess(targetType);
  if (destructorCall) {
    buffer->add("auto " + getDestructorName(unionTmpRef) + " = deferDestruct([&]{ "
        + destructorCall->getMangledName() + "(&" + unionTmpRef + ");});");
    sections->registerFunctionCall(destructorCall);
  }
  buffer->newLine("switch ("s + unionTmpRef + "."s + unionDiscriminatorName + ") {");
  ++buffer->indent;
  for (auto& caseElem : caseElems) {
    auto caseId = getOnlyElement(caseElem.ids)->first;
    buffer->newLine("case "s + *targetType->getMangledName() + "::" + unionEnumeratorPrefix + caseId + ": {");
    ++buffer->indent;
    if (!!caseElem.declaredVar)
      buffer->newLine("auto&& "s + caseId + " = " + unionTmpRef + "." + unionEntryPrefix + caseId + ";");
    if (destructorCall)
      buffer->newLine("auto& "s + getDestructorName(caseId) + " = " + getDestructorName(unionTmpRef) + ";");
    buffer->newLine();
    caseElem.block->codegen(buffer, sections);
    buffer->newLine("break;");
    --buffer->indent;
    buffer->newLine("}");
  }
  if (defaultBlock) {
    buffer->newLine("default: {");
    ++buffer->indent;
    buffer->newLine();
    defaultBlock->codegen(buffer, sections);
    buffer->newLine("break;");
    --buffer->indent;
    buffer->newLine("}");
  }
  --buffer->indent;
  buffer->newLine("}");
  buffer->newLine("}");
}

void UnaryExpression::codegen(Buffer* buffer, Sections* sections) const {
  if (functionInfo)
    sections->registerFunctionCall(functionInfo);
  if (functionInfo && !functionInfo->type.builtinOperator)
    if (auto opName = getCodegenName(op)) {
      buffer->add(opName + *functionInfo->getMangledSuffix() + "("s);
      if (destructorCall)
        buffer->add("*get_temporary_holder(");
      expr->codegen(buffer, sections);
      if (destructorCall) {
        buffer->add(", &::" + destructorCall->getMangledName() + ")");
        sections->registerFunctionCall(destructorCall);
      }
      buffer->add(")");
      return;
    }
  buffer->add(getString(op));
  buffer->add("(");
  expr->codegen(buffer, sections);
  buffer->add(") ");
}

void EmbedStatement::codegen(Buffer* buffer, Sections* sections) const {
  if (!isTopLevel)
    buffer->newLine("{");
  for (auto& r : replacements)
    buffer->newLine((r.constant ? "constexpr auto " : "using ") + r.from + " = " + r.to + ";");
  buffer->newLine(value);
  if (!isTopLevel)
    buffer->newLine("}");
  buffer->newLine();
}

static string getLoopBreakLabel(int loopId) {
  return "break_loop_" + to_string(loopId);
}

static string getLoopContinueLabel(int loopId) {
  return "continue_loop_" + to_string(loopId);
}

void WhileLoopStatement::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("while (");
  cond->codegen(buffer, sections);
  buffer->add(")");
  ++buffer->indent;
  buffer->newLine("{");
  body->codegen(buffer, sections);
  buffer->newLine(getLoopContinueLabel(loopId) + ":;");
  if (afterContinue)
    afterContinue->codegen(buffer, sections);
  buffer->add("}");
  --buffer->indent;
  buffer->newLine(getLoopBreakLabel(loopId) + ":;");
  buffer->newLine();
}

void ImportStatement::codegen(Buffer* buffer, Sections* sections) const {
  if (ast)
    for (auto& elem : ast->elems) {
      if (auto s = dynamic_cast<ImportStatement*>(elem.get())) {
        sections->addImport(s);
      }
      if (auto e = dynamic_cast<EmbedStatement*>(elem.get()))
        sections->addEmbed(e);
    }
}

void EnumConstant::codegen(Buffer* buffer, Sections* sections) const {
  sections->registerTypeAccess(enumType);
  buffer->add(enumType->getCodegenName() + "::" + enumElement);
}

static string getVTableFunName(const FunctionId id) {
  return id.visit(
      [](const string& s) {
        return s;
      },
      [](Operator op) {
        return getCodegenName(op);
      },
      [](auto&) -> string {
        fail();
      }
  );
}

void ConceptType::codegenDefinition(Buffer* buffer, Sections* sections) {
  const auto vTableName = getVTableName(*getMangledName());
  auto functions = getConceptFor(concept->getParams()[0])->getContext().getAllFunctions();
  auto typeBuf = make_unique<Buffer>(sections->includeLineNumbers);
  //buf->newLine("// " + type->getName());
  typeBuf->newLine("struct " + vTableName + " {");
  ++typeBuf->indent;
  for (auto& fun : functions) {
    typeBuf->newLine(fun->type.retVal->getCodegenName() + " (*" + getVTableFunName(fun->id) + ")(");
    for (auto param : fun->type.params)
      sections->registerTypeAccess(param);
    addVTableParams(fun, typeBuf.get());
    typeBuf->add(");");
  }
  typeBuf->newLine("void (*cpp_delete)(void const*);");
  --typeBuf->indent;
  typeBuf->newLine("};");
  typeBuf->newLine();
  sections->sections[Section::TYPES].push_back(std::move(typeBuf));
  ErrorBuffer errors;
  for (auto fun : functions) {
    fun = replaceInFunction(concept->getContext(), fun, concept->getParams()[0], this, errors);
    buffer->newLine("inline " + fun->type.retVal->getCodegenName() + " " + fun->getMangledName() + "(");
    auto getArgName = [](int i) { return "_arg" + to_string(i); };
    string virtualArg;
    sections->registerTypeAccess(fun->type.retVal);
    for (int i = 0; i < fun->type.params.size(); ++i) {
      auto& param = fun->type.params[i];
      sections->registerTypeAccess(param);
      buffer->add((i > 0 ? ", " : "") + param->getCodegenName() + " " + getArgName(i));
      if (i > 0)
        virtualArg += ", ";
      if (param->removePointer() == this)
        virtualArg = "return " + getArgName(i) + ".vTable->" + getVTableFunName(fun->id) + "(" + virtualArg + getArgName(i) + ".object";
      else
        virtualArg += "std::move(" + getArgName(i) + ")";
    }
    CHECK(!virtualArg.empty());
    buffer->add(") {");
    ++buffer->indent;
    buffer->newLine(virtualArg + ");");
    --buffer->indent;
    buffer->newLine("}");
    buffer->newLine();
  }
  CHECK(errors.empty());
}

void ConceptDefinition::codegen(Buffer* buffer, Sections* sections) const {
}

void BreakStatement::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("goto " + getLoopBreakLabel(loopId) + ";");
}

void ContinueStatement::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("goto " + getLoopContinueLabel(loopId) + ";");
}

void ArrayLiteral::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("make_array<" + type->getCodegenName() + ">(");
  for (auto& elem : contents) {
    elem->codegen(buffer, sections);
    buffer->add(", ");
  }
  if (!contents.empty()) {
    buffer->pop_back();
    buffer->pop_back();
  }
  buffer->add(")");
}

void LambdaExpression::codegen(Buffer* buffer, Sections* sections) const {
  sections->registerTypeAccess(type);
  for (auto& f : functionCalls)
    sections->registerFunctionCall(f);
  codegenLambda(type, sections->newBuffer(Section::DEFINITIONS), sections);
  buffer->add(type->getCodegenName() + "{");
  for (auto& capture : captureInfo.captures) {
    switch (capture.type) {
      case LambdaCaptureType::MOVE:
        if (capture.hasConstructor)
          buffer->add("({" + getDestructorName(capture.name) + ".wasMoved = true; std::move(" + capture.name + ");}),");
        else
          buffer->add("std::move(" + capture.name + "),");
        break;
      case LambdaCaptureType::COPY:
        buffer->add("::copy(&" + capture.name + "),");
        break;
      case LambdaCaptureType::IMPLICIT_COPY:
        buffer->add("::implicit_copy(&" + capture.name + "),");
        break;
      case LambdaCaptureType::REFERENCE:
        buffer->add("&" + capture.name + ",");
        break;
    }
  } if (!captureInfo.captures.empty())
    buffer->pop_back();
  buffer->add("}");
}

void LambdaType::codegenDefinition(Buffer* buffer, Sections* sections) {
  buffer->add("struct " + getCodegenName() + " {");
  for (auto& capture : captures)
    buffer->newLine(capture.type->getCodegenName() + " " + capture.name + ";");
  buffer->newLine("};\n");
}

void CountOfExpression::codegen(Buffer*, Sections*) const {
  FATAL << "Attempting to codegen countof expression";
}

void VariablePackElement::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add(*codegenName);
}

static void codegenMember(Buffer* buffer, Sections* sections, FunctionInfo* destructorCall,
    bool isMainDestructor, const string& identifier, const Expression& lhs, bool isUnion) {
  buffer->add("(");
  if (destructorCall) {
    if (isMainDestructor)
      buffer->add("(*get_temporary_holder(");
    else
      buffer->add("{auto&& tmp = ");
    sections->registerFunctionCall(destructorCall);
  }
  lhs.codegen(buffer, sections);
  if (destructorCall) {
    if (isMainDestructor)
      buffer->add(", &::" + destructorCall->getMangledName() + "))." + identifier + ")");
    else
      buffer->add(";" + destructorCall->getMangledName() + "(&tmp); std::move(tmp)." + identifier +";})");
  } else
    buffer->add(")."s + (isUnion ? unionEntryPrefix : "") + identifier);
}

void MemberAccessExpression::codegen(Buffer* buffer, Sections* sections) const {
  codegenMember(buffer, sections, destructorCall, isMainDestructor, identifier, *lhs, isUnion);
}

void TernaryExpression::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("(");
  condExpr->codegen(buffer, sections);
  buffer->add(")?(");
  e1->codegen(buffer, sections);
  buffer->add("):(");
  e2->codegen(buffer, sections);
  buffer->add(")");
}

void FatPointerConversion::codegen(Buffer* buffer, Sections* sections) const {
  sections->registerFatPointer(conceptType, argType, functions);
  if (dynamic_cast<PointerType*>(toType))
    buffer->add("make_const_fat_ptr(");
  else
    buffer->add("make_fat_ptr(");
  arg->codegen(buffer, sections);
  buffer->add(", &" + getVTableName(*conceptType->getMangledName())
      + "_" + *argType->getMangledName() + ")");
}

void UncheckedStatement::codegen(Buffer* buffer, Sections* sections) const {
  elem->codegen(buffer, sections);
}

void AttributeDefinition::codegen(Buffer*, Sections*) const {
}

void ExternalStatement::codegen(Buffer* buffer, Sections* sections) const {
  elem->codegen(buffer, sections);
}

void StatementExpression::codegen(Buffer* buffer, Sections* sections) const {
  buffer->add("({");
  for (auto& s : statements) {
    s->codegen(buffer, sections);
    buffer->add("; ");
  }
  value->codegen(buffer, sections);
  buffer->add(";})");
}

void MixinStatement::codegen(Buffer* buffer, Sections* sections) const {
  result->codegen(buffer, sections);
}

void StaticStatement::codegen(Buffer* buffer, Sections* sections) const {
  for (auto& elem : results)
    elem->codegen(buffer, sections);
}
