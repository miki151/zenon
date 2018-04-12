#include "type.h"
#include "state.h"

SType ArithmeticType::INT = make_shared<Type>(ArithmeticType{});
SType ArithmeticType::VOID = make_shared<Type>(ArithmeticType{});
SType ArithmeticType::BOOL = make_shared<Type>(ArithmeticType{});
SType ArithmeticType::STRING = make_shared<Type>(ArithmeticType{});
SType ArithmeticType::CHAR = make_shared<Type>(ArithmeticType{});

string getTemplateParamNames(const vector<SType>& templateParams) {
  string ret;
  if (!templateParams.empty()) {
    auto paramNames = transform(templateParams, [](auto& e) { return getName(e); });
    ret += "<" + combine(paramNames, ",") + ">";
  }
  return ret;
}

string getName(SType t) {
  return t->visit(
      [&](const ArithmeticType&) -> string {
        if (t == ArithmeticType::INT)
          return "int";
        if (t == ArithmeticType::VOID)
          return "void";
        if (t == ArithmeticType::BOOL)
          return "bool";
        if (t == ArithmeticType::STRING)
          return "string";
        if (t == ArithmeticType::CHAR)
          return "char";
        FATAL << "Unknown arithmetic type";
        return "error";
      },
      [&](const FunctionType& t) {
        return "function"s + getTemplateParamNames(t.templateParams);
      },
      [&](const ReferenceType& t) {
        return "reference("s + getName(t.underlying) + ")";
      },
      [&](const PointerType& t) {
        return getName(t.underlying) + "*";
      },
      [&](const StructType& t) {
        return t.name + getTemplateParamNames(t.templateParams);
      },
      [&](const TemplateParameter& t) {
        return t.name;
      },
      [&](const EnumType& t) {
        return t.name;
      }
  );
}

int getNewId() {
  static int idCounter = 0;
  return ++idCounter;
}

FunctionType::FunctionType(FunctionCallType t, SType returnType, vector<Param> p, vector<SType> tpl)
    : callType(t), retVal(std::move(returnType)), params(std::move(p)), templateParams(tpl) {
}

SType getUnderlying(SType type) {
  return type->visit(
      [&](const ReferenceType& t) -> SType {
        return getUnderlying(t.underlying);
      },
      [&](const auto&) -> SType {
        return type;
      }
  );
};

SType ReferenceType::get(SType type) {
  static map<SType, SType> generated;
  if (!generated.count(type))
    generated.insert({type, make_shared<Type>(ReferenceType(type))});
  return generated.at(type);
}

ReferenceType::ReferenceType(SType t) : underlying(getUnderlying(t)) {
}

SType PointerType::get(SType type) {
  static map<SType, SType> generated;
  if (!generated.count(type))
    generated.insert({type, make_shared<Type>(PointerType(type))});
  return generated.at(type);
}

PointerType::PointerType(SType t) : underlying(getUnderlying(t)) {
}

bool canAssign(SType to, SType from) {
  //INFO << "can assign " << getName(from) << " to " << getName(to);
  return to->visit(
      [&](const ReferenceType& t) {
        return getUnderlying(from) == t.underlying;
      },
      [&](const auto&) {
        return false;
      }
  );
}

bool canBind(SType to, SType from) {
  //INFO << "can bind " << getName(from) << " to " << getName(to);
  return to == from ||
      to->visit(
            [&](const ReferenceType& t) {
              return from == t.underlying;
            },
            [&](const auto&) {
              return false;
            }
      );
}

SType StructType::get(Kind kind, string name) {
  auto ret = make_shared<Type>(StructType{});
  auto& type = *ret->getReferenceMaybe<StructType>();
  type.kind = kind;
  type.name = name;
  type.parent = ret;
  return ret;
}

nullable<SType> StructType::getMember(const string& name) const {
  for (auto& member : members)
    if (member.name == name)
      return member.type;
  return nullptr;
}

State StructType::getContext() const {
  State state;
  if (kind != VARIANT)
    for (auto& member : members)
      state.addVariable(member.name, ReferenceType::get(member.type));
  for (auto& method : methods)
    state.addFunction(method.nameOrOp, *method.type);
  return state;
}

SType StructType::instantiate(SType self, vector<SType> newTemplateParams) {
  if (templateParams == newTemplateParams)
    return self;
  for (auto type : instantations) {
    auto& structType = *type->getReferenceMaybe<StructType>();
    if (structType.templateParams == newTemplateParams) {
      INFO << "Found instantiated type " << getName(type);
      return type;
    }
  }
  auto type = StructType::get(kind, name);
  auto& structType = *type->getReferenceMaybe<StructType>();
  structType.parent = self;
  instantations.push_back(type);
  INFO << "New instantiation: " << getName(type);
  return type;
}

void replaceInFunction(FunctionType&, SType from, SType to);

SType replace(SType in, SType from, SType to);

void StructType::updateInstantations() {
  for (auto type : instantations) {
    auto& structType = *type->getReferenceMaybe<StructType>();
    for (int i = 0; i < templateParams.size(); ++i) {
      structType.methods = methods;
      for (auto& method : structType.methods)
        replaceInFunction(*method.type, templateParams[i], structType.templateParams[i]);
      structType.staticMethods = staticMethods;
      for (auto& method : structType.staticMethods)
        replaceInFunction(method.second, templateParams[i], structType.templateParams[i]);
      structType.members = members;
      for (auto& member : structType.members)
        member.type = replace(member.type, templateParams[i], structType.templateParams[i]);
    }
  }
}

bool canConvert(SType from, SType to) {
  return getUnderlying(from) == to;
}

bool requiresInitialization(SType) {
  return true;
}

TemplateParameter::TemplateParameter(string n) : name(n) {}

SType replace(SType in, SType from, SType to) {
  return in->visit(
      [&](const ReferenceType& t) {
        return ReferenceType::get(replace(t.underlying, from, to));
      },
      [&](const PointerType& t) {
        return PointerType::get(replace(t.underlying, from, to));
      },
      [&](StructType& t) {
        vector<SType> templateParams;
        for (auto& param : t.templateParams)
          templateParams.push_back(replace(param, from, to));
        auto ret = t.parent->getReferenceMaybe<StructType>()->instantiate(t.parent.get(), templateParams);
        auto& structType = *ret->getReferenceMaybe<StructType>();
        // This is how we check if instantiate gave us a new type to fill
        if (structType.templateParams != templateParams) {
          structType.templateParams = templateParams;
          for (auto& member : t.members)
            structType.members.push_back({member.name, replace(member.type, from, to)});
          for (auto& method : t.methods) {
            structType.methods.push_back(method);
            replaceInFunction(*structType.methods.back().type, from, to);
          }
          for (auto& method : t.staticMethods) {
            structType.staticMethods.push_back(method);
            replaceInFunction(structType.staticMethods.back().second, from, to);
          }
        }
        return ret;
      },
      [&](TemplateParameter&) {
        if (from == in)
          return to;
        else
          return in;
      },
      [&](auto) {
        return in;
      }
  );
}

void replaceInFunction(FunctionType& in, SType from, SType to) {
  in.retVal = replace(in.retVal, from, to);
  for (auto& param : in.params)
    param.type = replace(param.type, from, to);
}

nullable<SType> instantiate(SType type, vector<SType> templateParams) {
  return type->visit(
      [&](StructType structType) -> nullable<SType> {
        if (templateParams.size() != structType.templateParams.size())
          return nullptr;
        SType ret = type;
        for (int i = 0; i < templateParams.size(); ++i)
          ret = replace(ret, structType.templateParams[i], templateParams[i]);
        return ret;
      },
      [&](const auto&) -> nullable<SType> {
         if (templateParams.empty())
           return type;
         else
           return nullptr;
      }
  );
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

bool canBind(TypeMapping& mapping, SType paramType, SType argType) {
  if (auto refType = argType->getValueMaybe<ReferenceType>()) {
    argType = refType->underlying;
    if (auto refType = paramType->getValueMaybe<ReferenceType>())
      paramType = refType->underlying;
  }
  if (auto index = mapping.getParamIndex(paramType)) {
    auto& arg = mapping.templateArgs.at(*index);
    if (arg && arg != argType)
      return false;
    arg = argType;
    return true;
  } else
    return paramType->visit(
        [&](PointerType& type) {
          if (auto argPointer = argType->getReferenceMaybe<PointerType>())
            return canBind(mapping, type.underlying, argPointer->underlying);
          return false;
        },
        [&](StructType& type) {
          auto argStruct = argType->getReferenceMaybe<StructType>();
          if (!argStruct || type.parent.get() != argStruct->parent.get())
            return false;
          for (int i = 0; i < type.templateParams.size(); ++i)
            if (!canBind(mapping, type.templateParams[i], argStruct->templateParams[i]))
              return false;
          return true;
        },
        [&](auto) {
          return canBind(paramType, argType);
        }
    );
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
    if (!canBind(mapping, funParams[i], argTypes[i])) {
      string deducedAsString;
      if (auto index = mapping.getParamIndex(funParams[i]))
        if (auto deduced = mapping.templateArgs.at(*index))
          deducedAsString = ", deduced as " + quote(getName(deduced.get()));
      argLoc[i].error("Can't bind argument of type "
        + quote(getName(argTypes[i])) + " to parameter " + quote(getName(funParams[i])) + deducedAsString);
    }
  for (int i = 0; i < type.templateParams.size(); ++i) {
    if (i >= templateArgs.size()) {
      if (auto deduced = mapping.templateArgs[i])
        templateArgs.push_back(deduced.get());
      else
        codeLoc.error("Couldn't deduce template argument " + quote(getName(type.templateParams[i])));
    }
    replaceInFunction(type, type.templateParams[i], templateArgs[i]);
    type.templateParams[i] = templateArgs[i];
  }
}

optional<FunctionType> getStaticMethod(const Type& type, string name) {
  return type.visit(
      [&](const StructType& t) -> optional<FunctionType> {
        for (auto& elem : t.staticMethods)
          if (elem.first == name)
            return elem.second;
        return none;
      },
      [](const auto&) -> optional<FunctionType> {return none;}
  );
}

EnumType::EnumType(string n, vector<string> e) : name(n), elements(e) {}
