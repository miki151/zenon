#include "type.h"
#include "identifier.h"

const static unordered_map<string, ArithmeticType> arithmeticTypes {
  {"int", ArithmeticType::INT},
  {"bool", ArithmeticType::BOOL},
  {"void", ArithmeticType::VOID},
};

string getTemplateParamNames(const vector<Type>& templateParams) {
  string ret;
  if (!templateParams.empty()) {
    auto paramNames = transform(templateParams, [](auto& e) { return getName(e); });
    ret += "<" + combine(paramNames, ",") + ">";
  }
  return ret;
}

string getName(const Type& t) {
  return t.visit(
      [&](const ArithmeticType& t) {
        for (auto& elem : arithmeticTypes)
          if (elem.second == t)
            return elem.first;
        FATAL << "Type not recognized: " << (int)t;
        return ""s;
      },
      [&](const FunctionType& t) {
        return "function"s + getTemplateParamNames(t.templateParams);
      },
      [&](const MemberAccess& m) {
        return "member " + quote(m.memberName);
      },
      [&](const ReferenceType& t) {
        return "reference("s + getName(*t.underlying) + ")";
      },
      [&](const StructType& t) {
        return t.name + getTemplateParamNames(t.templateParams);
      },
      [&](const VariantType& t) {
        return t.name + getTemplateParamNames(t.templateParams);
      },
      [&](const TemplateParameter& t) {
        return t.name;
      }
  );
}

int getNewId() {
  static int idCounter = 0;
  return ++idCounter;
}

FunctionType::FunctionType(FunctionCallType t, Type returnType, vector<Param> p, vector<Type> tpl)
    : callType(t), retVal(std::move(returnType)), params(std::move(p)), templateParams(tpl), id(getNewId()) {
}

bool FunctionType::operator == (const FunctionType& o) const {
  return id == o.id;
}

Type getUnderlying(Type type) {
  return type.visit(
      [&](const ReferenceType& t) -> Type {
        return getUnderlying(*t.underlying);
      },
      [&](const auto& t) -> Type {
        return t;
      }
  );
};

ReferenceType::ReferenceType(Type t) : underlying(getUnderlying(t)) {
}

bool ReferenceType::operator == (const ReferenceType& o) const {
  return underlying == o.underlying;
}

static optional<Type> getOperationResult(Operator op, const Type& underlyingOperands) {
  switch (op) {
    case Operator::PLUS:
    case Operator::MINUS:
      if (underlyingOperands == ArithmeticType::INT)
        return Type(ArithmeticType::INT);
      return none;
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
      if (underlyingOperands == ArithmeticType::INT)
        return Type(ArithmeticType::BOOL);
      return none;
    case Operator::EQUALS:
      if (underlyingOperands == ArithmeticType::INT || underlyingOperands == ArithmeticType::BOOL)
        return Type(ArithmeticType::BOOL);
      return none;
    default:
      return none;
  }
}

Type getOperationResult(CodeLoc codeLoc, Operator op, const Type& left, const Type& right) {
  switch (op) {
    case Operator::MEMBER_ACCESS:
      if (auto memberInfo = right.getReferenceMaybe<MemberAccess>()) {
        auto leftUnderlying = getUnderlying(left);
        if (auto structInfo = leftUnderlying.getReferenceMaybe<StructType>()) {
          for (auto& member : structInfo->members) {
            if (member.name == memberInfo->memberName) {
              auto ret = *member.type;
              if (left.contains<ReferenceType>())
                ret = ReferenceType(std::move(ret));
              return ret;
            }
          }
          codeLoc.error("No member named " + quote(memberInfo->memberName + " in struct " + quote(getName(*structInfo))));
          return {};
        }
      }
      codeLoc.error("Bad use of operator " + quote("."));
      return {};
    case Operator::ASSIGNMENT:
      if (getUnderlying(left) == getUnderlying(right) && left.contains<ReferenceType>())
        return left;
      else {
        codeLoc.error("Can't assign " + quote(getName(right)) + " to " + quote(getName(left)));
        return {};
      }
    case Operator::LESS_THAN:
    case Operator::MORE_THAN:
    case Operator::PLUS:
    case Operator::EQUALS:
    case Operator::MINUS: {
      auto operand = getUnderlying(left);
      if (operand == getUnderlying(right))
        if (auto res = getOperationResult(op, operand))
          return *res;
      codeLoc.error("Unsupported operator: " + quote(getName(left)) + " " + getString(op)
          + " " + quote(getName(right)));
      return {};
    }
  }
}

bool canAssign(const Type& to, const Type& from) {
  //INFO << "can assign " << getName(from) << " to " << getName(to);
  return to.visit(
      [&](const ReferenceType& t) {
        return getUnderlying(from) == *t.underlying;
      },
      [&](const auto&) {
        return false;
      }
  );
}

bool canBind(const Type& to, const Type& from) {
  //INFO << "can bind " << getName(from) << " to " << getName(to);
  return to == from || from == ReferenceType(to);
}

StructType::StructType(string n) : name(n), id(getNewId()) {}

bool StructType::operator == (const StructType& o) const {
  INFO << "Comparing struct " << getName(*this) << " id " << id << " and " << getName(o) << " id " << o.id;
  for (int i = 0; i < members.size(); ++i) {
    if (!(members[i].type == o.members[i].type))
      return false;
  }
  return id == o.id;
}

MemberAccess::MemberAccess(const string& m) : memberName(m), id(getNewId()) {}

bool MemberAccess::operator == (const MemberAccess& a) const {
  return id == a.id;
}

bool canConvert(const Type& from, const Type& to) {
  return getUnderlying(from) == to;
}

bool requiresInitialization(const Type&) {
  return true;
}

VariantType::VariantType(string n) : name(n), id(getNewId()) {}

bool VariantType::operator ==(const VariantType& o) const {
  INFO << "Comparing " << getName(*this) << " id " << id << " with " << getName(o) << " id " << o.id;
  if (types.size() == o.types.size())
    INFO << "Types equal";
  else
    INFO << "Types different";
  return id == o.id && types == o.types;
}

TemplateParameter::TemplateParameter(string n) : name(n), id(getNewId()) {}

bool TemplateParameter::operator ==(const TemplateParameter& t) const {
  return id == t.id;
}

extern void replace(Type& in, Type from, Type to, IdentifierInfo toName);
void replace(FunctionType&, Type from, Type to, IdentifierInfo toName);

void replace(Type& in, Type from, Type to, IdentifierInfo toName) {
  in.visit(
      [&](ReferenceType& t) {
        replace(*t.underlying, from, to, toName);
      },
      [&](StructType& t) {
        for (auto& param : t.templateParams)
          replace(param, from, to, toName);
        for (int i = 0; i < t.members.size(); ++i)
          replace(*t.members[i].type, from, to, toName);
      },
      [&](VariantType& t) {
        for (auto& param : t.templateParams)
          replace(param, from, to, toName);
        for (auto& subtype : t.types)
          replace(subtype.second, from, to, toName);
        for (auto& method : t.staticMethods)
          replace(method.second, from, to, toName);
      },
      [&](TemplateParameter& t) {
        if (from == t)
          in = to;
      },
      [](auto) {
      }
  );
}

void replace(FunctionType& in, Type from, Type to, IdentifierInfo toName) {
  replace(*in.retVal, from, to, toName);
  for (auto& param : in.params)
    replace(*param.type, from, to, toName);
}

optional<Type> instantiate(const Type& type, vector<Type> templateParams, vector<IdentifierInfo> templateParamNames) {
  return type.visit(
      [&](StructType type) -> optional<Type> {
        if (templateParams.size() != type.templateParams.size())
          return none;
        Type ret(type);
        for (int i = 0; i < templateParams.size(); ++i)
          replace(ret, type.templateParams[i], templateParams[i], templateParamNames[i]);
        return ret;
      },
      [&](VariantType type) -> optional<Type> {
        if (templateParams.size() != type.templateParams.size())
          return none;
        Type ret(type);
        for (int i = 0; i < templateParams.size(); ++i)
          replace(ret, type.templateParams[i], templateParams[i], templateParamNames[i]);
        return ret;
      },
      [&](const auto&) -> optional<Type> {
         if (templateParams.empty())
           return type;
         else
           return none;
      }
  );
}

optional<FunctionType> instantiate(FunctionType type, vector<Type> templateParams,
    vector<IdentifierInfo> templateParamNames) {
  if (templateParams.size() != type.templateParams.size())
    return none;
  for (int i = 0; i < templateParams.size(); ++i)
    replace(type, type.templateParams[i], templateParams[i], templateParamNames[i]);
  return type;
}

optional<FunctionType> getStaticMethod(const Type& type, string name) {
  return type.visit(
      [&](const VariantType& t) -> optional<FunctionType> {
        for (auto& elem : t.staticMethods)
          if (elem.first == name)
            return elem.second;
        return none;
      },
      [](const auto&) -> optional<FunctionType> {return none;}
  );
}

