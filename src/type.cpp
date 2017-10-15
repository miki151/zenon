#include "type.h"

const static unordered_map<string, ArithmeticType> arithmeticTypes {
  {"int", ArithmeticType::INT},
  {"bool", ArithmeticType::BOOL},
  {"void", ArithmeticType::VOID},
};

string getName(const Type& t) {
  return t.visit(
      [&](const ArithmeticType& t) {
        for (auto& elem : arithmeticTypes)
          if (elem.second == t)
            return elem.first;
        FATAL << "Type not recognized: " << (int)t;
        return ""s;
      },
      [&](const FunctionType&) {
        return "function"s;
      },
      [&](const MemberAccess& m) {
        return "member " + quote(m.memberName);
      },
      [&](const ReferenceType& t) {
        return "reference("s + getName(*t.underlying) + ")";
      },
      [&](const StructType& t) {
        return t.name;
      },
      [&](const VariantType& t) {
        return "variant " + t.name;
      }
  );
}

int getNewId() {
  static int idCounter = 0;
  return ++idCounter;
}

FunctionType::FunctionType(FunctionCallType t, Type returnType, vector<Param> p) : callType(t),
    retVal(std::move(returnType)), params(std::move(p)), id(getNewId()) {
}

bool FunctionType::operator == (const FunctionType& o) const {
  return id == o.id;
}

static Type getUnderlying(Type type) {
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
  return to.visit(
      [&](const ReferenceType& t) {
        return getUnderlying(from) == *t.underlying;
      },
      [&](const auto&) {
        return false;
      }
  );
}

StructType::StructType(string n) : name(n), id(getNewId()) {}

bool StructType::operator == (const StructType& o) const {
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
  return id == o.id;
}
