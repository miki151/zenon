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
      [&](const ReferenceType& t) {
        return "reference("s + getName(*t.underlying) + ")";
      }
  );
}

optional<ArithmeticType> getArithmeticType(const string& s) {
  if (arithmeticTypes.count(s))
    return ArithmeticType{arithmeticTypes.at(s)};
  else
    return none;
}

FunctionType::FunctionType(Type returnType, vector<Type> p) : retVal(std::move(returnType)), params(std::move(p)) {
}

bool FunctionType::operator == (const FunctionType& o) const {
  return retVal == o.retVal && params == o.params;
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

optional<Type> getOperationResult(char op, const Type& left, const Type& right) {
  switch (op) {
    case '=':
      if (getUnderlying(left) == getUnderlying(right) && left.contains<ReferenceType>())
        return left;
      else
        return none;
    case '+':
    case '-':
      if (getUnderlying(left) == ArithmeticType::INT && getUnderlying(right) == ArithmeticType::INT)
        return Type(ArithmeticType::INT);
      else
        return none;
    case '<':
    case '>':
      if (getUnderlying(left) == ArithmeticType::INT && getUnderlying(right) == ArithmeticType::INT)
        return Type(ArithmeticType::BOOL);
      else
        return none;
    default:
      FATAL << "Unrecognized operator: " + string(1, op);
      return {};
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
