#include "state.h"

const static unordered_map<string, ArithmeticType> types {
  {"int", ArithmeticType::INT},
  {"bool", ArithmeticType::BOOL},
  {"void", ArithmeticType::VOID},
};

optional<Type> State::getType(const string& ident) const {
  if (vars.count(ident))
    return vars.at(ident);
  else
    return none;
}

void State::setType(const string& ident, Type t) {
  vars[ident] = t;
}

const optional<ArithmeticType>&State::getReturnType() const {
  return returnType;
}

void State::setReturnType(ArithmeticType t) {
  CHECK(!returnType) << "Attempted to overwrite return type";
  returnType = t;
}

optional<ArithmeticType> getType(const string& s) {
  if (types.count(s))
    return types.at(s);
  else
    return none;
}

const char* getName(ArithmeticType type) {
  for (auto& elem : types)
    if (elem.second == type)
      return elem.first.c_str();
  FATAL << "Unrecognized type " << (int)type;
  return "dupa";
}
