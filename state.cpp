#include "state.h"

optional<Type> State::getType(const string& ident) const {
  if (vars.count(ident))
    return vars.at(ident);
  else
    return none;
}

void State::setType(const string& ident, Type t) {
  vars[ident] = t;
}

const optional<Type>& State::getReturnType() const {
  return returnType;
}

void State::setReturnType(Type t) {
  CHECK(!returnType) << "Attempted to overwrite return type";
  returnType = t;
}
