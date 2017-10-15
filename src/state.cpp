#include "state.h"
#include "ast.h"

optional<Type> State::getTypeOfVariable(const IdentifierInfo& ident) const {
  if (vars.count(ident))
    return vars.at(ident);
  else
    return none;
}

void State::setType(const IdentifierInfo& ident, Type t) {
  vars[ident] = t;
}

const optional<Type>& State::getReturnType() const {
  return returnType;
}

void State::setReturnType(Type t) {
  CHECK(!returnType) << "Attempted to overwrite return type";
  returnType = t;
}

void State::addType(const string& name, Type t) {
  types[name] = t;
}

optional<Type> State::getTypeFromString(const string& ident) const {
  if (types.count(ident))
    return types.at(ident);
  else
    return none;
}
