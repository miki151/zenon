#include "stdafx.h"
#include "variables.h"
#include "type.h"

void Variables::merge(const Variables& other) {
  for (auto& var : other.varsList)
    add(var, other.getType(var).get());
}

nullable<SType> Variables::getType(const string& name) const {
  if (vars.count(name))
    return vars.at(name);
  else
    return nullptr;
}

void Variables::add(const string& id, SType t) {
  CHECK(!vars.count(id));
  vars.insert({id, t});
  varsList.push_back(id);
}

const vector<string>& Variables::getNames() const {
  return varsList;
}

void Variables::replace(SType from, SType to) {
  for (auto& varName : varsList) {
    auto& var = vars.at(varName);
    var = var->replace(from, to);
  }
}
