#pragma once

#include "stdafx.h"
#include "operator.h"

struct ConstructorId {
  ConstructorId(string name) : name(name) {}
  string name;
  bool operator < (ConstructorId o) const {
    return name < o.name;
  }
};
using FunctionName = variant<string, Operator, ConstructorId>;

struct ConstructorTag {
  bool operator < (ConstructorTag) const {
    return false;
  }
  bool operator == (ConstructorTag) const {
    return true;
  }
};
using FunctionId = variant<string, Operator, ConstructorTag>;

inline string toString(const FunctionId& id) {
  return id.visit(
        [&](const string& s) { return s; },
        [&](Operator op) { return getString(op); },
        [&](ConstructorTag) { return "constructor"; }
  );
}
