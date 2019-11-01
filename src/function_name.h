#pragma once

#include "stdafx.h"
#include "operator.h"


struct ConstructorTag {
  COMPARABLE(ConstructorTag)
};
using FunctionId = variant<string, Operator, ConstructorTag>;

inline string toString(const FunctionId& id) {
  return id.visit(
        [&](const string& s) { return s; },
        [&](Operator op) { return getString(op); },
        [&](ConstructorTag) { return "constructor"; }
  );
}
