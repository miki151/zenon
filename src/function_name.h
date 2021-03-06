#pragma once

#include "stdafx.h"
#include "operator.h"


struct ConstructorTag {
  COMPARABLE(ConstructorTag)
};
struct AttributeTag {
  COMPARABLE(AttributeTag)
};
struct StructMembersTag {
  COMPARABLE(StructMembersTag)
};
using FunctionId = variant<string, Operator, ConstructorTag, AttributeTag, StructMembersTag>;

inline string toString(const FunctionId& id) {
  return id.visit(
      [&](const string& s) { return s; },
      [&](Operator op) { return getString(op); },
      [&](ConstructorTag) { return "constructor"; },
      [&](AttributeTag) { return "attribute tag"; },
      [&](StructMembersTag) { return "struct members tag"; }
  );
}
