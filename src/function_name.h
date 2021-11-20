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

namespace std {
template <>
struct hash<FunctionId> {
  size_t operator()(const FunctionId& x) const {
    return x.visit(
        [](const string& s) { return std::hash<string>()(s); },
        [](Operator o) { return size_t(o); },
        [](ConstructorTag t) { return size_t(123); },
        [](AttributeTag t) { return size_t(124); },
        [](StructMembersTag t) { return size_t(125); }
    );
  }
};
}
