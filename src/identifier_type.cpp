#include "identifier_type.h"
#include "type.h"

IdentifierType::IdentifierType(string name) {
  parts.push_back({std::move(name)});
}

IdentifierType::IdentifierType(SType type) {
  parts.push_back({std::move(type)});
}

IdentifierType IdentifierType::replace(SType from, SType to, ErrorBuffer& errors) const {
  IdentifierType ret;
  for (auto& part : parts)
    ret.parts.push_back(part.name.visit(
        [&](const SType& type) -> Part { return Part { type->replace(from, to, errors) }; },
        [&](const string& name) -> Part { return Part { name }; }
    ));
  return ret;
}

IdentifierType IdentifierType::getWithoutFirstPart() const {
  CHECK(parts.size() > 1);
  IdentifierType ret;
  for (int i = 1; i < parts.size(); ++i)
    ret.parts.push_back(parts[i]);
  return ret;
}

string IdentifierType::prettyString() const {
  string ret;
  for (auto& part : parts) {
    if (!ret.empty())
      ret += "::";
    part.name.visit(
          [&](const SType& t) { ret += t->getName(); },
          [&](const string& s) { ret += s; }
    );
  }
  return ret;
}
