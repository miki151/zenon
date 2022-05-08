#include "stdafx.h"
#include "type_registry.h"
#include "type.h"
#include "ast.h"

JustError<string> TypeRegistry::addStruct(const string& name, bool external, CodeLoc definition) {
  TRY(checkNameConflict(name));
  auto s = new StructType(name, StructType::Private{});
  s->parent = s;
  s->external = external;
  s->definition = definition;
  structs.insert(make_pair(name, s));
  return success;
}

JustError<string> TypeRegistry::addEnum(const string& name, bool external, EnumDefinition* definition) {
  TRY(checkNameConflict(name));
  auto e = new EnumType(name, EnumType::Private{});
  e->external = external;
  e->definition = definition;
  enums.insert(make_pair(name, e));
  return success;
}

void TypeRegistry::addAlias(const string& name, Type* t, CodeLoc l) {
  CHECK(!getType(name));
  aliases.insert(make_pair(name, make_pair(l, t)));
}

StructType* TypeRegistry::getStruct(const string& name) const {
  if (auto s = getValueMaybe(structs, name))
    return *s;
  return nullptr;
}

EnumType* TypeRegistry::getEnum(const string& name) const {
  if (auto s = getValueMaybe(enums, name))
    return *s;
  return nullptr;
}

optional<CodeLoc> TypeRegistry::getAliasCodeLoc(const string& name) const {
  if (auto s = getValueMaybe(aliases, name))
    return s->first;
  return none;
}

Type* TypeRegistry::getType(const string& name) const {
  if (auto s = getValueMaybe(aliases, name))
    return s->second;
  if (auto s = getStruct(name))
    return s;
  if (auto s = getEnum(name))
    return s;
  return nullptr;
}

Concept* TypeRegistry::getConcept(const string& name) const {
  if (auto s = getValueMaybe(concepts, name))
    return *s;
  return nullptr;
}

JustError<string> TypeRegistry::checkNameConflict(const string& name) const {
  if (auto s = getStruct(name))
    return quote(name) + " conflicts with existing type declared at: " + s->definition->toString();
  if (auto s = getEnum(name))
    return quote(name) + " conflicts with existing type declared at: " + s->definition->codeLoc.toString();
  return success;
}

vector<StructType*> TypeRegistry::getAllStructs() const {
  vector<StructType*> ret;
  for (auto& elem : structs)
    ret.push_back(elem.second);
  return ret;
}

vector<EnumType*> TypeRegistry::getAllEnums() const {
  vector<EnumType*> ret;
  for (auto& elem : enums)
    ret.push_back(elem.second);
  return ret;
}


void TypeRegistry::addConcept(const string& name, Concept* c) {
  concepts.insert(make_pair(name, c));
}
