#include "stdafx.h"
#include "type_registry.h"
#include "type.h"

JustError<string> TypeRegistry::addStruct(const string& name, bool external, CodeLoc definition) {
  TRY(checkNameConflict(name));
  auto s = shared<StructType>(name, StructType::Private{});
  s->parent = s;
  s->external = external;
  s->definition = definition;
  structs.insert(make_pair(name, s));
  return success;
}

JustError<string> TypeRegistry::addEnum(const string& name, bool external, CodeLoc definition) {
  TRY(checkNameConflict(name));
  auto e = shared<EnumType>(name, EnumType::Private{});
  e->external = external;
  e->definition = definition;
  enums.insert(make_pair(name, e));
  return success;
}

nullable<shared_ptr<StructType>> TypeRegistry::getStruct(const string& name) const {
  if (auto s = getValueMaybe(structs, name))
    return *s;
  return nullptr;
}

nullable<shared_ptr<EnumType>> TypeRegistry::getEnum(const string& name) const {
  if (auto s = getValueMaybe(enums, name))
    return *s;
  return nullptr;
}

nullable<SType> TypeRegistry::getType(const string& name) const {
  if (auto s = getStruct(name))
    return SType(s.get());
  if (auto s = getEnum(name))
    return SType(s.get());
  return nullptr;
}

JustError<string> TypeRegistry::checkNameConflict(const string& name) const {
  if (auto s = getStruct(name))
    return quote(name) + " conflicts with existing type declared at: " + s->definition->toString();
  if (auto s = getEnum(name))
    return quote(name) + " conflicts with existing type declared at: " + s->definition->toString();
  return success;
}

vector<shared_ptr<StructType>> TypeRegistry::getAllStructs() const {
  vector<shared_ptr<StructType>> ret;
  for (auto& elem : structs)
    ret.push_back(elem.second);
  return ret;
}

vector<shared_ptr<EnumType>> TypeRegistry::getAllEnums() const {
  vector<shared_ptr<EnumType>> ret;
  for (auto& elem : enums)
    ret.push_back(elem.second);
  return ret;
}
