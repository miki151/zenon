#include "stdafx.h"
#include "type_registry.h"
#include "type.h"

shared_ptr<StructType> TypeRegistry::getStruct(const string& name) {
  if (!structs.count(name)) {
    auto s = shared<StructType>(name, StructType::Private{});
    s->parent = s;
    structs.insert(make_pair(name, std::move(s)));
  }
  return structs.at(name);
}

shared_ptr<EnumType> TypeRegistry::getEnum(const string& name) {
  if (!enums.count(name))
    enums.insert(make_pair(name, shared<EnumType>(name, EnumType::Private{})));
  return enums.at(name);
}

const vector<shared_ptr<LambdaType>>& TypeRegistry::getLambdas() const {
  return lambdas;
}

void TypeRegistry::addLambda(shared_ptr<LambdaType> l) {
  lambdas.push_back(std::move(l));
}
