#pragma once

#include "util.h"

class StructType;
class EnumType;
struct LambdaType;

class TypeRegistry {
  public:
  shared_ptr<StructType> getStruct(const string& name);
  shared_ptr<EnumType> getEnum(const string& name);
  const vector<shared_ptr<LambdaType>>& getLambdas() const;
  void addLambda(shared_ptr<LambdaType>);

  private:
  map<string, shared_ptr<StructType>> structs;
  map<string, shared_ptr<EnumType>> enums;
  vector<shared_ptr<LambdaType>> lambdas;
};
