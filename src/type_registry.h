#pragma once

#include "util.h"

class StructType;
class EnumType;

class TypeRegistry {
  public:
  shared_ptr<StructType> getStruct(const string& name);
  shared_ptr<EnumType> getEnum(const string& name);

  private:
  map<string, shared_ptr<StructType>> structs;
  map<string, shared_ptr<EnumType>> enums;
};
