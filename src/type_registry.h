#pragma once

#include "util.h"
#include "code_loc.h"

class StructType;
class EnumType;
struct LambdaType;
struct Type;
using SType = shared_ptr<Type>;

class TypeRegistry {
  public:
  JustError<string> addStruct(const string& name, bool external, CodeLoc definition);
  JustError<string> addEnum(const string& name, bool external, CodeLoc definition);
  nullable<shared_ptr<StructType>> getStruct(const string& name) const;
  nullable<shared_ptr<EnumType>> getEnum(const string& name) const;
  nullable<SType> getType(const string& name) const;
  vector<shared_ptr<StructType>> getAllStructs() const;
  vector<shared_ptr<EnumType>> getAllEnums() const;

  private:
  JustError<string> checkNameConflict(const string& name) const;
  map<string, shared_ptr<StructType>> structs;
  map<string, shared_ptr<EnumType>> enums;
};
