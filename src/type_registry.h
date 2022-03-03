#pragma once

#include "util.h"
#include "code_loc.h"

class StructType;
class EnumType;
struct LambdaType;
struct Type;

class TypeRegistry {
  public:
  JustError<string> addStruct(const string& name, bool external, CodeLoc definition);
  JustError<string> addEnum(const string& name, bool external, CodeLoc definition);
  void addAlias(const string& name, Type*);
  StructType* getStruct(const string& name) const;
  EnumType* getEnum(const string& name) const;
  Type* getType(const string& name) const;
  vector<StructType*> getAllStructs() const;
  vector<EnumType*> getAllEnums() const;

  private:
  JustError<string> checkNameConflict(const string& name) const;
  map<string, StructType*> structs;
  map<string, EnumType*> enums;
  map<string, Type*> aliases;
};
