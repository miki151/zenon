#pragma once

#include "util.h"
#include "code_loc.h"

class StructType;
class EnumType;
struct LambdaType;
struct Type;
struct Concept;
struct EnumDefinition;

class TypeRegistry {
  public:
  JustError<string> addStruct(const string& name, bool external, bool isUnion, CodeLoc definition);
  JustError<string> addEnum(const string& name, bool external, EnumDefinition*);
  void addAlias(const string& name, Type*, CodeLoc);
  void addConcept(const string& name, Concept*);
  StructType* getStruct(const string& name) const;
  EnumType* getEnum(const string& name) const;
  Type* getType(const string& name) const;
  Concept* getConcept(const string& name) const;
  vector<StructType*> getAllStructs() const;
  vector<EnumType*> getAllEnums() const;
  optional<CodeLoc> getAliasCodeLoc(const string& name) const;

  private:
  JustError<string> checkNameConflict(const string& name) const;
  map<string, StructType*> structs;
  map<string, EnumType*> enums;
  map<string, pair<CodeLoc, Type*>> aliases;
  map<string, Concept*> concepts;
};
