#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "identifier.h"
#include "function_name.h"

struct IdentifierInfo;
struct Type;
struct FunctionType;
struct Concept;
class Context;
using SContext = shared_ptr<Context>;
using SConcept = shared_ptr<Concept>;
using SConstContext = shared_ptr<const Context>;

class Context : public owned_object<Context> {
  public:
  static Context withParent(const Context&);
  static Context withParent(vector<Context*>);
  void merge(const Context&);
  void mergeAndCollapse(const Context&);
  Context();
  Context(const Context&) = delete;
  Context(Context&&) = default;
  void deepCopyFrom(const Context&);
  vector<FunctionType> getMissingFunctions(const Context&) const;
  WithError<SType> getTypeOfVariable(const string&) const;
  optional<string> setVariableAsMoved(const string&) const;
  void addVariable(const string& ident, SType);
  const vector<string>& getBottomLevelVariables() const;
  void replace(SType from, SType to);
  nullable<SType> getReturnType() const;
  void setReturnType(SType);
  void addType(const string& name, SType);
  WithError<SType> getTypeFromString(IdentifierInfo) const;
  [[nodiscard]] optional<string> addFunction(FunctionType);
  WithError<vector<FunctionType>> getFunctionTemplate(IdentifierInfo) const;
  WithErrorLine<FunctionType> instantiateFunctionTemplate(CodeLoc, FunctionType, IdentifierInfo, vector<SType> argTypes, vector<CodeLoc> argLoc) const;
  vector<FunctionType> getOperatorType(Operator) const;
  FunctionId getFunctionId(const FunctionName& name) const;
  bool canConstructWith(SType, vector<SType> args) const;
  bool canCopyConstruct(SType) const;
  [[nodiscard]] optional<string> addCopyConstructorFor(SType, const vector<SType>& templateParams = {});
  void pushImport(const string& name);
  void popImport();
  const vector<string>& getImports() const;
  const vector<string>& getAllImports() const;
  void checkNameConflict(CodeLoc loc, const string& name, const string& type) const;
  void checkNameConflictExcludingFunctions(CodeLoc loc, const string& name, const string& type) const;
  vector<SType> getTypeList(const vector<IdentifierInfo>&) const;
  void addConcept(const string& name, SConcept);
  nullable<SConcept> getConcept(const string& name) const;
  void print() const;

  private:

  struct State : public owned_object<State> {
    map<string, SType> vars;
    vector<string> varsList;
    set<string> movedVars;
    map<string, SType> types;
    map<FunctionId, vector<FunctionType>> functions;
    nullable<SType> returnType;
    vector<string> imports;
    vector<string> allImports;
    map<string, shared_ptr<Concept>> concepts;
    void merge(const State&);
    void print() const;
  };
  vector<shared_ptr<const State>> parentStates;
  shared_ptr<State> state;
  vector<shared_ptr<const State>> getReversedStates() const;
  const State& getTopState() const;
  nullable<SType> getType(const string&) const;
  vector<FunctionType> getFunctions(FunctionId) const;
  nullable<SType> getVariable(const string&) const;
  bool areParamsEquivalent(const FunctionType&, const FunctionType&) const;
  bool isGeneralization(const FunctionType& general, const FunctionType& specific) const;
  vector<FunctionType> getAllFunctions() const;
};
