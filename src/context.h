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
  Context();
  Context(const Context&) = delete;
  Context(Context&&) = default;
  void operator = (const Context&) = delete;
  void operator = (Context&&) = delete;
  void deepCopyFrom(const Context&);
  optional<string> getMissingFunctions(const Concept&, vector<FunctionType> existing) const;
  WithError<SType> getTypeOfVariable(const string&) const;
  optional<string> setVariableAsMoved(const string&);
  void addVariable(const string& ident, SType);
  void replace(SType from, SType to);
  nullable<SType> getReturnType() const;
  void setReturnType(SType);
  void addType(const string& name, SType);
  WithErrorLine<SType> getTypeFromString(IdentifierInfo) const;
  nullable<SType> getType(const string&) const;
  [[nodiscard]] optional<string> addFunction(FunctionType);
  WithError<vector<FunctionType>> getFunctionTemplate(IdentifierInfo) const;
  WithErrorLine<FunctionType> instantiateFunctionTemplate(CodeLoc, FunctionType, IdentifierInfo,
      vector<SType> argTypes, vector<CodeLoc> argLoc) const;
  nullable<SType> invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const;
  using BuiltInFunction = function<WithError<SType>(const Context&, vector<SType>)>;
  void addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction);
  vector<FunctionType> getOperatorType(Operator) const;
  optional<FunctionType> getBuiltinOperator(Operator, vector<SType> argTypes) const;
  FunctionId getFunctionId(const FunctionName& name) const;
  bool canConstructWith(SType, vector<SType> args) const;
  void checkNameConflict(CodeLoc loc, const string& name, const string& type) const;
  void checkNameConflictExcludingFunctions(CodeLoc loc, const string& name, const string& type) const;
  void addConcept(const string& name, SConcept);
  nullable<SConcept> getConcept(const string& name) const;
  void print() const;
  vector<SType> getConversions(SType) const;
  bool canConvert(SType from, SType to) const;
  bool breakAllowed() const;
  void setBreakAllowed();
  bool areParamsEquivalent(const FunctionType&, const FunctionType&) const;

  struct BuiltInFunctionInfo {
    vector<SType> argTypes;
    BuiltInFunction fun;
  };

  struct State : public owned_object<State> {
    map<string, SType> vars;
    vector<string> varsList;
    mutable set<string> movedVars;
    map<string, SType> types;
    map<FunctionId, vector<FunctionType>> functions;
    nullable<SType> returnType;
    map<string, shared_ptr<Concept>> concepts;
    map<string, BuiltInFunctionInfo> builtInFunctions;
    bool breakAllowed = false;
    bool isBuiltInModule = false;
    void merge(const State&);
    void print() const;
  };
  using MovedVarsSnapshot = map<shared_ptr<const State>, set<string>>;

  MovedVarsSnapshot getMovedVarsSnapshot() const;
  void setMovedVars(MovedVarsSnapshot);
  void mergeMovedVars(MovedVarsSnapshot);

  private:

  vector<shared_ptr<const State>> parentStates;
  shared_ptr<State> state;

  vector<SType> getTypeList(const vector<TemplateParameterInfo>&) const;
  vector<shared_ptr<const State>> getReversedStates() const;
  const State& getTopState() const;
  vector<FunctionType> getFunctions(FunctionId) const;
  nullable<SType> getVariable(const string&) const;
  bool isGeneralization(const FunctionType& general, const FunctionType& specific, vector<FunctionType> existing) const;
  vector<FunctionType> getAllFunctions() const;
};
