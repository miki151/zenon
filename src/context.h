#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "identifier.h"
#include "function_name.h"

struct IdentifierInfo;
struct Type;
struct FunctionType;
struct FunctionInfo;
struct Concept;
class Context;
using SContext = shared_ptr<Context>;
using SConcept = shared_ptr<Concept>;
using SConstContext = shared_ptr<const Context>;
class IdentifierType;

class Context : public owned_object<Context> {
  public:
  static Context withParent(const Context&);
  static Context withParent(vector<const Context*>);
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
  void replace(SType from, SType to, ErrorBuffer&);
  nullable<SType> getReturnType() const;
  void setReturnType(SType);
  void addType(const string& name, SType);
  WithErrorLine<SType> getTypeFromString(IdentifierInfo) const;
  nullable<SType> getType(const string&) const;
  vector<SType> getAllTypes() const;
  [[nodiscard]] optional<string> addImplicitFunction(FunctionId, FunctionType);
  [[nodiscard]] optional<string> addFunction(SFunctionInfo);
  WithError<IdentifierType> getIdentifierType(const IdentifierInfo&) const;
  WithError<vector<SFunctionInfo>> getFunctionTemplate(IdentifierType) const;
  WithErrorLine<SFunctionInfo> instantiateFunctionTemplate(CodeLoc, SFunctionInfo, vector<SType>,
      vector<SType> argTypes, vector<CodeLoc> argLoc) const;
  nullable<SType> invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const;
  using BuiltInFunction = function<WithError<SType>(vector<SType>)>;
  void addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction);
  vector<SFunctionInfo> getOperatorType(Operator) const;
  nullable<SFunctionInfo> getBuiltinOperator(Operator, vector<SType> argTypes) const;
  bool canDefaultInitialize(SType) const;
  NODISCARD optional<string> checkNameConflict(const string& name, const string& type) const;
  NODISCARD optional<string> checkNameConflictExcludingFunctions(const string& name, const string& type) const;
  void addConcept(const string& name, SConcept);
  nullable<SConcept> getConcept(const string& name) const;
  void print() const;
  vector<SType> getConversions(SType) const;
  bool canConvert(SType from, SType to) const;
  bool breakAllowed() const;
  void setBreakAllowed();
  bool areParamsEquivalent(const SFunctionInfo&, const SFunctionInfo&) const;
  bool isTemplated() const;
  void setTemplated();

  struct BuiltInFunctionInfo {
    vector<SType> argTypes;
    SType returnType;
    BuiltInFunction fun;
    nullable<SType> invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const;
  };

  struct State : public owned_object<State> {
    map<string, SType> vars;
    vector<string> varsList;
    mutable set<string> movedVars;
    map<string, SType> types;
    map<FunctionId, vector<SFunctionInfo>> functions;
    nullable<SType> returnType;
    map<string, shared_ptr<Concept>> concepts;
    map<string, BuiltInFunctionInfo> builtInFunctions;
    bool breakAllowed = false;
    bool isBuiltInModule = false;
    bool templated = false;
    void merge(const State&);
    void print() const;
  };
  using MovedVarsSnapshot = map<shared_ptr<const State>, set<string>>;

  MovedVarsSnapshot getMovedVarsSnapshot() const;
  void setMovedVars(MovedVarsSnapshot);
  void mergeMovedVars(MovedVarsSnapshot);

  using ConstStates = vector<shared_ptr<const State>>;
  ConstStates getAllStates() const;
  ConstStates getTopLevelStates() const;
  void setAsTopLevel();
  static Context withStates(ConstStates);

  WithErrorLine<vector<SType> > getTypeList(const vector<TemplateParameterInfo>&) const;

  private:

  ConstStates parentStates;
  ConstStates topLevelStates;
  shared_ptr<State> state;


  vector<shared_ptr<const State>> getReversedStates() const;
  const State& getTopState() const;
  vector<SFunctionInfo> getFunctions(FunctionId) const;
  nullable<SType> getVariable(const string&) const;
  bool isGeneralization(const SFunctionInfo& general, const SFunctionInfo& specific, vector<FunctionType> existing) const;
  vector<SFunctionInfo> getAllFunctions() const;
  vector<SFunctionInfo> getConstructorsFor(const SType&) const;
};
