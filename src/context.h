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
  vector<FunctionType> getMissingFunctions(const Context&, vector<FunctionType> existing) const;
  WithError<SType> getTypeOfVariable(const string&) const;
  optional<string> setVariableAsMoved(const string&);
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
  void pushImport(const string& name, size_t contentHash);
  void popImport();
  bool isCurrentlyImported(size_t contentHash);
  const vector<string>& getCurrentImports() const;
  bool wasEverImported(size_t contentHash);
  const vector<string>& getAllImports() const;
  void checkNameConflict(CodeLoc loc, const string& name, const string& type) const;
  void checkNameConflictExcludingFunctions(CodeLoc loc, const string& name, const string& type) const;
  vector<SType> getTypeList(const vector<IdentifierInfo>&) const;
  void addConcept(const string& name, SConcept);
  nullable<SConcept> getConcept(const string& name) const;
  void print() const;
  vector<SType> getConversions(SType) const;
  bool canConvert(SType from, SType to) const;
  bool breakAllowed() const;
  void setBreakAllowed();

  struct State : public owned_object<State> {
    map<string, SType> vars;
    vector<string> varsList;
    mutable set<string> movedVars;
    map<string, SType> types;
    map<FunctionId, vector<FunctionType>> functions;
    nullable<SType> returnType;
    vector<string> imports;
    vector<size_t> importHashes;
    vector<string> allImports;
    unordered_set<size_t> allImportHashes;
    map<string, shared_ptr<Concept>> concepts;
    bool breakAllowed = false;
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
  vector<shared_ptr<const State>> getReversedStates() const;
  const State& getTopState() const;
  nullable<SType> getType(const string&) const;
  vector<FunctionType> getFunctions(FunctionId) const;
  nullable<SType> getVariable(const string&) const;
  bool areParamsEquivalent(const FunctionType&, const FunctionType&) const;
  bool isGeneralization(const FunctionType& general, const FunctionType& specific, vector<FunctionType> existing) const;
  vector<FunctionType> getAllFunctions() const;
};
