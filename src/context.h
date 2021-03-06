#pragma once
#include "stdafx.h"
#include "optional.h"
#include "variant.h"
#include "identifier.h"
#include "function_name.h"
#include "return_type_checker.h"
#include "lambda_capture_info.h"

struct IdentifierInfo;
struct Type;
struct FunctionSignature;
struct FunctionInfo;
struct Concept;
class Context;
using SContext = shared_ptr<Context>;
using SConcept = shared_ptr<Concept>;
using SConstContext = shared_ptr<const Context>;
class IdentifierType;
class TypeRegistry;
struct LambdaCaptureInfo;
struct LambdaCapture;
struct StateContainer;

class Context : public owned_object<Context> {
  public:
  Context getChild(bool isTopLevel = false) const;
  Context getTopLevel() const;
  void merge(const Context&);
  Context(TypeRegistry*, bool isTopLevel = false);
  Context(const Context&) = delete;
  Context(Context&&) = default;
  void operator = (const Context&) = delete;
  Context& operator = (Context&&) = default;
  void deepCopyFrom(const Context&);
  WithError<vector<SFunctionInfo>> getRequiredFunctions(const Concept&, vector<FunctionSignature> existing) const;
  nullable<SFunctionInfo> isGeneralization(const SFunctionInfo& general, const SFunctionInfo& specific,
      vector<FunctionSignature> existing = {}) const;
  nullable<SFunctionInfo> isGeneralizationWithoutReturnType(const SFunctionInfo& general, const SFunctionInfo& specific,
      vector<FunctionSignature> existing = {}) const;
  WithError<SType> getTypeOfVariable(const string&) const;
  bool isCapturedVariable(const string&) const;
  void addVariable(const string& ident, SType, CodeLoc, bool global = false);
  void setShadowId(const string& oldId, const string& newId);
  optional<string> getShadowId(const string& id) const;
  void setNonMovable(const string& variable);
  bool isNonMovable(const string& variable) const;
  void replace(SType from, SType to, ErrorBuffer&);
  void expand(SType, vector<SType> to, ErrorBuffer&);
  ReturnTypeChecker* getReturnTypeChecker() const;
  void addReturnTypeChecker(ReturnTypeChecker*);
  void addType(const string& name, SType);
  void addExpandedTypePack(const string& name, vector<SType>);
  void addUnexpandedTypePack(string, SType);
  void addUnexpandedVariablePack(string, SType);
  void addExpandedVariablePack(const string& name, vector<SType>);
  optional<pair<string, vector<SType>>> getExpandedTypePack() const;
  WithError<pair<string, vector<SType>>> getExpandedVariablePack() const;
  optional<pair<string, SType>> getUnexpandedTypePack() const;
  optional<pair<string, SType>> getUnexpandedVariablePack() const;
  struct SubstitutionInfo {
    string from;
    string to;
    bool constant;
  };
  void addSubstitution(SubstitutionInfo);
  vector<SubstitutionInfo> getSubstitutions() const;
  void setAttribute(SType, SType, vector<SType> templateParams);
  void setStructMembers(SType structType, vector<SType> members, vector<SType> templateParams);
  WithErrorLine<SType> getTypeFromString(IdentifierInfo, optional<bool> typePack = false) const;
  nullable<SType> getType(const string&) const;
  SType getSliceType(SType underlying) const;
  SType getMutableSliceType(SType underlying) const;
  bool isFullyDefined(const Type*) const;
  vector<SType> getAllTypes() const;
  NODISCARD JustError<string> addImplicitFunction(FunctionId, FunctionSignature);
  NODISCARD JustError<string> addFunction(SFunctionInfo);
  WithError<vector<SFunctionInfo>> getFunctionTemplate(IdentifierInfo) const;
  WithEvalError<SType> invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const;
  using BuiltInFunction = function<WithError<SType>(const Context&, vector<SType>)>;
  void addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction);
  vector<SFunctionInfo> getOperatorType(Operator) const;
  nullable<SFunctionInfo> getBuiltinOperator(Operator, vector<SType> argTypes) const;
  NODISCARD JustError<string> checkNameConflict(const string& name, const string& type) const;
  NODISCARD JustError<string> checkNameConflictExcludingFunctions(const string& name, const string& type) const;
  void addConcept(const string& name, SConcept);
  nullable<SConcept> getConcept(const string& name) const;
  void print() const;
  vector<SType> getConversions(SType, SType, bool withConcepts) const;
  JustError<string> canConvert(SType from, SType to, unique_ptr<Expression>&) const;
  JustError<string> canConvert(SType from, SType to) const;
  optional<int> getLoopId() const;
  int setIsInLoop();
  void setIsInBranch();
  bool areParamsEquivalent(FunctionSignature, FunctionSignature) const;
  optional<vector<SType>> getTemplateParams() const;
  void setTemplated(vector<SType>);
  void setTemplateInstance();
  bool isTemplateInstance() const;
  void setLambda(LambdaCaptureInfo*);

  struct BuiltInFunctionInfo {
    vector<SType> argTypes;
    SType returnType;
    BuiltInFunction fun;
    WithEvalError<SType> invokeFunction(const Context&, const string& id, CodeLoc loc, vector<SType> args,
        vector<CodeLoc> argLoc) const;
  };

  struct VariableInfo {
    SType type;
    CodeLoc declarationLoc;
    bool global;
  };

  struct State : public owned_object<State> {
    map<string, VariableInfo> vars;
    map<string, string> shadowIds;
    vector<string> varsList;
    map<string, SType> types;
    set<const Type*> typesSet;
    optional<pair<string, vector<SType>>> expandedTypePack;
    optional<pair<string, vector<SType>>> expandedVariablePack;
    optional<pair<string, SType>> unexpandedVariablePack;
    optional<pair<string, SType>> unexpandedTypePack;
    map<FunctionId, vector<SFunctionInfo>> functions;
    ReturnTypeChecker* returnTypeChecker = nullptr;
    map<string, shared_ptr<Concept>> concepts;
    map<string, BuiltInFunctionInfo> builtInFunctions;
    optional<int> loopId;
    bool isBranch = false;
    bool isTemplateInstance = false;
    vector<SType> templateParams;
    LambdaCaptureInfo* lambdaInfo;
    vector<SubstitutionInfo> substitutions;
    unordered_set<string> nonMovableVars;
    bool isTopLevel = false;
    void merge(const State&);
    void print() const;
  };

  using ConstStates = vector<shared_ptr<const State>>;

  WithErrorLine<vector<SType>> getTypeList(const vector<TemplateParameterInfo>&, bool variadic) const;
  vector<SFunctionInfo> getFunctions(FunctionId) const;
  vector<SFunctionInfo> getAllFunctions() const;

  TypeRegistry* typeRegistry;

  private:

  friend struct StateIterator;
  friend struct StateContainer;

  ConstStates parentStates;
  shared_ptr<State> state;

  StateContainer getReversedStates() const;
  nullable<SType> getVariable(const string&) const;
  vector<SFunctionInfo> getConstructorsFor(const SType&) const;
};
