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
struct FunctionType;
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

class Context : public owned_object<Context> {
  public:
  static Context withParent(const Context&);
  static Context withParent(vector<const Context*>);
  void merge(const Context&);
  Context(TypeRegistry*);
  Context(const Context&) = delete;
  Context(Context&&) = default;
  void operator = (const Context&) = delete;
  Context& operator = (Context&&) = default;
  void deepCopyFrom(const Context&);
  WithError<vector<SFunctionInfo>> getRequiredFunctions(const Concept&, vector<FunctionType> existing) const;
  bool isGeneralization(const SFunctionInfo& general, const SFunctionInfo& specific, vector<FunctionType> existing = {}) const;
  WithError<SType> getTypeOfVariable(const string&) const;
  bool isCapturedVariable(const string&) const;
  void addVariable(const string& ident, SType, CodeLoc);
  void addVariablePack(const string& ident, SType, CodeLoc);
  struct VariablePackInfo {
    string name;
    SType type;
    CodeLoc codeLoc;
    bool wasExpanded = false;
  };
  const optional<VariablePackInfo>& getVariablePack() const;
  JustError<string> expandVariablePack();
  void replace(SType from, SType to, ErrorBuffer&);
  void expand(SType, vector<SType> to, ErrorBuffer&);
  ReturnTypeChecker* getReturnTypeChecker() const;
  void addReturnTypeChecker(ReturnTypeChecker*);
  void addType(const string& name, SType, bool fullyDefined = true, bool typePack = false);
  WithErrorLine<SType> getTypeFromString(IdentifierInfo, optional<bool> typePack = false) const;
  nullable<SType> getType(const string&) const;
  nullable<SType> getTypePack() const;
  bool isFullyDefined(const Type*) const;
  void setFullyDefined(const Type*, bool);
  vector<SType> getAllTypes() const;
  NODISCARD JustError<string> addImplicitFunction(FunctionId, FunctionType);
  NODISCARD JustError<string> addFunction(SFunctionInfo);
  WithError<IdentifierType> getIdentifierType(const IdentifierInfo&) const;
  WithError<vector<SFunctionInfo>> getFunctionTemplate(IdentifierType) const;
  WithEvalError<SType> invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const;
  using BuiltInFunction = function<WithError<SType>(vector<SType>)>;
  void addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction);
  vector<SFunctionInfo> getOperatorType(Operator) const;
  nullable<SFunctionInfo> getBuiltinOperator(Operator, vector<SType> argTypes) const;
  NODISCARD JustError<string> checkNameConflict(const string& name, const string& type) const;
  NODISCARD JustError<string> checkNameConflictExcludingFunctions(const string& name, const string& type) const;
  void addConcept(const string& name, SConcept);
  nullable<SConcept> getConcept(const string& name) const;
  void print() const;
  vector<SType> getConversions(SType) const;
  JustError<string> canConvert(SType from, SType to, unique_ptr<Expression>&) const;
  JustError<string> canConvert(SType from, SType to) const;
  optional<int> getLoopId() const;
  int setIsInLoop();
  bool areParamsEquivalent(FunctionType, FunctionType) const;
  optional<vector<SType>> getTemplateParams() const;
  void setTemplated(vector<SType>);
  NODISCARD WithErrorLine<vector<LambdaCapture>> setLambda(LambdaCaptureInfo*);

  struct BuiltInFunctionInfo {
    vector<SType> argTypes;
    SType returnType;
    BuiltInFunction fun;
    WithEvalError<SType> invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const;
  };

  struct VariableInfo {
    SType type;
    CodeLoc declarationLoc;
  };

  struct State : public owned_object<State> {
    map<string, VariableInfo> vars;
    vector<string> varsList;
    optional<VariablePackInfo> variablePack;
    map<string, SType> types;
    nullable<SType> typePack;
    map<const Type*, bool> fullyDefinedTypes;
    map<FunctionId, vector<SFunctionInfo>> functions;
    ReturnTypeChecker* returnTypeChecker = nullptr;
    map<string, shared_ptr<Concept>> concepts;
    map<string, BuiltInFunctionInfo> builtInFunctions;
    optional<int> loopId;
    bool isBuiltInModule = false;
    vector<SType> templateParams;
    LambdaCaptureInfo* lambdaInfo;
    void merge(const State&);
    void print() const;
  };

  using ConstStates = vector<shared_ptr<const State>>;
  static Context withStates(TypeRegistry*, ConstStates);

  WithErrorLine<vector<SType> > getTypeList(const vector<TemplateParameterInfo>&, bool variadic) const;
  vector<SFunctionInfo> getFunctions(FunctionId) const;
  vector<SFunctionInfo> getAllFunctions() const;

  TypeRegistry* const typeRegistry;

  private:

  ConstStates parentStates;
  shared_ptr<State> state;

  vector<shared_ptr<const State>> getReversedStates() const;
  nullable<SType> getVariable(const string&) const;
  vector<SFunctionInfo> getConstructorsFor(const SType&) const;
};
