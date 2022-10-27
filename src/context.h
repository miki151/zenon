#pragma once
#include "stdafx.h"
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
class IdentifierType;
class TypeRegistry;
struct LambdaCaptureInfo;
struct LambdaCapture;
struct StateContainer;
struct Node;
struct LanguageIndex;

class Context : public owned_object<Context> {
  public:
  Context getChild(bool isTopLevel = false) const;
  Context getTopLevel() const;
  void merge(const Context&);
  Context(TypeRegistry*, LanguageIndex*, bool isTopLevel = false);
  Context(const Context&) = delete;
  Context(Context&&) = default;
  void operator = (const Context&) = delete;
  Context& operator = (Context&&) = default;
  void deepCopyFrom(const Context&);
  WithError<vector<FunctionInfo*>> getRequiredFunctions(const Concept&, vector<FunctionSignature> existing) const;
  FunctionInfo* isGeneralization(FunctionInfo* general, FunctionInfo* specific,
      vector<FunctionSignature> existing = {}) const;
  FunctionInfo* isGeneralizationWithoutReturnType(FunctionInfo* general, FunctionInfo* specific,
      vector<FunctionSignature> existing = {}) const;
  WithError<Type*> getTypeOfVariable(const string&, CodeLoc) const;
  bool isCapturedVariable(const string&) const;
  void addVariable(const string& ident, Type*, CodeLoc, bool global = false);
  void setShadowId(const string& oldId, const string& newId);
  optional<string> getShadowId(const string& id) const;
  void setNonMovable(const string& variable);
  bool isNonMovable(const string& variable) const;
  void replace(const Context&, Type* from, Type* to, ErrorBuffer&);
  void expand(Type*, vector<Type*> to, ErrorBuffer&);
  ReturnTypeChecker* getReturnTypeChecker() const;
  void addReturnTypeChecker(ReturnTypeChecker*);
  void addType(const string& name, Type*);
  void setTypeFullyDefined(Type*);
  void addExpandedTypePack(const string& name, vector<Type*>);
  void addUnexpandedTypePack(string, Type*);
  void addUnexpandedVariablePack(string, Type*);
  void addExpandedVariablePack(const string& name, vector<Type*>);
  optional<pair<string, vector<Type*>>> getExpandedTypePack() const;
  WithError<pair<string, vector<Type*>>> getExpandedVariablePack() const;
  optional<pair<string, Type*>> getUnexpandedTypePack() const;
  optional<pair<string, Type*>> getUnexpandedVariablePack() const;
  struct SubstitutionInfo {
    string from;
    string to;
    bool constant;
  };
  void addSubstitution(SubstitutionInfo);
  vector<SubstitutionInfo> getSubstitutions() const;
  void setAttribute(Type*, Type*, vector<Type*> templateParams);
  void setStructMembers(Type* structType, vector<Type*> members, vector<Type*> templateParams);
  void setUnionAlternatives(Type* unionType, vector<Type*> alternatives, vector<Type*> templateParams);
  WithErrorLine<Type*> getTypeFromString(IdentifierInfo, optional<bool> typePack = false) const;
  Type* getType(const string&) const;
  Type* getSliceType(Type* underlying) const;
  Type* getMutableSliceType(Type* underlying) const;
  bool isFullyDefined(const Type*) const;
  vector<Type*> getAllTypes() const;
  NODISCARD JustError<string> addImplicitFunction(FunctionId, FunctionSignature);
  NODISCARD JustError<string> addFunction(FunctionInfo*);
  WithError<vector<FunctionInfo*>> getFunctionTemplate(IdentifierInfo, bool compileTimeArgs) const;
  WithEvalError<Type*> invokeFunction(const string& id, CodeLoc loc, vector<Type*> args, vector<CodeLoc> argLoc) const;
  using BuiltInFunction = function<WithError<Type*>(const Context&, vector<Type*>)>;
  void addBuiltInFunction(const string& id, Type* returnType, vector<Type*> argTypes, BuiltInFunction);
  vector<FunctionInfo*> getOperatorType(Operator) const;
  FunctionInfo* getBuiltinOperator(Operator, vector<Type*> argTypes) const;
  NODISCARD JustError<ErrorLoc> checkNameConflictIncludingConcepts(const string& name, const CodeLoc&,
      const string& type) const;
  NODISCARD JustError<ErrorLoc> checkNameConflict(const string& name, const CodeLoc&, const string& type) const;
  NODISCARD JustError<ErrorLoc> checkNameConflictExcludingFunctions(const string& name, const CodeLoc& codeLoc,
      const string& type) const;
  Concept* getConcept(const string& name, CodeLoc) const;
  void addConcept(const string& name, Concept*);
  void print() const;
  vector<Type*> getConversions(Type*, Type*, bool withConcepts) const;
  JustError<string> canConvert(Type* from, Type* to, unique_ptr<Expression>&) const;
  JustError<string> canConvert(Type* from, Type* to) const;
  optional<int> getLoopId() const;
  int setIsInLoop();
  void setIsInBranch();
  bool areParamsEquivalent(FunctionSignature, FunctionSignature) const;
  optional<vector<Type*>> getTemplateParams() const;
  void setTemplated(vector<Type*>);
  void setTemplateInstance();
  bool isTemplateInstance() const;
  void setLambda(LambdaCaptureInfo*);

  struct BuiltInFunctionInfo {
    FunctionInfo* functionInfo;
    BuiltInFunction fun;
    WithEvalError<Type*> invokeFunction(const Context&, const string& id, CodeLoc loc, vector<Type*> args,
        vector<CodeLoc> argLoc) const;
  };

  struct VariableInfo {
    Type* type;
    CodeLoc declarationLoc;
    bool global;
  };

  struct State : public owned_object<State> {
    unordered_map<string, VariableInfo> vars;
    unordered_map<string, string> shadowIds;
    vector<string> varsList;
    unordered_map<string, Type*> types;
    unordered_set<const Type*> typesSet;
    optional<pair<string, vector<Type*>>> expandedTypePack;
    optional<pair<string, vector<Type*>>> expandedVariablePack;
    optional<pair<string, Type*>> unexpandedVariablePack;
    optional<pair<string, Type*>> unexpandedTypePack;
    unordered_map<FunctionId, vector<FunctionInfo*>> functions;
    ReturnTypeChecker* returnTypeChecker = nullptr;
    unordered_set<Concept*> concepts;
    unordered_map<string, BuiltInFunctionInfo> builtInFunctions;
    optional<int> loopId;
    bool isBranch = false;
    bool isTemplateInstance = false;
    vector<Type*> templateParams;
    LambdaCaptureInfo* lambdaInfo;
    vector<SubstitutionInfo> substitutions;
    unordered_set<string> nonMovableVars;
    bool isTopLevel = false;
    void merge(const State&);
    void print() const;
  };

  using ConstStates = vector<shared_ptr<const State>>;

  WithErrorLine<vector<Type*>> getTypeList(const vector<TemplateParameterInfo>&, bool variadic) const;
  vector<FunctionInfo*> getFunctions(FunctionId, bool compileTime) const;
  vector<FunctionInfo*> getAllFunctions() const;

  TypeRegistry* typeRegistry;
  LanguageIndex* languageIndex;
  private:

  friend struct StateIterator;
  friend struct StateContainer;

  ConstStates parentStates;
  shared_ptr<State> state;

  StateContainer getReversedStates() const;
  Type* getVariable(const string&) const;
  vector<FunctionInfo*> getConstructorsFor(Type*) const;
  Type* getIndexHelperType(int index) const;
};
