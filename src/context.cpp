#include "context.h"
#include "ast.h"
#include "type.h"


Context Context::withParent(const Context& c) {
  Context ret;
  ret.parentStates = c.parentStates;
  ret.parentStates.push_back(c.state);
  return ret;
}

Context Context::withParent(vector<Context*> parents) {
  Context ret;
  for (auto context : parents) {
    append(ret.parentStates, context->parentStates);
    ret.parentStates.push_back(context->state);
  }
  return ret;
}

void Context::merge(const Context& context) {
  for (auto& s : context.parentStates)
    if (!contains(parentStates, s) && state != s)
      parentStates.push_back(s);
  if (!contains(parentStates, context.state) && state != context.state)
    parentStates.push_back(context.state);
}

Context::Context() : state(shared<State>()) {
}

void Context::State::merge(const Context::State& o) {
  for (auto& f : o.functions) {
    CHECK(!functions.count(f.first));
    functions.insert(f);
  }
}

void Context::deepCopyFrom(const Context& c) {
  CHECK(parentStates.empty());
  *state = *c.state;
  for (auto s : c.parentStates)
    state->merge(*s);
}

static string getFunctionIdName(const FunctionId& id) {
  return id.visit(
      [&](const string& s) { return s; },
      [&](Operator op) { return "operator "s + getString(op); },
      [&](ConstructorTag) { return "constructor"; }
  );
}

static string getFunctionNameForError(const FunctionId& name, const FunctionType& function) {
  string typePrefix;
  if (function.parentType)
    typePrefix = function.parentType->getName() + "::";
  return function.retVal->getName() + " " + typePrefix + getFunctionIdName(name) + "(" +
      combine(transform(function.params, [](const auto& t) { return t.type->getName(); }), ", ") + ")";
}

bool Context::areParamsEquivalent(const FunctionType& f1, const FunctionType& f2) const {
  auto checkFun = [this](const FunctionType& f1, const FunctionType& f2) {
    return !!instantiateFunction(*this, f1, CodeLoc(), {}, transform(f2.params, [](const auto& param) { return param.type; }),
        vector<CodeLoc>(f2.params.size(), CodeLoc()), {});
  };
  return checkFun(f1, f2) && checkFun(f2, f1);
}

bool Context::isGeneralization(const FunctionType& general, const FunctionType& specific,
    vector<FunctionType> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = instantiateFunction(*this, general, CodeLoc(), {}, transform(specific.params, [](const auto& param) { return param.type; }),
      vector<CodeLoc>(specific.params.size(), CodeLoc()), existing)) {
    return specific.retVal == inst->retVal;
  }
  else
    return false;
}

static bool isBuiltinCopyConstructor(const Context& context, const FunctionId& name, const FunctionType& f) {
  if (auto type = name.contains<ConstructorTag>())
    return f.params.size() == 1 && f.params[0].type == PointerType::get(f.retVal)
        && f.retVal->isBuiltinCopyable(context);
  return false;
}

optional<string> Context::getMissingFunctions(const Concept& required, vector<FunctionType> existing) const {
  //cout << "Looking for missing functions in:\n";
  //required.print();
  vector<FunctionType> ret;
  for (auto otherState : required.getContext().getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        // what is this line for? no test breaks when it is removed...
        if (isBuiltinCopyConstructor(*this, overloads.first, function))
          continue;
    //    cout << "Looking for function: " << getFunctionNameForError(function) << "\n";
      //  print();
        bool found = false;
        for (auto& myFun : getFunctions(overloads.first))
          if (isGeneralization(myFun, function, existing)) {
            found = true;
            break;
          }
        if (!found)
          return "Required function not implemented: " + FunctionInfo{overloads.first, function}.prettyString() +
              ", required by concept: " + quote(required.getName());
      }
  }
  return none;
}

Context::MovedVarsSnapshot Context::getMovedVarsSnapshot() const {
  MovedVarsSnapshot ret;
  for (auto& state : getReversedStates())
    ret[state] = state->movedVars;
  return ret;
}

void Context::setMovedVars(Context::MovedVarsSnapshot snapshot) {
  for (auto& state : getReversedStates())
    state->movedVars = getValueMaybe(snapshot, state).value_or(set<string>());
}

void Context::mergeMovedVars(Context::MovedVarsSnapshot snapshot) {
  for (auto& state : getReversedStates())
    for (auto& var : getValueMaybe(snapshot, state).value_or(set<string>()))
      state->movedVars.insert(var);
}

WithError<SType> Context::getTypeOfVariable(const string& id) const {
  for (auto& state : getReversedStates()) {
    if (state->movedVars.count(id))
      return "Variable has been moved: " + id;
    if (state->vars.count(id))
      return state->vars.at(id);
  }
  return "Variable not found: " + id;
}

optional<string> Context::setVariableAsMoved(const string& id) {
  for (auto& state : getReversedStates())
    if (state->vars.count(id)) {
      state->movedVars.insert(id);
      return none;
    }
  return "Variable not found: " + id;
}

void Context::addVariable(const string& ident, SType t) {
  state->vars.insert({ident, t});
  state->varsList.push_back(ident);
}

void Context::State::print() const {
  for (auto& varName : varsList) {
    auto& var = vars.at(varName);
    cout << "Variable " << quote(varName) << " of type " << var->getName() << "\n";
  }
  for (auto& function : functions) {
    cout << "Function " << quote(getFunctionIdName(function.first)) << " overloads: \n";
    for (auto& overload : function.second)
      cout << getFunctionNameForError(function.first, overload) << "\n";
  }
  for (auto& type : types)
    cout << "Type: " << type.second->getName() << "\n";
}

void Context::print() const {
  for (auto& state : getReversedStates())
    state->print();
}

vector<SType> Context::getConversions(SType type) const {
  vector<SType> ret = {type};
  if (type->getUnderlying()->isBuiltinCopyable(*this) && type != type->getUnderlying())
    ret.push_back(type->getUnderlying());
  if (type->getUnderlying() == ArithmeticType::INT)
    ret.push_back(ArithmeticType::DOUBLE);
  if (auto ptr = type->getUnderlying().dynamicCast<MutablePointerType>())
    ret.push_back(PointerType::get(ptr->underlying));
  return ret;
}

bool Context::canConvert(SType from, SType to) const {
  return contains(getConversions(from), to);
}

bool Context::breakAllowed() const {
  for (auto state : getReversedStates())
    if (state->breakAllowed)
      return true;
  return false;
}

void Context::setBreakAllowed() {
  state->breakAllowed = true;
}

void Context::replace(SType from, SType to) {
  for (auto& varName : state->varsList) {
    auto& var = state->vars.at(varName);
    var = var->replace(from, to);
  }
  for (auto& function : state->functions) {
    for (auto& overload : function.second)
      replaceInFunction(overload, from, to);
  }
  for (auto& type : state->types)
    type.second = type.second->replace(from, to);
}

const Context::State& Context::getTopState() const {
  if (parentStates.empty())
    return *state;
  else
    return *parentStates.front();
}

vector<shared_ptr<const Context::State>> Context::getReversedStates() const {
  vector<shared_ptr<const Context::State>> ret { state };
  for (auto& state : reverse(parentStates))
    ret.push_back(state);
  return ret;
}

nullable<SType> Context::getReturnType() const {
  for (auto& state : getReversedStates())
    if (state->returnType)
      return state->returnType;
  return nullptr;
}

void Context::setReturnType(SType t) {
  CHECK(!getReturnType()) << "Attempted to overwrite return type";
  state->returnType = t;
}

void Context::addType(const string& name, SType t) {
  CHECK(!getType(name));
  state->types.insert({name, t});
}

vector<SType> Context::getTypeList(const vector<TemplateParameterInfo>& ids) const {
  vector<SType> params;
  for (auto& id : ids)
    id.visit(
        [&](const IdentifierInfo& id) { params.push_back(getTypeFromString(id).get()); },
        [&](const shared_ptr<Expression>& expr) { params.push_back(expr->eval(*this).get()); }
    );
  return params;
}

void Context::addConcept(const string& name, shared_ptr<Concept> i) {
  state->concepts.insert({name, i});
}

nullable<SConcept> Context::getConcept(const string& name) const {
  for (auto& state : getReversedStates())
    if (state->concepts.count(name))
      return state->concepts.at(name);
  return nullptr;

}

nullable<SType> Context::getType(const string& s) const {
  for (auto& state : getReversedStates())
    if (state->types.count(s))
      return state->types.at(s);
  return nullptr;
}

vector<FunctionType> Context::getFunctions(FunctionId name) const {
  vector<FunctionType> ret;
  for (auto& state : getReversedStates())
    if (state->functions.count(name))
      append(ret, state->functions.at(name));
  return ret;
}

vector<FunctionInfo> Context::getAllFunctions() const {
  vector<FunctionInfo> ret;
  for (auto& state : getReversedStates())
    for (auto& overloadSet : state->functions)
      for (auto& fun : overloadSet.second)
        ret.push_back({overloadSet.first, fun});
  return ret;
}

nullable<SType> Context::getVariable(const string& name) const {
  for (auto& state : getReversedStates())
    if (state->vars.count(name))
      return state->vars.at(name);
  return nullptr;
}

WithErrorLine<SType> Context::getTypeFromString(IdentifierInfo id) const {
  if (id.parts.size() != 1)
    return id.codeLoc.getError("Bad type identifier: " + id.toString());
  auto name = id.parts.at(0).name;
  auto topType = getType(name);
  if (!topType)
    return id.codeLoc.getError("Type not found: " + quote(name));
  WithErrorLine<SType> ret = topType->instantiate(*this, getTypeList(id.parts.at(0).templateArguments))
      .addCodeLoc(id.codeLoc);
  if (ret)
    for (auto& elem : id.typeOperator) {
      elem.visit(
          [&](IdentifierInfo::PointerType type) {
            switch (type) {
              case IdentifierInfo::CONST:
                *ret = PointerType::get(*ret);
                break;
              case IdentifierInfo::MUTABLE:
                *ret = MutablePointerType::get(*ret);
                break;
            }
          },
          [&](const IdentifierInfo::ArraySize& size) {
            if (auto type = size.expr->eval(*this)) {
              if (auto value = type.get().dynamicCast<CompileTimeValue>())
                if (value.get()->getType() == ArithmeticType::INT) {
                  *ret = ArrayType::get(*ret, value);
                  return;
                }
              ret = size.expr->codeLoc.getError("Inappropriate type of array size: " + quote(type.get()->getName()));
            } else
              ret = size.expr->codeLoc.getError("Can't evaluate array size expression at compile-time"s);
          },
          [&](IdentifierInfo::Slice) {
            *ret = SliceType::get(*ret);
          }
      );
      if (!ret)
        return ret;
    }
  return ret;
}

void Context::checkNameConflict(CodeLoc loc, const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  loc.check(!getType(name), desc + " conflicts with an existing type");
  loc.check(!getVariable(name), desc + " conflicts with an existing variable or function");
  loc.check(getFunctions(name).empty(), desc + " conflicts with existing function");
}

void Context::checkNameConflictExcludingFunctions(CodeLoc loc, const string& name, const string& type) const {
  auto desc = type + " " + quote(name);
  loc.check(!getType(name), desc + " conflicts with an existing type");
  loc.check(!getVariable(name), desc + " conflicts with an existing variable or function");
}

optional<string> Context::addFunction(FunctionId id, FunctionType f) {
  auto& overloads = state->functions[id];
  for (auto& fun : overloads)
    if (areParamsEquivalent(fun, f) && (!id.contains<ConstructorTag>() || fun.retVal == f.retVal))
      return "Can't overload " + quote(getFunctionNameForError(id, f)) + " with the same argument types."s;
  overloads.push_back(f);
  return none;
}

optional<string> Context::addFunction(FunctionInfo info) {
  return addFunction(std::move(info.id), std::move(info.type));
}

WithError<vector<FunctionInfo>> Context::getFunctionTemplate(IdentifierInfo id) const {
  vector<FunctionInfo> ret;
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts.at(0), id.codeLoc)))
      return (*type)->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
    else
      return "Type not found: " + id.toString();
  } else {
    string funName = id.parts.at(0).name;
    for (auto& fun : getFunctions(funName))
      ret.push_back(FunctionInfo{FunctionId{funName}, fun});
    if (auto type = getType(funName))
      for (auto& fun : getFunctions(ConstructorTag{}))
        if (fun.retVal == type.get())
          ret.push_back(FunctionInfo{ConstructorTag{}, fun});
  }
  return ret;
}

WithErrorLine<FunctionType> Context::instantiateFunctionTemplate(CodeLoc codeLoc, FunctionType templateType,
    vector<TemplateParameterInfo> templateParams, vector<SType> argTypes, vector<CodeLoc> argLoc) const {
  return instantiateFunction(*this, templateType, codeLoc, getTypeList(templateParams), argTypes, argLoc, {});
}

nullable<SType> Context::invokeFunction(const string& id, CodeLoc loc, vector<SType> args, vector<CodeLoc> argLoc) const {
  for (auto& state : getReversedStates())
    if (auto f = getReferenceMaybe(state->builtInFunctions, id)) {
      if (f->argTypes.size() != args.size())
        loc.error("Expected " + to_string(f->argTypes.size()) + " arguments to built-in function " + quote(id));
      for (int i = 0; i < args.size(); ++i)
        if (f->argTypes[i] != args[i]->getType())
          argLoc[i].error("Expected argument of type " + quote(f->argTypes[i]->getName()) + ", got "
              + quote(args[i]->getName()) + " of type " + quote(args[i]->getType()->getName()));
      return f->fun(*this, args).addCodeLoc(loc).get();
    }
  return nullptr;
}

void Context::addBuiltInFunction(const string& id, SType returnType, vector<SType> argTypes, BuiltInFunction fun) {
  CHECK(!state->builtInFunctions.count(id));
  CHECK(!addFunction(id, FunctionType(returnType,
      transform(argTypes, [](const auto& p){ return FunctionType::Param(p); }), {})));
  state->builtInFunctions.insert(make_pair(id, BuiltInFunctionInfo{std::move(argTypes), std::move(fun)}));
}

optional<FunctionType> Context::getBuiltinOperator(Operator op, vector<SType> argTypes) const {
  switch (op) {
    case Operator::SUBSCRIPT:
      if (argTypes.size() == 2)
        if (auto arrayType = argTypes[0]->getUnderlying().dynamicCast<ArrayType>())
          if (argTypes[1]->getUnderlying() == ArithmeticType::INT) {
            auto ret = [&] {
              if (argTypes[0].dynamicCast<ReferenceType>())
                return FunctionType(
                    ReferenceType::get(arrayType->underlying), {{argTypes[0]}, {ArithmeticType::INT}}, {});
              if (argTypes[0].dynamicCast<MutableReferenceType>())
                return FunctionType(
                    MutableReferenceType::get(arrayType->underlying), {{argTypes[0]}, {ArithmeticType::INT}}, {});
              return FunctionType(arrayType->underlying, {{argTypes[0]}, {ArithmeticType::INT}}, {});
            }();
            ret.subscriptOpWorkaround = false;
            return ret;
          }
      break;
    case Operator::GET_ADDRESS:
      if (argTypes.size() == 1) {
        if (auto referenceType = argTypes[0].dynamicCast<ReferenceType>())
          return FunctionType(PointerType::get(referenceType->underlying), {argTypes[0]}, {});
        else if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
          return FunctionType(MutablePointerType::get(referenceType->underlying), {argTypes[0]}, {});
      }
      break;
    case Operator::POINTER_DEREFERENCE:
      if (argTypes.size() == 1) {
        if (auto pointerType = argTypes[0]->getUnderlying().dynamicCast<PointerType>())
          return FunctionType(ReferenceType::get(pointerType->underlying), {argTypes[0]}, {});
        else if (auto pointerType = argTypes[0]->getUnderlying().dynamicCast<MutablePointerType>())
          return FunctionType(MutableReferenceType::get(pointerType->underlying), {argTypes[0]}, {});
      }
      break;
    case Operator::ASSIGNMENT:
      if (argTypes.size() == 2 && canConvert(argTypes[1], argTypes[0]->getUnderlying()))
        if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
          return FunctionType(ArithmeticType::VOID, {argTypes[0], referenceType->underlying}, {});
      break;
    default:
      break;
  }
  return none;
}

vector<FunctionType> Context::getOperatorType(Operator op) const {
  return getFunctions(op);
}

bool Context::canConstructWith(SType type, vector<SType> argsRef) const {
//  cout << "Trying to construct " << type->getName() << " with " << combine(transform(argsRef, [](const auto& t) { return t->getName(); }), ", ") << "\n";
//  print();
  auto args = transform(argsRef, [](const auto& arg) { return arg->getUnderlying();});
  if (args.size() == 1 && args[0] == type)
    return true;
  vector<SType> templateParams;
  if (auto s = type.dynamicCast<StructType>()) {
    type = s->parent.get();
    templateParams = s->templateParams;
  }
  for (auto& f : getFunctions(ConstructorTag{}))
    if (f.retVal == type &&
       instantiateFunction(*this, f, CodeLoc(), templateParams, args, vector<CodeLoc>(args.size())))
      return true;
  return false;
}
