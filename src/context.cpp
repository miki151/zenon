#include "context.h"
#include "ast.h"
#include "type.h"
#include "identifier_type.h"

Context Context::withParent(const Context& c) {
  Context ret;
  ret.parentStates = c.parentStates;
  ret.parentStates.push_back(c.state);
  ret.topLevelStates = c.topLevelStates;
  return ret;
}

Context Context::withParent(vector<const Context*> parents) {
  Context ret;
  for (auto context : parents) {
    append(ret.parentStates, context->parentStates);
    append(ret.topLevelStates, context->topLevelStates);
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

bool Context::areParamsEquivalent(const SFunctionInfo& f1, const SFunctionInfo& f2) const {
  auto checkFun = [this](const SFunctionInfo& f1, const SFunctionInfo& f2) {
    return !!instantiateFunction(*this, f1, CodeLoc(), {}, transform(f2->type.params,
            [](const auto& param) { return param.type; }),
        vector<CodeLoc>(f2->type.params.size(), CodeLoc()), {});
  };
  return checkFun(f1, f2) && checkFun(f2, f1);
}

bool Context::isTemplated() const {
  for (auto& state : getReversedStates())
    if (state->templated)
      return true;
  return false;
}

void Context::setTemplated() {
  state->templated = true;
}

bool Context::isGeneralization(const SFunctionInfo& general, const SFunctionInfo& specific,
    vector<FunctionType> existing) const {
  // the name can change during instantation if name is a type (if it's a constructor)
  if (auto inst = instantiateFunction(*this, general, CodeLoc(), {}, transform(specific->type.params, [](const auto& param) { return param.type; }),
      vector<CodeLoc>(specific->type.params.size(), CodeLoc()), existing)) {
    return specific->type.retVal == inst.get()->type.retVal;
  }
  else
    return false;
}

optional<string> Context::getMissingFunctions(const Concept& required, vector<FunctionType> existing) const {
  vector<FunctionType> ret;
  for (auto otherState : required.getContext().getReversedStates()) {
    for (auto& overloads : otherState->functions)
      for (auto& function : overloads.second) {
        bool found = false;
        for (auto& myFun : getFunctions(overloads.first))
          if (isGeneralization(myFun, function, existing)) {
            found = true;
            break;
          }
        if (!found)
          return "Required function not implemented: " + function->prettyString() +
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

Context::ConstStates Context::getAllStates() const {
  auto ret = parentStates;
  ret.push_back(state);
  return ret;
}

Context::ConstStates Context::getTopLevelStates() const {
  return topLevelStates;
}

void Context::setAsTopLevel() {
  topLevelStates = parentStates;
}

Context Context::withStates(ConstStates states) {
  Context ret;
  ret.parentStates = std::move(states);
  return ret;
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
      cout << overload->prettyString() << "\n";
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
      overload = replaceInFunction(overload, from, to);
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

vector<SType> Context::getAllTypes() const {
  vector<SType> ret;
  for (auto& state : getReversedStates())
    for (auto& type : state->types)
      ret.push_back(type.second);
  return ret;
}

optional<string> Context::addImplicitFunction(FunctionId id, FunctionType type) {
  return addFunction(FunctionInfo::getImplicit(std::move(id), std::move(type)));
}

vector<SFunctionInfo> Context::getFunctions(FunctionId name) const {
  vector<SFunctionInfo> ret;
  for (auto& state : getReversedStates())
    if (state->functions.count(name))
      append(ret, state->functions.at(name));
  return ret;
}

vector<SFunctionInfo> Context::getAllFunctions() const {
  vector<SFunctionInfo> ret;
  for (auto& state : getReversedStates())
    for (auto& overloadSet : state->functions)
      for (auto& fun : overloadSet.second)
        ret.push_back(fun);
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
    return id.codeLoc.getError("Bad type identifier: " + id.prettyString());
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

optional<string> Context::addFunction(SFunctionInfo info) {
  auto& overloads = state->functions[info->id];
  for (auto& fun : overloads)
    if (areParamsEquivalent(fun, info) && (!info->id.contains<ConstructorTag>()
            || fun->type.retVal == info->type.retVal) &&
        fun->type.generatedConstructor == info->type.generatedConstructor)
      return "Can't overload " + info->prettyString() + " with the same argument types."s;
  overloads.push_back(info);
  return none;
}

vector<SFunctionInfo> Context::getConstructorsFor(const SType& type) const {
  vector<SFunctionInfo> ret;
  for (auto& fun : getFunctions(ConstructorTag{})) {
    if (fun->type.retVal == type) {
      ret.push_back(fun);
    } else
    if (auto structType = fun->type.retVal.dynamicCast<StructType>())
      if (structType->parent.get() == type) {
        ret.push_back(fun);
      }
  }
  return ret;
}

WithError<IdentifierType> Context::getIdentifierType(const IdentifierInfo& id) const {
  vector<FunctionInfo> ret;
  if (id.parts.size() > 1) {
    if (auto type = getTypeFromString(IdentifierInfo(id.parts.at(0), id.codeLoc))) {
      if (auto ret = type.get()->getStaticContext().getIdentifierType(id.getWithoutFirstPart())) {
        ret->parts = concat({IdentifierType::Part{{type.get()}}}, ret->parts);
        return ret;
      }
    }
    return "Type not found: " + id.prettyString();
  } else {
    string name = id.parts.at(0).name;
    if (auto type = getType(name))
      return IdentifierType(type.get());
    else
      return IdentifierType(name);
  }
}

WithError<vector<SFunctionInfo>> Context::getFunctionTemplate(IdentifierType id) const {
  vector<SFunctionInfo> ret;
  if (id.parts.size() > 1)
    return (*id.parts[0].name.getReferenceMaybe<SType>())->getStaticContext().getFunctionTemplate(id.getWithoutFirstPart());
  else
    return id.parts.at(0).name.visit(
          [&](const SType& type) { return getConstructorsFor(type); },
          [&](const string& name) { return getFunctions(name);}
    );
}

WithErrorLine<SFunctionInfo> Context::instantiateFunctionTemplate(CodeLoc codeLoc, SFunctionInfo templateType,
    vector<SType> templateParams, vector<SType> argTypes, vector<CodeLoc> argLoc) const {
  return instantiateFunction(*this, templateType, codeLoc, templateParams, argTypes, argLoc, {});
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
  CHECK(!addImplicitFunction(id, FunctionType(returnType,
      transform(argTypes, [](const auto& p){ return FunctionType::Param(p); }), {})));
  state->builtInFunctions.insert(make_pair(id, BuiltInFunctionInfo{std::move(argTypes), std::move(fun)}));
}

nullable<SFunctionInfo> Context::getBuiltinOperator(Operator op, vector<SType> argTypes) const {
  auto functionType = [&] () -> optional<FunctionType> {
    switch (op) {
      case Operator::SUBSCRIPT:
        if (argTypes.size() == 2)
          if (auto arrayType = argTypes[0]->getUnderlying().dynamicCast<ArrayType>())
            if (argTypes[1]->getUnderlying() == ArithmeticType::INT) {
              if (argTypes[0].dynamicCast<ReferenceType>())
                return FunctionType(
                    ReferenceType::get(arrayType->underlying), {{argTypes[0]}, {ArithmeticType::INT}}, {}).setBuiltin();
              if (argTypes[0].dynamicCast<MutableReferenceType>())
                return FunctionType(
                    MutableReferenceType::get(arrayType->underlying), {{argTypes[0]}, {ArithmeticType::INT}}, {}).setBuiltin();
              return FunctionType(arrayType->underlying, {{argTypes[0]}, {ArithmeticType::INT}}, {}).setBuiltin();
            }
        break;
      case Operator::GET_ADDRESS:
        if (argTypes.size() == 1) {
          if (auto referenceType = argTypes[0].dynamicCast<ReferenceType>())
            return FunctionType(PointerType::get(referenceType->underlying), {argTypes[0]}, {}).setBuiltin();
          else if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
            return FunctionType(MutablePointerType::get(referenceType->underlying), {argTypes[0]}, {}).setBuiltin();
        }
        break;
      case Operator::POINTER_DEREFERENCE:
        if (argTypes.size() == 1) {
          if (auto pointerType = argTypes[0]->getUnderlying().dynamicCast<PointerType>())
            return FunctionType(ReferenceType::get(pointerType->underlying), {argTypes[0]}, {}).setBuiltin();
          else if (auto pointerType = argTypes[0]->getUnderlying().dynamicCast<MutablePointerType>())
            return FunctionType(MutableReferenceType::get(pointerType->underlying), {argTypes[0]}, {}).setBuiltin();
        }
        break;
      case Operator::ASSIGNMENT:
        if (argTypes.size() == 2 && canConvert(argTypes[1], argTypes[0]->getUnderlying()))
          if (auto referenceType = argTypes[0].dynamicCast<MutableReferenceType>())
            return FunctionType(ArithmeticType::VOID, {argTypes[0], referenceType->underlying}, {}).setBuiltin();
        break;
      default:
        break;
    }
    return none;
  }();
  if (functionType)
    return FunctionInfo::getImplicit(op, *functionType);
  return nullptr;
}

vector<SFunctionInfo> Context::getOperatorType(Operator op) const {
  return getFunctions(op);
}

bool Context::canDefaultInitialize(SType type) const {
  if (auto s = type.dynamicCast<StructType>())
    type = s->parent.get();
  for (auto& f : getConstructorsFor(type))
    if (f->type.params.empty())
      return true;
  return false;
}
