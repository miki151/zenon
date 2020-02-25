#include "return_type_checker.h"
#include "context.h"
#include "type.h"

ReturnTypeChecker::ReturnTypeChecker(nullable<SType> explicitReturn) : explicitReturn(std::move(explicitReturn)) {

}

JustError<string> ReturnTypeChecker::addReturnStatement(const Context& context, SType returnType, unique_ptr<Expression>& expr) {
  auto underlying = returnType->removeReference();
  if (explicitReturn) {
    if (explicitReturn == BuiltinType::NORETURN)
      return "This function should never return"s;
    if (underlying == BuiltinType::VOID) {
      if (explicitReturn != BuiltinType::VOID)
        return "Expected an expression in return statement in a function returning non-void"s;
    }
  } else
  if (returnStatement && returnStatement != underlying)
    return "Ambigous implicit return type: " + quote(returnStatement->getName()) + " and " + quote(underlying->getName());
  else
    returnStatement = underlying;
  auto toConvert = explicitReturn.value_or([&] { return returnStatement.get();});
  if (context.canConvert(returnType, toConvert, expr))
    return success;
  else
    return "Can't return value of type " + quote(returnType->getName()) + " from a function returning " + toConvert->getName();
}

SType ReturnTypeChecker::getReturnType() const {
  if (explicitReturn)
    return explicitReturn.get();
  if (returnStatement)
    return returnStatement.get();
  return BuiltinType::VOID;
}
