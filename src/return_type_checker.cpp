#include "return_type_checker.h"
#include "context.h"
#include "type.h"

ReturnTypeChecker::ReturnTypeChecker(Type* explicitReturn) : explicitReturn(std::move(explicitReturn)) {

}

JustError<string> ReturnTypeChecker::addReturnStatement(const Context& context, Type* returnType, unique_ptr<Expression>& expr) {
  auto underlying = returnType->removeReference();
  if (explicitReturn) {
    if (explicitReturn == BuiltinType::NORETURN)
      return "This function should never return"s;
  } else
  if (returnStatement && returnStatement != underlying)
    return "Ambigous implicit return type: " + quote(returnStatement->getName()) + " and " + quote(underlying->getName());
  else
    returnStatement = underlying;
  auto toConvert = explicitReturn ? explicitReturn : returnStatement;
  if (auto res = context.canConvert(returnType, toConvert, expr); !!res)
    return success;
  else
    return "Can't return value of type " + quote(returnType->getName()) +
        " from a function returning " + toConvert->getName() + ":\n" + res.get_error();
}

Type* ReturnTypeChecker::getReturnType() const {
  if (explicitReturn)
    return explicitReturn;
  if (returnStatement)
    return returnStatement;
  return BuiltinType::VOID;
}
