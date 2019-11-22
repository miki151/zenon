#include "return_type_checker.h"
#include "context.h"
#include "type.h"

ReturnTypeChecker::ReturnTypeChecker(nullable<SType> explicitReturn) : explicitReturn(std::move(explicitReturn)) {

}

optional<string> ReturnTypeChecker::addReturnStatement(const Context& context, SType t) {
  auto underlying = t->getUnderlying();
  if (explicitReturn) {
    if (explicitReturn == ArithmeticType::NORETURN)
      return "This function should never return"s;
    if (underlying == ArithmeticType::VOID) {
      if (explicitReturn != ArithmeticType::VOID)
        return "Expected an expression in return statement in a function returning non-void"s;
    }
  } else
  if (returnStatement && returnStatement != underlying)
    return "Ambigous implicit return type: " + quote(returnStatement->getName()) + " and " + quote(underlying->getName());
  else
    returnStatement = underlying;
  auto toConvert = explicitReturn.value_or([&] { return returnStatement.get();});
  if (!context.canConvert(t, toConvert))
    return "Can't return value of type " + quote(t->getName()) + " from a function returning " + toConvert->getName();
  return none;
}

SType ReturnTypeChecker::getReturnType() const {
  if (explicitReturn)
    return explicitReturn.get();
  if (returnStatement)
    return returnStatement.get();
  return ArithmeticType::VOID;
}
