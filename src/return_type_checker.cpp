#include "return_type_checker.h"
#include "context.h"
#include "type.h"

ReturnTypeChecker::ReturnTypeChecker(nullable<SType> explicitReturn) : explicitReturn(std::move(explicitReturn)) {

}

optional<string> ReturnTypeChecker::addReturnStatement(const Context& context, SType t) {
  t = t->getUnderlying();
  if (explicitReturn) {
    if (explicitReturn == ArithmeticType::NORETURN)
      return "This function should never return"s;
    if (t == ArithmeticType::VOID) {
      if (explicitReturn != ArithmeticType::VOID)
        return "Expected an expression in return statement in a function returning non-void"s;
    } else
    if (!context.canConvert(t, explicitReturn.get()))
      return "Can't return value of type " + quote(t->getName()) + " from a function returning " + explicitReturn->getName();
    return none;
  } else
  if (returnStatement && returnStatement != t)
    return "Ambigous implicit return type: " + quote(returnStatement->getName()) + " and " + quote(t->getName());
  else
    returnStatement = t;
  return none;
}

SType ReturnTypeChecker::getReturnType() const {
  if (explicitReturn)
    return explicitReturn.get();
  if (returnStatement)
    return returnStatement.get();
  return ArithmeticType::VOID;
}
