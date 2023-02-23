#include "stdafx.h"
#include <vector>
#include "parser.h"
#include "token.h"
#include "ast.h"
#include "lexer.h"

WithErrorLine<unique_ptr<Expression>> parseExpression(Tokens&, int minPrecedence = 0);
WithErrorLine<unique_ptr<Expression>> parsePrimary(Tokens&);
WithErrorLine<unique_ptr<Statement>> parseTopLevelStatement(Tokens&);
WithErrorLine<unique_ptr<VariableDeclaration>> parseVariableDeclaration(Tokens&, bool eatSemicolon = true,
    bool forLoop = false);

template <typename T>
static shared_ptr<T> getSharedPtr(unique_ptr<T> p) {
  return shared_ptr<T>(p.release());
}

static WithErrorLine<IdentifierInfo> parseIdentifier(Tokens& tokens, bool allowPointer) {
  IdentifierInfo ret;
  if (tokens.eatMaybe(Keyword::DOLLAR)) {
    ret.typeExpression = getSharedPtr(TRY(parsePrimary(tokens)));
    ret.codeLoc = ret.typeExpression->codeLoc;
  } else
    while (1) {
      if (!tokens.peek().contains<IdentifierToken>())
        return tokens.peek().codeLoc.getError("Expected identifier");
      ret.parts.emplace_back();
      auto& token = tokens.popNext();
      ret.codeLoc = token.codeLoc;
      ret.parts.back().name = token.value;
      ret.parts.back().codeLoc = token.codeLoc;
      auto beforeLessThan = tokens.getBookmark();
      if (tokens.eatMaybe(Operator::LESS_THAN)) {
        while (1) {
          if (auto ident = parseIdentifier(tokens, true)) {
            if (tokens.peek() == Keyword::COMMA || tokens.peek() == Keyword::MORE_THAN || tokens.peek() == Keyword::ELLIPSIS)
              ret.parts.back().templateArguments.push_back(*ident);
            else {
              tokens.rewind(beforeLessThan);
              ret.parts.back().templateArguments.clear();
              break;
            }
          } else {
            if (auto expr = parsePrimary(tokens)) {
              if (tokens.peek() == Keyword::COMMA || tokens.peek() == Keyword::MORE_THAN || tokens.peek() == Keyword::ELLIPSIS)
                ret.parts.back().templateArguments.push_back(getSharedPtr(std::move(*expr)));
              else {
                ret.parts.back().templateArguments.clear();
                tokens.rewind(beforeLessThan);
                break;
              }
            } else
              return expr.get_error();
          }
          if (tokens.eatMaybe(Keyword::ELLIPSIS))
            ret.parts.back().variadic = true;
          if (tokens.eatMaybe(Keyword::MORE_THAN))
            break;
          else if (!tokens.eatMaybe(Keyword::COMMA)) {
            ret.parts.back().templateArguments.clear();
            tokens.rewind(beforeLessThan);
            break;
          }
        }
      }
      if (tokens.peek() == Keyword::NAMESPACE_ACCESS) {
        tokens.popNext();
        continue;
      } else
        break;
    }
  if (allowPointer) {
    while (1) {
      if (auto t = tokens.eatMaybe(Keyword::MUTABLE)) {
        if (tokens.eatMaybe(Operator::MULTIPLY))
          ret.typeOperator.push_back(IdentifierInfo::MUTABLE);
        else if (tokens.eatMaybe(Keyword::OPEN_SQUARE_BRACKET)) {
          TRY(tokens.eat(Keyword::CLOSE_SQUARE_BRACKET));
          auto arg = ret;
          ret = IdentifierInfo("mutable_slice_t", ret.codeLoc);
          ret.parts[0].templateArguments.push_back(std::move(arg));
        } else
          return tokens.peek().codeLoc.getError("Expected a pointer or slice modifier");
      } else
      if (auto t = tokens.eatMaybe(Operator::MULTIPLY)) {
        ret.typeOperator.push_back(IdentifierInfo::CONST);
      } else
      if (auto t = tokens.eatMaybe(Operator::MAYBE)) {
        ret.typeOperator.push_back(IdentifierInfo::Optional{});
      } else
      if (auto t = tokens.eatMaybe(Operator::LOGICAL_NOT)) {
        ret.typeOperator.push_back(TRY(parseIdentifier(tokens, false)));
      } else
      if (auto t = tokens.eatMaybe(Keyword::OPEN_SQUARE_BRACKET)) {
        if (tokens.peek() != Keyword::CLOSE_SQUARE_BRACKET) {
          ret.typeOperator.push_back(IdentifierInfo::ArraySize{getSharedPtr(TRY(parseExpression(tokens)))});
        } else {
          auto arg = ret;
          ret = IdentifierInfo("slice_t", ret.codeLoc);
          ret.parts[0].templateArguments.push_back(std::move(arg));
        }
        TRY(tokens.eat(Keyword::CLOSE_SQUARE_BRACKET));
      } else
        break;
    }
  }
  //INFO << "Identifier " << ret.toString();
  return ret;
}

WithErrorLine<unique_ptr<FunctionCall>> parseFunctionCall(IdentifierInfo id, Tokens& tokens) {
  auto ret = make_unique<FunctionCall>(id, false);
  TRY(tokens.eat(Keyword::OPEN_BRACKET));
  while (tokens.peek() != Keyword::CLOSE_BRACKET) {
    if (tokens.eatMaybe(Keyword::MEMBER_ACCESS)) {
      auto idToken = tokens.popNext();
      if (!idToken.contains<IdentifierToken>())
        return idToken.codeLoc.getError("Expected function parameter name");
      ret->argNames.push_back(idToken.value);
      TRY(tokens.eat(Operator::ASSIGNMENT));
    }
    ret->arguments.push_back(TRY(parseExpression(tokens)));
    if (tokens.peek() == Keyword::COMMA)
      tokens.popNext();
    else if (tokens.eatMaybe(Keyword::ELLIPSIS)) {
      ret->variadicArgs = true;
      break;
    } else
      break;
  }
  if (!ret->argNames.empty() && ret->argNames.size() < ret->arguments.size())
    return ret->codeLoc.getError("When using named arguments, all arguments must be named.");
  ret->endLoc = TRY(tokens.eat(Keyword::CLOSE_BRACKET)).codeLoc;
  return std::move(ret);
}

static string getInputReg() {
  return "((?:(?:\\\\\\{)|[^\\{])+)|(\\{.*?\\})|(\\{.*)";
}

WithErrorLine<unique_ptr<Expression>> parseStringLiteral(CodeLoc initialLoc, string literal) {
  if (literal.empty())
    return cast<Expression>(make_unique<Constant>(initialLoc, CompileTimeValue::get(""s)));
  unique_ptr<Expression> left;
  auto addElem = [&left] (unique_ptr<Expression> right, CodeLoc loc) {
    if (!left)
      left = std::move(right);
    else
      left = BinaryExpression::get(loc, Operator::PLUS, std::move(left), std::move(right));
  };
  regex re(getInputReg());
  auto words_begin = sregex_iterator(literal.begin(), literal.end(), re);
  auto words_end = sregex_iterator();
  for (auto it = words_begin; it != words_end; ++it)
    for (int index = 0 ; index < it->size(); ++index)
      if (!it->str(index + 1).empty()) { // determine which submatch was matched
        CodeLoc loc = initialLoc.plus(0, (int) it->position());
        //cout << "Matched \"" << it->str() << "\" with rule " << index << " at " << loc.line << ":" << loc.column << endl;
        if (index == 0) {
          addElem(make_unique<Constant>(loc, CompileTimeValue::get(
              regex_replace(it->str(), regex("\\\\([\\{\\}])"), "$1"))), loc);
        } else if (index == 1) {
          loc = loc.plus(0, 2);
          auto tokens = lex(it->str().substr(1, it->str().size() - 2), loc, "end of expression").get();
          auto call = cast<Expression>(make_unique<FunctionCall>(IdentifierInfo("to_string", loc),
              make_unique<UnaryExpression>(loc, Operator::GET_ADDRESS, TRY(parseExpression(tokens))), false));
          addElem(std::move(call), loc);
        } else {
          return loc.getError("Unmatched " + quote("{"));
        }
      }
  return std::move(left);
}

static WithErrorLine<unique_ptr<Expression>> parseArrayLiteral(Tokens& tokens) {
  auto ret = make_unique<ArrayLiteral>(tokens.peek().codeLoc);
  TRY(tokens.eat(Keyword::OPEN_BLOCK));
  if (tokens.eatMaybe(Operator::LESS_THAN)) {
    ret->typeId = TRY(parseIdentifier(tokens, true));
    TRY(tokens.eat(Keyword::MORE_THAN));
  }
  if (!tokens.eatMaybe(Keyword::CLOSE_BLOCK))
    while (1) {
      ret->contents.push_back(TRY(parseExpression(tokens)));
      if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
        break;
      else
        TRY(tokens.eat(Keyword::COMMA));
    }
  if (ret->contents.empty() && !ret->typeId)
    return ret->codeLoc.getError("Empty array literal must include type specifier, ex. " + quote("{}<int>"));
  return cast<Expression>(std::move(ret));
}

WithErrorLine<unique_ptr<StatementBlock>> parseBlock(Tokens&);

WithErrorLine<unique_ptr<Expression>> parseLambda(Tokens& tokens) {
  LambdaCaptureInfo captureInfo;
  auto beginToken = *tokens.eat(Keyword::OPEN_SQUARE_BRACKET);
  while (!tokens.eatMaybe(Keyword::CLOSE_SQUARE_BRACKET)) {
    auto captureType = LambdaCaptureType::IMPLICIT_COPY;
    bool closeBracket = false;
    if (tokens.eatMaybe(Operator::GET_ADDRESS)) {
      if (tokens.peek() == Keyword::COMMA || tokens.peek() == Keyword::CLOSE_SQUARE_BRACKET) {
        tokens.eatMaybe(Keyword::COMMA);
        captureInfo.defaultCapture = LambdaCaptureType::REFERENCE;
        continue;
      }
      captureType = LambdaCaptureType::REFERENCE;
    }
    else if (tokens.eatMaybe(Keyword::MOVE)) {
      TRY(tokens.eat(Keyword::OPEN_BRACKET));
      captureType = LambdaCaptureType::MOVE;
      closeBracket = true;
    } else
    if (tokens.peek().value == "copy") {
      tokens.popNext();
      TRY(tokens.eat(Keyword::OPEN_BRACKET));
      captureType = LambdaCaptureType::COPY;
      closeBracket = true;
    }
    auto id = TRY(tokens.eat<IdentifierToken>("Expected variable name"));
    if (closeBracket)
      TRY(tokens.eat(Keyword::CLOSE_BRACKET));
    captureInfo.captures.push_back(LambdaCaptureInfo::Var{id.value, id.codeLoc, captureType, false});
    if (tokens.peek() != Keyword::CLOSE_SQUARE_BRACKET)
      TRY(tokens.eat(Keyword::COMMA));
  }
  vector<FunctionParameter> params;
  if (!!tokens.eat(Keyword::OPEN_BRACKET))
    while (1) {
      if (tokens.eatMaybe(Keyword::CLOSE_BRACKET))
        break;
      if (!params.empty())
        TRY(tokens.eat(Keyword::COMMA));
      bool isParamMutable = !!tokens.eatMaybe(Keyword::MUTABLE);
      auto typeId = TRY(parseIdentifier(tokens, true));
      auto paramCodeLoc = typeId.codeLoc;
      optional<string> paramName;
      auto nameToken = tokens.peek();
      if (nameToken.contains<IdentifierToken>()) {
        paramName = nameToken.value;
        paramCodeLoc = nameToken.codeLoc;
        tokens.popNext();
      }
      params.push_back({paramCodeLoc, typeId, paramName, isParamMutable, false});
    }
  optional<IdentifierInfo> returnType;
  if (tokens.eat(Keyword::LAMBDA_RETURN_TYPE))
    returnType = TRY(parseIdentifier(tokens, true));
  return cast<Expression>(make_unique<LambdaExpression>(beginToken.codeLoc, std::move(params), TRY(parseBlock(tokens)),
      std::move(returnType), std::move(captureInfo)));
}

WithErrorLine<unique_ptr<Expression>> parsePrimary(Tokens& tokens) {
  auto& token = tokens.peek();
  return token.visit(
      [&](const Keyword& k) -> WithErrorLine<unique_ptr<Expression>> {
        switch (k) {
          case Keyword::OPEN_SQUARE_BRACKET: {
            return parseLambda(tokens);
          }
          case Keyword::OPEN_BRACKET: {
            tokens.popNext();
            auto ret = TRY(parseExpression(tokens));
            TRY(tokens.eat(Keyword::CLOSE_BRACKET));
            return std::move(ret);
          }
          case Keyword::MOVE: {
            tokens.popNext();
            TRY(tokens.eat(Keyword::OPEN_BRACKET));
            auto var = tokens.popNext();
            TRY(tokens.eat(Keyword::CLOSE_BRACKET));
            if (!var.contains<IdentifierToken>())
              return var.codeLoc.getError("Expected variable identifier");
            return cast<Expression>(make_unique<MoveExpression>(token.codeLoc, var.value, var.codeLoc));
          }
          case Keyword::TRY : {
            tokens.popNext();
// try(expr) = ({ const x = expr; if (!x) return move(x).get_error(); return *move(x); })
            auto varName = "var1234"s;
            auto codeLoc = token.codeLoc;
            auto varExpr = cast<Expression>(make_unique<Variable>(IdentifierInfo(varName, codeLoc)));
            auto statements = makeVec<unique_ptr<Statement>>(
                cast<Statement>(make_unique<VariableDeclaration>(codeLoc, none, varName, TRY(parseExpression(tokens, 10)))),
                cast<Statement>(make_unique<IfStatement>(codeLoc, nullptr,
                    cast<Expression>(make_unique<UnaryExpression>(codeLoc, Operator::LOGICAL_NOT, varExpr->deepCopy())),
                    cast<Statement>(make_unique<ReturnStatement>(codeLoc,
                        cast<Expression>(make_unique<FunctionCall>(IdentifierInfo("get_error", codeLoc),
                            cast<Expression>(make_unique<MoveExpression>(codeLoc, varName, token.codeLoc)), true)))),
                    nullptr
                ))
            );
            return cast<Expression>(make_unique<StatementExpression>(codeLoc, std::move(statements),
                cast<Expression>(make_unique<UnaryExpression>(codeLoc, Operator::POINTER_DEREFERENCE,
                    cast<Expression>(make_unique<MoveExpression>(codeLoc, varName, token.codeLoc))))));
          }
          case Keyword::COUNTOF: {
            tokens.popNext();
            TRY(tokens.eat(Keyword::OPEN_BRACKET));
            auto var = tokens.popNext();
            TRY(tokens.eat(Keyword::CLOSE_BRACKET));
            if (!var.contains<IdentifierToken>())
              return var.codeLoc.getError("Expected variable identifier");
            return cast<Expression>(make_unique<CountOfExpression>(token.codeLoc, var.value));
          }
          case Keyword::FALSE:
            tokens.popNext();
            return cast<Expression>(make_unique<Constant>(token.codeLoc, CompileTimeValue::get(false)));
          case Keyword::TRUE:
            tokens.popNext();
            return cast<Expression>(make_unique<Constant>(token.codeLoc, CompileTimeValue::get(true)));
          case Keyword::NULL_TOKEN:
            tokens.popNext();
            return cast<Expression>(make_unique<Constant>(token.codeLoc, CompileTimeValue::get(CompileTimeValue::NullValue{})));
          case Keyword::OPEN_BLOCK:
            return parseArrayLiteral(tokens);
          default:
            return token.codeLoc.getError("Expected primary expression, got: " + quote(token.value));
        }
      },
      [&](const IdentifierToken&) -> WithErrorLine<unique_ptr<Expression>> {
        auto identifier = TRY(parseIdentifier(tokens, false));
        if (tokens.peek() == Keyword::OPEN_BRACKET)
          return cast<Expression>(TRY(parseFunctionCall(identifier, tokens)));
        else if (tokens.peek() == Keyword::ELLIPSIS && tokens.peekNext() == Keyword::OPEN_SQUARE_BRACKET) {
          tokens.popNext();
          TRY(tokens.eat(Keyword::OPEN_SQUARE_BRACKET));
          auto expr = TRY(parseExpression(tokens));
          TRY(tokens.eat(Keyword::CLOSE_SQUARE_BRACKET));
          if (auto id = identifier.asBasicIdentifier())
            return cast<Expression>(make_unique<VariablePackElement>(identifier.codeLoc, *id, std::move(expr)));
          else
            return identifier.codeLoc.getError("Expected variable pack identifier");
        } else {
          if (identifier.parts.size() == 1)
            return cast<Expression>(make_unique<Variable>(identifier));
          else
            return cast<Expression>(make_unique<EnumConstant>(token.codeLoc, identifier.parts[0].name,
                identifier.parts[1].codeLoc, identifier.parts[1].name));
        }
      },
      [&](const Number&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        int ret = 0;
        try {
          ret = stoi(token.value);
        } catch (std::out_of_range e) {
          return token.codeLoc.getError("Constant value out of range");
        }
        return cast<Expression>(make_unique<Constant>(token.codeLoc, CompileTimeValue::get(ret)));
      },
      [&](const RealNumber&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        return cast<Expression>(make_unique<Constant>(token.codeLoc, CompileTimeValue::get(stod(token.value))));
      },
      [&](const StringToken&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        return parseStringLiteral(token.codeLoc, token.value);
      },
      [&](const CharToken&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        return cast<Expression>(make_unique<Constant>(token.codeLoc,
            CompileTimeValue::get(CompileTimeValue::CharLiteral{token.value})));
      },
      [&](const Operator& op) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        if (auto opUnary = getUnary(op)) {
          return cast<Expression>(make_unique<UnaryExpression>(token.codeLoc, *opUnary,
              TRY(parseExpression(tokens, getPrecedence(*opUnary) + 1))));
        } else
          return token.codeLoc.getError(getString(op) + " is not a unary operator"s);
      },
      [&](const auto&) -> WithErrorLine<unique_ptr<Expression>> {
        return token.codeLoc.getError("Expected primary expression, got: " + quote(token.value));
      }
  );
}

WithErrorLine<unique_ptr<Expression>> parseFunctionCall(Tokens& tokens, unique_ptr<Expression> lhs) {
  while (tokens.peek() == Keyword::OPEN_BRACKET) {
    auto codeLoc = lhs->codeLoc;
    auto call = TRY(parseFunctionCall(IdentifierInfo("invoke", codeLoc), tokens));
    call->arguments = concat(makeVec(std::move(lhs)), std::move(call->arguments));
    call->methodCall = true;
    lhs = cast<Expression>(std::move(call));
  }
  return std::move(lhs);
}

WithErrorLine<unique_ptr<Expression>> parseMemberAccess(Tokens& tokens, unique_ptr<Expression> lhs) {
  if (auto id = parseIdentifier(tokens, false)) {
    if (tokens.peek() == Keyword::OPEN_BRACKET) {
      auto function = TRY(parseFunctionCall(*id, tokens));
      function.get()->arguments = concat(makeVec(std::move(lhs)), std::move(function->arguments));
      function.get()->methodCall = true;
      return cast<Expression>(std::move(function));
    } else if (auto s = id->asBasicIdentifier())
      return cast<Expression>(make_unique<MemberAccessExpression>(id->codeLoc, std::move(lhs), *s));
    else
      return id->codeLoc.getError("Expected member name or method call");
  } else
    return tokens.peek().codeLoc.getError("Bad use of member access operator");
}

struct OperatorInfo {
  Operator op;
  bool reverse;
  bool negate;
};

static optional<OperatorInfo> getOperator(const Token& token) {
  if (auto op = token.getValueMaybe<Operator>())
    return OperatorInfo{*op, false, false};
  if (auto k = token.getValueMaybe<Keyword>())
    switch (*k) {
      case Keyword::MORE_THAN: return OperatorInfo{Operator::LESS_THAN, true, false};
      case Keyword::LESS_OR_EQUAL: return OperatorInfo{Operator::LESS_THAN, true, true};
      case Keyword::MORE_OR_EQUAL: return OperatorInfo{Operator::LESS_THAN, false, true};
      default:
        break;
    }
  return none;
}

WithErrorLine<unique_ptr<Expression>> parseExpressionImpl(Tokens& tokens, unique_ptr<Expression> lhs,
    int minPrecedence) {
  while (1) {
    auto* token = &tokens.peek();
    if (auto op1 = getOperator(*token)) {
      if (getPrecedence(op1->op) < minPrecedence)
        break;
      unique_ptr<Expression> mhs;
      tokens.popNext();
      if (op1->op == Operator::MAYBE) {
        mhs = TRY(parseExpression(tokens));
        TRY(tokens.eat(Keyword::COLON));
      }
      auto rhs = TRY(parsePrimary(tokens));
      while (1) {
        token = &tokens.peek();
        auto op2 = getOperator(*token);
        if (*token == Keyword::OPEN_SQUARE_BRACKET)
          op2 = OperatorInfo{Operator::SUBSCRIPT, false, false};
        if (op2) {
          if (getPrecedence(op2->op) <= getPrecedence(op1->op) &&
              (!isRightAssociative(op2->op) || getPrecedence(op2->op) < getPrecedence(op1->op)))
            break;
          rhs = TRY(parseExpressionImpl(tokens, std::move(rhs), getPrecedence(op2->op)));
        } else
        if (*token == Keyword::POINTER_DEREFERENCE_POSTFIX) {
          tokens.popNext();
          rhs = make_unique<UnaryExpression>(token->codeLoc, Operator::POINTER_DEREFERENCE, std::move(rhs));
        } else
        if (*token == Keyword::MEMBER_ACCESS) {
          tokens.popNext();
          rhs = TRY(parseMemberAccess(tokens, std::move(rhs)));
        } else if (*token == Keyword::OPEN_BRACKET)
          rhs = TRY(parseFunctionCall(tokens, std::move(rhs)));
        else
          break;
      }
      if (mhs)
        lhs = cast<Expression>(make_unique<TernaryExpression>(token->codeLoc, std::move(lhs), std::move(mhs), std::move(rhs)));
      else {
        if (op1->reverse)
          swap(lhs, rhs);
        lhs = BinaryExpression::get(token->codeLoc, op1->op, std::move(lhs), std::move(rhs));
        if (op1->negate)
          lhs = make_unique<UnaryExpression>(token->codeLoc, Operator::LOGICAL_NOT, std::move(lhs));
      }
    } else
    if (*token == Keyword::POINTER_DEREFERENCE_POSTFIX) {
      tokens.popNext();
      lhs = make_unique<UnaryExpression>(token->codeLoc, Operator::POINTER_DEREFERENCE, std::move(lhs));
    } else
    if (*token == Keyword::OPEN_SQUARE_BRACKET) {
      tokens.popNext();
      auto rhs = TRY(parseExpression(tokens));
      TRY(tokens.eat(Keyword::CLOSE_SQUARE_BRACKET));
      lhs = BinaryExpression::get(token->codeLoc, Operator::SUBSCRIPT, std::move(lhs), std::move(rhs));
    } else
    if (*token == Keyword::MEMBER_ACCESS) {
      tokens.popNext();
      lhs = TRY(parseMemberAccess(tokens, std::move(lhs)));
    } else
    if (*token == Keyword::OPEN_BRACKET)
      lhs = TRY(parseFunctionCall(tokens, std::move(lhs)));
    else
      break;
  }
  return std::move(lhs);
}

WithErrorLine<unique_ptr<Expression>> parseExpression(Tokens& tokens, int minPrecedence) {
  return parseExpressionImpl(tokens, TRY(parsePrimary(tokens)), minPrecedence);
}

WithErrorLine<unique_ptr<Statement>> parseNonTopLevelStatement(Tokens&);

WithErrorLine<unique_ptr<StatementBlock>> parseBlock(Tokens& tokens) {
  auto openBlock = TRY(tokens.eat(Keyword::OPEN_BLOCK));
  auto block = make_unique<StatementBlock>(openBlock.codeLoc);
  while (1) {
    auto& token2 = tokens.peek();
    if (token2 == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    block->elems.push_back(TRY(parseNonTopLevelStatement(tokens)));
  }
  return std::move(block);
}

WithErrorLine<unique_ptr<FunctionDefinition>> parseFunctionSignature(IdentifierInfo type, Tokens& tokens) {
  auto& token2 = tokens.popNext();
  unique_ptr<FunctionDefinition> ret;
  if (token2 == Keyword::OPERATOR) {
    if (auto op = tokens.peek().getValueMaybe<Operator>()) {
      tokens.popNext();
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, *op, token2.codeLoc);
    } else {
      TRY(tokens.eat(Keyword::OPEN_SQUARE_BRACKET));
      TRY(tokens.eat(Keyword::CLOSE_SQUARE_BRACKET));
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, Operator::SUBSCRIPT, token2.codeLoc);
    }
  } else if (token2 == Keyword::OPEN_BRACKET) {
    if (auto typeName = type.asBasicIdentifier()) {
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, ConstructorTag { }, type.codeLoc);
      tokens.rewind();
    } else
      return type.codeLoc.getError("Expected type name without template parameters");
  } else {
    if (!token2.contains<IdentifierToken>())
      return token2.codeLoc.getError("Expected identifier, got: " + quote(token2.value));
    if (token2.value == "builtin_has_attribute")
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, AttributeTag{}, token2.codeLoc);
    else if (token2.value == "builtin_has_members")
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, StructMembersTag{}, token2.codeLoc);
    else if (token2.value == "builtin_has_member")
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, StructMemberTag{}, token2.codeLoc);
    else if (token2.value == "builtin_has_alternative")
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, UnionAlternativeTag{}, token2.codeLoc);
    else
      ret = make_unique<FunctionDefinition>(type.codeLoc, type, token2.value, token2.codeLoc);
  }
  TRY(tokens.eat(Keyword::OPEN_BRACKET));
  while (1) {
    if (tokens.eatMaybe(Keyword::CLOSE_BRACKET))
      break;
    else if (ret->isVariadicParams)
      return tokens.peek().codeLoc.getError("Function parameter pack is only allowed at the end of parameter list");
    if (!ret->parameters.empty())
      TRY(tokens.eat(Keyword::COMMA));
    bool isParamMutable = !!tokens.eatMaybe(Keyword::MUTABLE);
    bool isParamVirtual = !!tokens.eatMaybe(Keyword::VIRTUAL);
    isParamMutable |= !!tokens.eatMaybe(Keyword::MUTABLE);
    if (isParamMutable && isParamVirtual)
      return tokens.peek().codeLoc.getError("Parameter can't be both mutable and virtual");
    ret->isVirtual |= isParamVirtual;
    auto typeId = TRY(parseIdentifier(tokens, true));
    auto paramCodeLoc = typeId.codeLoc;
    ret->isVariadicParams = !!tokens.eatMaybe(Keyword::ELLIPSIS);
    optional<string> paramName;
    auto nameToken = tokens.peek();
    if (nameToken.contains<IdentifierToken>()) {
      paramName = nameToken.value;
      paramCodeLoc = nameToken.codeLoc;
      tokens.popNext();
    }
    ret->parameters.push_back({paramCodeLoc, typeId, paramName, isParamMutable, isParamVirtual});
  }
  if (ret->parameters.size() == 1)
    if (auto op = ret->name.getValueMaybe<Operator>())
      if (auto opUnary = getUnary(*op))
        ret->name = *opUnary;
  return std::move(ret);
}

WithErrorLine<unique_ptr<FunctionDefinition>> parseFunctionDefinition(IdentifierInfo type, Tokens& tokens) {
  auto ret = TRY(parseFunctionSignature(type, tokens));
  if (tokens.eatMaybe(Operator::ASSIGNMENT)) {
    TRY(tokens.eat(Keyword::DEFAULT));
    ret.get()->isDefault = true;
    TRY(tokens.eat(Keyword::SEMICOLON));
  }
  else if (!ret.get()->isVirtual || !tokens.eatMaybe(Keyword::SEMICOLON)) {
    if (tokens.peek().contains<EmbedToken>()) {
      ret.get()->body = make_unique<StatementBlock>(tokens.peek().codeLoc, makeVec(
          TRY(parseStatement(tokens, false))
      ));
    } else
      ret.get()->body = TRY(parseBlock(tokens));
  }
  return std::move(ret);
}

static WithErrorLine<TemplateInfo> parseTemplateInfo(Tokens& tokens) {
  TRY(tokens.eat(Operator::LESS_THAN));
  TemplateInfo ret;
  while (tokens.peek() != Keyword::MORE_THAN) {
    auto paramToken = tokens.popNext();
    if (ret.variadic)
      return paramToken.codeLoc.getError("Parameter pack is only allowed at the end of parameter list");
    if (!paramToken.contains<IdentifierToken>())
      return paramToken.codeLoc.getError("Template parameter name expected");
    optional<string> typeName;
    if (tokens.eatMaybe(Keyword::ELLIPSIS))
      ret.variadic = true;
    if (tokens.peek() != Keyword::MORE_THAN && !tokens.eatMaybe(Keyword::COMMA)) {
      typeName = paramToken.value;
      paramToken = tokens.popNext();
      if (!paramToken.contains<IdentifierToken>())
        return paramToken.codeLoc.getError("Template parameter name expected");
      if (tokens.peek() != Keyword::MORE_THAN)
        TRY(tokens.eat(Keyword::COMMA));
    }
    ret.params.push_back({paramToken.value, typeName, paramToken.codeLoc});
  }
  CHECK(!!tokens.eat(Keyword::MORE_THAN));
  if (tokens.eatMaybe(Keyword::REQUIRES))
    while (1) {
      if (tokens.eatMaybe(Keyword::OPEN_BRACKET)) {
        ret.requirements.push_back(getSharedPtr(TRY(parseExpression(tokens))));
        TRY(tokens.eat(Keyword::CLOSE_BRACKET));
      } else {
        auto id = TRY(parseIdentifier(tokens, false));
        bool variadic = !!tokens.eatMaybe(Keyword::ELLIPSIS);
        ret.requirements.push_back(TemplateInfo::ConceptRequirement{id, variadic});
      }
      if (!tokens.eatMaybe(Keyword::COMMA))
        break;
    }
  return ret;
}

WithErrorLine<unique_ptr<StructDefinition>> parseStructDefinition(Tokens& tokens, bool external) {
  TRY(tokens.eat(Keyword::STRUCT));
  auto& token2 = tokens.popNext();
  if (!token2.contains<IdentifierToken>())
    return token2.codeLoc.getError("Expected struct name");
  auto ret = make_unique<StructDefinition>(token2.codeLoc, token2.value);
  if (external)
    ret->external = true;
  if (tokens.eatMaybe(Keyword::OPEN_BLOCK))
    while (1) {
      auto memberToken = tokens.peek();
      if (memberToken == Keyword::CLOSE_BLOCK) {
        tokens.popNext();
        break;
      }
      bool isConst = !!tokens.eatMaybe(Keyword::CONST);
      auto typeIdent = TRY(parseIdentifier(tokens, true));
      auto memberName = tokens.popNext();
      if (!memberName.contains<IdentifierToken>())
        return memberName.codeLoc.getError("Expected identifier");
      unique_ptr<Expression> defaultExpr;
      if (tokens.eatMaybe(Operator::ASSIGNMENT))
        defaultExpr = TRY(parseExpression(tokens));
      ret->members.push_back({typeIdent, memberName.value, memberName.codeLoc, isConst,
          getSharedPtr(std::move(defaultExpr))});
      TRY(tokens.eat(Keyword::SEMICOLON));
    }
  else if (!external)
    return tokens.peek().codeLoc.getError("Expected struct definition");
  TRY(tokens.eat(Keyword::SEMICOLON));
  return std::move(ret);
}

WithErrorLine<unique_ptr<ConceptDefinition>> parseConceptDefinition(Tokens& tokens) {
  TRY(tokens.eat(Keyword::CONCEPT));
  auto templateInfo = TRY(parseTemplateInfo(tokens));
  auto& token2 = tokens.popNext();
  if (!token2.contains<IdentifierToken>())
    return token2.codeLoc.getError("Expected concept name");
  auto ret = make_unique<ConceptDefinition>(token2.codeLoc, token2.value);
  ret->templateInfo = templateInfo;
  TRY(tokens.eat(Keyword::OPEN_BLOCK));
  while (1) {
    auto memberToken = tokens.peek();
    if (memberToken == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    auto typeId = TRY(parseIdentifier(tokens, true));
    bool constructor = false;
    if (tokens.peek() == Keyword::OPEN_BRACKET) {
      tokens.rewind();
      constructor = true;
    }
    ret->functions.push_back(TRY(parseFunctionSignature(typeId, tokens)));
    if (constructor)
      ret->functions.back()->name = ConstructorTag{};
    TRY(tokens.eat(Keyword::SEMICOLON));
  }
  TRY(tokens.eat(Keyword::SEMICOLON));
  return std::move(ret);
}

WithErrorLine<unique_ptr<ReturnStatement>> parseReturnStatement(Tokens& tokens) {
  auto returnToken = TRY(tokens.eat(Keyword::RETURN));
  auto ret = make_unique<ReturnStatement>(returnToken.codeLoc);
  auto& token2 = tokens.peek();
  if (token2 == Keyword::SEMICOLON) {
    tokens.popNext();
    return std::move(ret);
  }
  ret->expr = TRY(parseExpression(tokens));
  TRY(tokens.eat(Keyword::SEMICOLON));
  return std::move(ret);
}

WithErrorLine<unique_ptr<BreakStatement>> parseBreakStatement(Tokens& tokens) {
  auto breakToken = TRY(tokens.eat(Keyword::BREAK));
  TRY(tokens.eat(Keyword::SEMICOLON));
  return make_unique<BreakStatement>(breakToken.codeLoc);
}

WithErrorLine<unique_ptr<ContinueStatement>> parseContinueStatement(Tokens& tokens) {
  auto continueToken = TRY(tokens.eat(Keyword::CONTINUE));
  TRY(tokens.eat(Keyword::SEMICOLON));
  return make_unique<ContinueStatement>(continueToken.codeLoc);
}

WithErrorLine<unique_ptr<UnionDefinition>> parseUnionDefinition(Tokens& tokens) {
  TRY(tokens.eat(Keyword::UNION));
  auto nameToken = tokens.popNext();
  if (!nameToken.contains<IdentifierToken>())
    return nameToken.codeLoc.getError("Expected union type name");
  auto ret = make_unique<UnionDefinition>(nameToken.codeLoc, nameToken.value);
  TRY(tokens.eat(Keyword::OPEN_BLOCK));
  while (1) {
    auto memberToken = tokens.peek();
    if (memberToken == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    TemplateInfo templateParams;
    if (memberToken == Keyword::TEMPLATE) {
      tokens.popNext();
      templateParams = TRY(parseTemplateInfo(tokens));
    }
    auto typeIdent = TRY(parseIdentifier(tokens, true));
    auto& token2 = tokens.popNext();
    if (!token2.contains<IdentifierToken>())
      return token2.codeLoc.getError("Expected name of a union member");
    ret->elements.push_back(UnionDefinition::Element{typeIdent, token2.value, token2.codeLoc});
    TRY(tokens.eat(Keyword::SEMICOLON));
  }
  TRY(tokens.eat(Keyword::SEMICOLON));
  return std::move(ret);
}

WithErrorLine<unique_ptr<Statement>> parseForLoopStatement(Tokens& tokens) {
  auto codeLoc = tokens.peek().codeLoc;
  TRY(tokens.eat(Keyword::FOR));
  TRY(tokens.eat(Keyword::OPEN_BRACKET));
  auto normalForBookmark = tokens.getBookmark();
  if (tokens.peekNext() == Keyword::COLON) {
    auto id = tokens.popNext();
    CHECK(!!tokens.eat(Keyword::COLON));
    auto container = TRY(parseExpression(tokens));
    TRY(tokens.eat(Keyword::CLOSE_BRACKET));
    auto body = TRY(parseNonTopLevelStatement(tokens));
    return cast<Statement>(getRangedLoop(codeLoc, id.value,
        std::move(container),
        std::move(body)));
  }
  tokens.rewind(normalForBookmark);
  auto init = TRY(parseVariableDeclaration(tokens, true, true));
  auto cond = TRY(parseExpression(tokens));
  TRY(tokens.eat(Keyword::SEMICOLON));
  auto iter = TRY(parseExpression(tokens));
  TRY(tokens.eat(Keyword::CLOSE_BRACKET));
  auto body = TRY(parseNonTopLevelStatement(tokens));
  return getForLoop(codeLoc, std::move(init), std::move(cond), std::move(iter), std::move(body));
}

WithErrorLine<unique_ptr<WhileLoopStatement>> parseWhileLoopStatement(Tokens& tokens) {
  auto codeLoc = tokens.peek().codeLoc;
  TRY(tokens.eat(Keyword::WHILE));
  TRY(tokens.eat(Keyword::OPEN_BRACKET));
  auto cond = TRY(parseExpression(tokens));
  TRY(tokens.eat(Keyword::CLOSE_BRACKET));
  auto body = TRY(parseNonTopLevelStatement(tokens));
  return make_unique<WhileLoopStatement>(codeLoc, std::move(cond), std::move(body), nullptr);
}

WithErrorLine<unique_ptr<SwitchStatement>> parseSwitchStatement(Tokens& tokens) {
  auto switchToken = TRY(tokens.eat(Keyword::SWITCH));
  TRY(tokens.eat(Keyword::OPEN_BRACKET));
  auto expr = TRY(parseExpression(tokens));
  TRY(tokens.eat(Keyword::CLOSE_BRACKET));
  TRY(tokens.eat(Keyword::OPEN_BLOCK));
  auto ret = make_unique<SwitchStatement>(switchToken.codeLoc, std::move(expr));
  while (1) {
    auto& token = tokens.peek();
    if (token == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    if (token == Keyword::DEFAULT) {
      if (ret->defaultBlock)
        return token.codeLoc.getError("Default switch statement is repeated");
      tokens.popNext();
      ret->defaultBlock = TRY(parseBlock(tokens));
    } else {
      TRY(tokens.eat(Keyword::CASE));
      TRY(tokens.eat(Keyword::OPEN_BRACKET));
      SwitchStatement::CaseElem caseElem;
      caseElem.codeloc = token.codeLoc;
      while (1) {
        auto id = TRY(tokens.eat<IdentifierToken>("Expected union or enum element"));
        caseElem.ids.push_back(make_pair(id.value, id.codeLoc));
        if (tokens.eatMaybe(Keyword::CLOSE_BRACKET))
          break;
        else
          TRY(tokens.eat(Keyword::COMMA));
      }
      caseElem.block = TRY(parseBlock(tokens));
      ret->caseElems.push_back(std::move(caseElem));
    }
  }
  return std::move(ret);
}

WithErrorLine<unique_ptr<VariableDeclaration>> parseVariableDeclaration(Tokens& tokens, bool eatSemicolon,
    bool forLoop) {
  auto typeLoc = tokens.peek().codeLoc;
  optional<IdentifierInfo> type;
  bool isMutable = false;
  bool isDeclaration = forLoop;
  if (!forLoop) {
    if (tokens.eatMaybe(Keyword::MUTABLE)) {
      isMutable = true;
      isDeclaration = true;
    } else
    if (tokens.eatMaybe(Keyword::CONST))
      isDeclaration = true;
  }
  auto id1 = TRY(parseIdentifier(tokens, true));
  string variableName;
  if (tokens.peek() == Operator::ASSIGNMENT && isDeclaration) {
    if (auto name = id1.asBasicIdentifier())
      variableName = *name;
    else
      return id1.codeLoc.getError("Expected variable name");
  } else {
    type = id1;
    auto id2 = tokens.popNext();
    if (id2.contains<IdentifierToken>())
      variableName = id2.value;
    else if (!isDeclaration)
      return unique_ptr<VariableDeclaration>();
    else
      return id2.codeLoc.getError("Expected variable name, got " + quote(id2.value));

  }
  unique_ptr<Expression> initExpression;
  if (tokens.eatMaybe(Operator::ASSIGNMENT))
    initExpression = TRY(parseExpression(tokens));
  if (eatSemicolon)
    TRY(tokens.eat(Keyword::SEMICOLON));
  auto ret = make_unique<VariableDeclaration>(typeLoc, type, variableName, std::move(initExpression));
  ret->isMutable = isMutable;
  return std::move(ret);
}

WithErrorLine<unique_ptr<Statement>> parseIfStatement(Tokens& tokens) {
  auto ifToken = TRY(tokens.eat(Keyword::IF));
  TRY(tokens.eat(Keyword::OPEN_BRACKET));
  unique_ptr<VariableDeclaration> decl;
  unique_ptr<Expression> cond;
  if (tokens.peek() == Keyword::CONST || tokens.peek() == Keyword::MUTABLE) {
    decl = TRY(parseVariableDeclaration(tokens, false));
    if (tokens.peek() == Keyword::SEMICOLON) {
      tokens.popNext();
      cond = TRY(parseExpression(tokens));
    }
  } else
    cond = TRY(parseExpression(tokens));
  if (!decl && !cond)
    return ifToken.codeLoc.getError("Expected condition expression or declaration");
  TRY(tokens.eat(Keyword::CLOSE_BRACKET));
  auto ifTrue = TRY(parseNonTopLevelStatement(tokens));
  unique_ptr<Statement> ifFalse;
  if (!tokens.empty()) {
    auto& token2 = tokens.peek();
    if (auto k1 = token2.getReferenceMaybe<Keyword>())
      if (*k1 == Keyword::ELSE) {
        tokens.popNext();
        ifFalse = TRY(parseNonTopLevelStatement(tokens));
      }
  }
  return cast<Statement>(
      make_unique<IfStatement>(ifToken.codeLoc, std::move(decl), std::move(cond), std::move(ifTrue), std::move(ifFalse)));
}

WithErrorLine<unique_ptr<Statement>> parseTypeAlias(Tokens& tokens) {
  auto loc = TRY(tokens.eat(Keyword::USING)).codeLoc;
  auto id = TRY(tokens.eat<IdentifierToken>("Expected name of type alias")).value;
  TRY(tokens.eat(Operator::ASSIGNMENT));
  auto type = TRY(parseIdentifier(tokens, true));
  TRY(tokens.eat(Keyword::SEMICOLON));
  return cast<Statement>(make_unique<TypeAliasDeclaration>(loc, id, type));
}

WithErrorLine<unique_ptr<Statement>> parseTemplateDefinition(Tokens& tokens) {
  TRY(tokens.eat(Keyword::TEMPLATE));
  auto templateInfo = TRY(parseTemplateInfo(tokens));
  auto nextToken = tokens.peek();
  auto checkNameConflict = [&templateInfo] (const string& name, const string& type) -> JustError<ErrorLoc> {
    for (auto& param : templateInfo.params)
      if (param.name == name)
        return param.codeLoc.getError("Template parameter conflicts with " + type + " name");
    return success;
  };
  if (nextToken == Keyword::EXTERN) {
    tokens.popNext();
    if (tokens.peek() == Keyword::STRUCT) {
      auto ret = TRY(parseStructDefinition(tokens, true));
      TRY(checkNameConflict(ret.get()->name, "struct"));
      ret.get()->templateInfo = templateInfo;
      return cast<Statement>(std::move(ret));
    } else {
      unique_ptr<FunctionDefinition> ret;
      ret = TRY(parseFunctionSignature(TRY(parseIdentifier(tokens, true)), tokens));
      TRY(tokens.eat(Keyword::SEMICOLON));
      if (auto name = ret->name.getReferenceMaybe<string>())
        TRY(checkNameConflict(*name, "function"));
      ret->templateInfo = templateInfo;
      ret->external = true;
      if (ret->name.contains<ConstructorTag>())
        for (auto& elem : templateInfo.params)
          ret->returnType.parts[0].templateArguments.push_back(IdentifierInfo(elem.name, elem.codeLoc));
      return cast<Statement>(std::move(ret));
    }
  } else {
    auto addTemplate = [&] (auto structure, const char* name) -> WithErrorLine<unique_ptr<Statement>> {
      if (!structure)
        return structure.get_error();
      TRY(checkNameConflict(structure.get()->name, name));
      structure.get()->templateInfo = templateInfo;
      return cast<Statement>(std::move(*structure));
    };
    if (nextToken == Keyword::STRUCT)
      return addTemplate(parseStructDefinition(tokens, false), "struct");
    if (nextToken == Keyword::UNION)
      return addTemplate(parseUnionDefinition(tokens), "union");
    else {
      unique_ptr<FunctionDefinition> ret;
      ret = TRY(parseFunctionDefinition(TRY(parseIdentifier(tokens, true)), tokens));
      if (auto name = ret->name.getReferenceMaybe<string>())
        TRY(checkNameConflict(*name, "function"));
      ret->templateInfo = templateInfo;
      if (ret->name.contains<ConstructorTag>()) {
        for (auto& elem : templateInfo.params)
          ret->returnType.parts[0].templateArguments.push_back(IdentifierInfo(elem.name, elem.codeLoc));
      }
      return cast<Statement>(std::move(ret));
    }
  }
}

WithErrorLine<unique_ptr<Statement>> parseImportStatement(Tokens& tokens) {
  auto codeLoc = tokens.peek().codeLoc;
  TRY(tokens.eat(Keyword::IMPORT));
  auto path = TRY(tokens.eat(StringToken{}));
  auto endLoc =  TRY(tokens.eat(Keyword::SEMICOLON)).codeLoc;
  return cast<Statement>(make_unique<ImportStatement>(codeLoc, endLoc, path.value, false));
}

WithErrorLine<unique_ptr<Statement>> parseEnumStatement(Tokens& tokens, bool external) {
  TRY(tokens.eat(Keyword::ENUM));
  auto name = tokens.popNext();
  if (!name.contains<IdentifierToken>())
    return name.codeLoc.getError("Expected enum name, got: " + quote(name.value));
  auto ret = make_unique<EnumDefinition>(tokens.peek().codeLoc, name.value);
  ret->external = external;
  if (tokens.eatMaybe(Keyword::OPEN_BLOCK))
    while (1) {
      auto element = tokens.popNext();
      if (!element.contains<IdentifierToken>())
        return element.codeLoc.getError("Expected enum element, got: " + quote(element.value));
      ret->elements.push_back(make_pair(element.value, element.codeLoc));
      if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
        break;
      TRY(tokens.eat(Keyword::COMMA));
      if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
        break;
    }
  else
    return tokens.peek().codeLoc.getError("Expected enum definition");
  TRY(tokens.eat(Keyword::SEMICOLON));
  return cast<Statement>(std::move(ret));
}

template <typename T, typename U>
static WithErrorLine<unique_ptr<T>> cast(WithErrorLine<unique_ptr<U>> elem) {
  if (!elem)
    return elem.get_error();
  return cast<T>(std::move(*elem));
}

WithErrorLine<unique_ptr<ExternConstantDeclaration>> parseExternalConstant(Tokens& tokens) {
  CHECK(!!tokens.eat(Keyword::CONST));
  auto id1 = TRY(parseIdentifier(tokens, true));
  auto name = TRY(tokens.eat<IdentifierToken>("Expected identifier"));
  TRY(tokens.eat(Keyword::SEMICOLON));
  return make_unique<ExternConstantDeclaration>(name.codeLoc, id1, name.value);
}

WithErrorLine<unique_ptr<Statement>> parseStatement(Tokens& tokens, bool topLevel);

WithErrorLine<unique_ptr<Statement>> parseStatementWithAttributes(Tokens& tokens) {
  TRY(tokens.eat(Keyword::OPEN_SQUARE_BRACKET));
  vector<AttributeInfo> attrs;
  while (1) {
    auto element = tokens.popNext();
    if (!element.contains<IdentifierToken>())
      return element.codeLoc.getError("Expected attribute name, got: " + quote(element.value));
    attrs.push_back(AttributeInfo{element.value, element.codeLoc});
    if (tokens.eatMaybe(Keyword::CLOSE_SQUARE_BRACKET))
      break;
    TRY(tokens.eat(Keyword::COMMA));
    if (tokens.eatMaybe(Keyword::CLOSE_SQUARE_BRACKET))
      break;
  }
  auto stmt = TRY(parseStatement(tokens, true));
  if (!stmt->canHaveAttributes())
    return stmt->codeLoc.getError("This type of definition doesn't support attributes");
  stmt->attributes = std::move(attrs);
  return std::move(stmt);
}

WithErrorLine<unique_ptr<Statement>> parseStatement(Tokens& tokens, bool topLevel) {
  auto parseExpressionAndSemicolon = [&] () -> WithErrorLine<unique_ptr<ExpressionStatement>> {
    auto ret = TRY(parseExpression(tokens));
    TRY(tokens.eat(Keyword::SEMICOLON));
    return make_unique<ExpressionStatement>(std::move(ret));
  };
  auto handleType = [&] () -> WithErrorLine<unique_ptr<Statement>> {
    if (topLevel) {
      return cast<Statement>(parseFunctionDefinition(TRY(parseIdentifier(tokens, true)), tokens));
    } else {
      auto bookmark = tokens.getBookmark();
      auto decl = TRY(parseVariableDeclaration(tokens));
      if (decl) {
        return cast<Statement>(std::move(decl));
      } else {
        tokens.rewind(bookmark);
        return cast<Statement>(parseExpressionAndSemicolon());
      }
    }
  };
  auto& token = tokens.peek();
  return token.visit(
      [](EofToken) -> WithErrorLine<unique_ptr<Statement>> {
        return unique_ptr<Statement>();
      },
      [&](const Keyword& k) -> WithErrorLine<unique_ptr<Statement>> {
        switch (k) {
          case Keyword::USING:
            return parseTypeAlias(tokens);
          case Keyword::TEMPLATE:
            return parseTemplateDefinition(tokens);
          case Keyword::IF:
            return parseIfStatement(tokens);
          case Keyword::OPEN_BLOCK:
            return cast<Statement>(parseBlock(tokens));
          case Keyword::RETURN:
            return cast<Statement>(parseReturnStatement(tokens));
          case Keyword::BREAK:
            return cast<Statement>(parseBreakStatement(tokens));
          case Keyword::CONTINUE:
            return cast<Statement>(parseContinueStatement(tokens));
          case Keyword::EXTERN:
            tokens.popNext();
            if (tokens.peek() == Keyword::STRUCT)
              return cast<Statement>(parseStructDefinition(tokens, true));
            if (tokens.peek() == Keyword::ENUM)
              return cast<Statement>(parseEnumStatement(tokens, true));
            if (tokens.peek() == Keyword::CONST)
              return cast<Statement>(parseExternalConstant(tokens));
            else {
              auto ret = TRY(parseFunctionSignature(TRY(parseIdentifier(tokens, true)), tokens));
              ret.get()->external = true;
              TRY(tokens.eat(Keyword::SEMICOLON));
              return cast<Statement>(std::move(ret));
            }
          case Keyword::STRUCT:
            return cast<Statement>(parseStructDefinition(tokens, false));
          case Keyword::UNION:
            return cast<Statement>(parseUnionDefinition(tokens));
          case Keyword::CONCEPT:
            return cast<Statement>(parseConceptDefinition(tokens));
          case Keyword::ATTRIBUTE: {
            tokens.popNext();
            auto name = TRY(tokens.eat<IdentifierToken>("Expected attribute name"));
            TRY(tokens.eat(Keyword::SEMICOLON));
            return cast<Statement>(make_unique<AttributeDefinition>(name.codeLoc, name.value));
          }
          case Keyword::SWITCH:
            return cast<Statement>(parseSwitchStatement(tokens));
          case Keyword::ENUM:
            return parseEnumStatement(tokens, false);
          case Keyword::FOR:
            return parseForLoopStatement(tokens);
          case Keyword::WHILE:
            return cast<Statement>(parseWhileLoopStatement(tokens));
          case Keyword::IMPORT:
            return parseImportStatement(tokens);
          case Keyword::OPEN_BRACKET:
            return cast<Statement>(parseExpressionAndSemicolon());
          case Keyword::MUTABLE:
            return cast<Statement>(parseVariableDeclaration(tokens));
          case Keyword::CONST:
            return cast<Statement>(parseVariableDeclaration(tokens));
          case Keyword::MOVE:
            return cast<Statement>(parseExpressionAndSemicolon());
          case Keyword::DISCARD: {
            tokens.popNext();
            auto ret = TRY(parseExpressionAndSemicolon());
            ret.get()->canDiscard = true;
            return cast<Statement>(std::move(ret));
          }
          case Keyword::STATIC: {
            tokens.popNext();
            auto ret = cast<Statement>(make_unique<StaticStatement>(tokens.peek().codeLoc,
                TRY(parseStatement(tokens, false))));
            return std::move(ret);
          }
          case Keyword::UNCHECKED:
            tokens.popNext();
            return cast<Statement>(make_unique<UncheckedStatement>(tokens.peek().codeLoc,
                TRY(parseStatement(tokens, false))));
          case Keyword::MIXIN: {
            tokens.popNext();
            auto ret = cast<Statement>(make_unique<MixinStatement>(tokens.peek().codeLoc,
                TRY(parseExpression(tokens))));
            TRY(tokens.eat(Keyword::SEMICOLON));
            return std::move(ret);
          }
          case Keyword::OPEN_SQUARE_BRACKET:
            if (tokens.peekNext().value[0] == '@')
              return parseStatementWithAttributes(tokens);
            else
              return cast<Statement>(parseExpressionAndSemicolon());
          case Keyword::DOLLAR:
            return handleType();
          default:
            return cast<Statement>(parseExpressionAndSemicolon());
        }
      },
      [&](const IdentifierToken&) -> WithErrorLine<unique_ptr<Statement>> {
        return handleType();
      },
      [&](EmbedToken) -> WithErrorLine<unique_ptr<Statement>> {
        auto text = token.value;
        auto ret = make_unique<EmbedStatement>(token.codeLoc, text);
        ret->isTopLevel = topLevel;
        tokens.popNext();
        return cast<Statement>(std::move(ret));
      },
      [&](const auto&) -> WithErrorLine<unique_ptr<Statement>> {
        return cast<Statement>(parseExpressionAndSemicolon());
      }
  );
}

WithErrorLine<unique_ptr<Statement>> parseNonTopLevelStatement(Tokens& tokens) {
  auto ret = TRY(parseStatement(tokens, false));
  if (!ret)
    return tokens.peek().codeLoc.getError("Unexpected end of file");
  if (ret->allowTopLevel() == Statement::TopLevelAllowance::MUST)
    return ret.get()->codeLoc.getError("Statement only allowed in the top level of the program");
  return std::move(ret);
}

WithErrorLine<unique_ptr<Statement>> parseTopLevelStatement(Tokens& tokens) {
  bool isExported = !!tokens.eatMaybe(Keyword::EXPORT);
  auto statement = TRY(parseStatement(tokens, true));
  if (statement) {
    if (statement->allowTopLevel() == Statement::TopLevelAllowance::CANT)
      return statement->codeLoc.getError(
          "Statement not allowed in the top level of the program");
    statement->exported = isExported;
  }
  return std::move(statement);
}

WithErrorLine<AST> parse(Tokens tokens) {
  AST ret;
  auto loc = CodeLoc(tokens.peek().codeLoc.file, 0, 0);
  ret.elems.push_back(make_unique<ImportStatement>(loc, loc, "std/builtin.znn", true));
  ret.elems.front()->exported = true;
  while (1) {
    auto s = TRY(parseTopLevelStatement(tokens));
    if (s == nullptr)
      break;
    ret.elems.push_back(std::move(s));
  }
  return std::move(ret);
}
