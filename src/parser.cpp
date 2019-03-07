#include <vector>
#include "parser.h"
#include "token.h"
#include "ast.h"
#include "lexer.h"

WithErrorLine<unique_ptr<Expression>> parseExpression(Tokens&, int minPrecedence = 0);
WithErrorLine<unique_ptr<Expression>> parsePrimary(Tokens&);

template <typename T>
static shared_ptr<T> getSharedPtr(unique_ptr<T> p) {
  return shared_ptr<T>(p.release());
}

static WithErrorLine<IdentifierInfo> parseIdentifier(Tokens& tokens, bool allowPointer) {
  IdentifierInfo ret;
  while (1) {
    if (!tokens.peek().contains<IdentifierToken>())
      return tokens.peek().codeLoc.getError("Expected identifier");
    ret.parts.emplace_back();
    auto token = tokens.popNext();
    ret.codeLoc = token.codeLoc;
    ret.parts.back().name = token.value;
    auto beforeLessThan = tokens.getBookmark();
    if (tokens.eatMaybe(Operator::LESS_THAN)) {
      while (1) {
        if (auto ident = parseIdentifier(tokens, true)) {
          if (tokens.peek() == Keyword::COMMA || tokens.peek() == Operator::MORE_THAN)
            ret.parts.back().templateArguments.push_back(*ident);
          else {
            tokens.rewind(beforeLessThan);
            ret.parts.back().templateArguments.clear();
            break;
          }
        } else {
          if (auto expr = parsePrimary(tokens)) {
            if (tokens.peek() == Keyword::COMMA || tokens.peek() == Operator::MORE_THAN)
              ret.parts.back().templateArguments.push_back(getSharedPtr(std::move(*expr)));
            else {
              ret.parts.back().templateArguments.clear();
              tokens.rewind(beforeLessThan);
              break;
            }
          } else
            return expr.get_error();
        }
        if (tokens.eatMaybe(Operator::MORE_THAN))
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
        if (auto t = tokens.eat(Operator::MULTIPLY); !t)
          return t.get_error();
        ret.typeOperator.push_back(IdentifierInfo::MUTABLE);
      }
      else if (auto t = tokens.eatMaybe(Operator::MULTIPLY)) {
        ret.typeOperator.push_back(IdentifierInfo::CONST);
      }
      else if (auto t = tokens.eatMaybe(Keyword::OPEN_SQUARE_BRACKET)) {
        if (tokens.peek() != Keyword::CLOSE_SQUARE_BRACKET) {
          if (auto expr = parseExpression(tokens))
            ret.typeOperator.push_back(IdentifierInfo::ArraySize{getSharedPtr(std::move(*expr))});
          else
            return expr.get_error();
        } else {
          ret.typeOperator.push_back(IdentifierInfo::Slice{});
        }
        if (auto t = tokens.eat(Keyword::CLOSE_SQUARE_BRACKET); !t)
          return t.get_error();
      } else
        break;
    }
  }
  //INFO << "Identifier " << ret.toString();
  return ret;
}

WithErrorLine<unique_ptr<Expression>> parseFunctionCall(IdentifierInfo id, Tokens& tokens) {
  auto ret = unique<FunctionCall>(tokens.peek().codeLoc, id);
  if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
    return t.get_error();
  while (tokens.peek() != Keyword::CLOSE_BRACKET) {
    optional<string> argName;
    if (tokens.eatMaybe(Operator::MEMBER_ACCESS)) {
      auto idToken = tokens.popNext();
      if (!idToken.contains<IdentifierToken>())
        return idToken.codeLoc.getError("Expected function parameter name");
      argName = idToken.value;
      if (auto t = tokens.eat(Operator::ASSIGNMENT); !t)
        return t.get_error();
    }
    if (auto expr = parseExpression(tokens))
      ret->arguments.push_back(std::move(*expr));
    else
      return expr.get_error();
    ret->argNames.push_back(argName);
    if (tokens.peek() == Keyword::COMMA)
      tokens.popNext();
    else
      break;
  }
  if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
    return t.get_error();
  return cast<Expression>(std::move(ret));
}

static string getInputReg() {
  return "((?:(?:\\\\\\{)|[^\\{])+)|(\\{.*?\\})|(\\{.*)";
  /*string inputReg;
  std::cin >> inputReg;
  return inputReg;*/
}

WithErrorLine<unique_ptr<Expression>> parseStringLiteral(CodeLoc initialLoc, string literal) {
  if (literal.empty())
    return cast<Expression>(unique<Constant>(initialLoc, CompileTimeValue::get(""s)));
  unique_ptr<Expression> left;
  auto addElem = [&left] (unique_ptr<Expression> right, CodeLoc loc) {
    if (!left)
      left = std::move(right);
    else
      left = BinaryExpression::get(loc, Operator::PLUS, std::move(left), std::move(right));
  };
  regex re(getInputReg());
  int lastPos = 0;
  auto words_begin = sregex_iterator(literal.begin(), literal.end(), re);
  auto words_end = sregex_iterator();
  for (auto it = words_begin; it != words_end; ++it)
    for (int index = 0 ; index < it->size(); ++index)
      if (!it->str(index + 1).empty()) { // determine which submatch was matched
        CodeLoc loc = initialLoc.plus(0, (int) it->position());
        //cout << "Matched \"" << it->str() << "\" with rule " << index << " at " << loc.line << ":" << loc.column << endl;
        if (index == 0) {
          addElem(unique<Constant>(loc, CompileTimeValue::get(
              std::regex_replace(it->str(), std::regex("\\\\([\\{\\}])"), "$1"))), loc);
        } else if (index == 1) {
          loc = loc.plus(0, 2);
          auto tokens = lex(it->str().substr(1, it->str().size() - 2), loc, "end of expression").get();
          if (auto expr = parseExpression(tokens)) {
            auto call = BinaryExpression::get(loc, Operator::MEMBER_ACCESS, std::move(*expr),
                unique<FunctionCall>(loc, IdentifierInfo("to_string", loc)));
            addElem(std::move(call), loc);
          } else
            return expr.get_error();
        } else {
          return loc.getError("Unmatched " + quote("{"));
        }
      }
  return std::move(left);
}

static WithErrorLine<unique_ptr<Expression>> parseArrayLiteral(Tokens& tokens) {
  auto ret = unique<ArrayLiteral>(tokens.peek().codeLoc);
  if (auto t = tokens.eat(Keyword::OPEN_BLOCK); !t)
    return t.get_error();
  while (1) {
    if (auto expr = parseExpression(tokens))
      ret->contents.push_back(std::move(*expr));
    else
      return expr.get_error();
    if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
      break;
    else
      if (auto t = tokens.eat(Keyword::COMMA); !t)
        return t.get_error();
  }
  return cast<Expression>(std::move(ret));
}

WithErrorLine<unique_ptr<Expression>> parsePrimary(Tokens& tokens) {
  auto token = tokens.peek();
  return token.visit(
      [&](const Keyword& k) -> WithErrorLine<unique_ptr<Expression>> {
        switch (k) {
          case Keyword::OPEN_BRACKET: {
            tokens.popNext();
            auto ret = parseExpression(tokens);
            if (!ret)
              return ret.get_error();
            if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
              return t.get_error();
            return std::move(*ret);
          }
          case Keyword::MOVE: {
            tokens.popNext();
            if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
              return t.get_error();
            auto var = tokens.popNext();
            if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
              return t.get_error();
            if (!var.contains<IdentifierToken>())
              return var.codeLoc.getError("Expected variable identifier");
            return cast<Expression>(unique<MoveExpression>(token.codeLoc, var.value));
          } 
          case Keyword::FALSE:
            tokens.popNext();
            return cast<Expression>(unique<Constant>(token.codeLoc, CompileTimeValue::get(false)));
          case Keyword::TRUE:
            tokens.popNext();
            return cast<Expression>(unique<Constant>(token.codeLoc, CompileTimeValue::get(true)));
          case Keyword::OPEN_BLOCK:
            return parseArrayLiteral(tokens);
          default:
            return token.codeLoc.getError("Expected primary expression, got: " + quote(token.value));
        }
      },
      [&](const IdentifierToken&) -> WithErrorLine<unique_ptr<Expression>> {
        auto identifier = parseIdentifier(tokens, false);
        if (!identifier)
          return identifier.get_error();
        if (tokens.peek() == Keyword::OPEN_BRACKET) {
          return parseFunctionCall(*identifier, tokens);
        } else {
          if (identifier->parts.size() == 1)
            return cast<Expression>(unique<Variable>(token.codeLoc, *identifier));
          else
            return cast<Expression>(unique<EnumConstant>(token.codeLoc, identifier->parts[0].name,
                identifier->parts[1].name));
        }
      },
      [&](const Number&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        return cast<Expression>(unique<Constant>(token.codeLoc, CompileTimeValue::get(stoi(token.value))));
      },
      [&](const RealNumber&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        return cast<Expression>(unique<Constant>(token.codeLoc, CompileTimeValue::get(stod(token.value))));
      },
      [&](const StringToken&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        return parseStringLiteral(token.codeLoc, token.value);
      },
      [&](const CharToken&) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        return cast<Expression>(unique<Constant>(token.codeLoc, CompileTimeValue::get(token.value[0])));
      },
      [&](const Operator& op) -> WithErrorLine<unique_ptr<Expression>> {
        tokens.popNext();
        if (auto opUnary = getUnary(op)) {
          if (auto expr = parseExpression(tokens, getPrecedence(*opUnary) + 1))
            return cast<Expression>(unique<UnaryExpression>(token.codeLoc, *opUnary, std::move(*expr)));
          else
            return expr.get_error();
        } else
          return token.codeLoc.getError(getString(op) + " is not a unary operator"s);
      },
      [&](const auto&) -> WithErrorLine<unique_ptr<Expression>> {
        return token.codeLoc.getError("Expected primary expression, got: " + quote(token.value));
      }
  );
}

WithErrorLine<unique_ptr<Expression>> parseExpressionImpl(Tokens& tokens, unique_ptr<Expression> lhs,
    int minPrecedence) {
  while (1) {
    if (tokens.empty())
      break;
    auto token = tokens.peek();
    if (auto op1 = token.getValueMaybe<Operator>()) {
      if (getPrecedence(*op1) < minPrecedence)
        break;
      tokens.popNext();
      auto rhs = parsePrimary(tokens);
      if (!rhs)
        return rhs.get_error();
      token = tokens.peek();
      while (1) {
        auto op2 = token.getValueMaybe<Operator>();
        if (token == Keyword::OPEN_SQUARE_BRACKET)
          op2 = Operator::SUBSCRIPT;
        if (op2) {
          INFO << "Comparing operators op1 " << getString(*op1) << " and op2 " << getString(*op2);
          if (getPrecedence(*op2) <= getPrecedence(*op1) &&
              (!isRightAssociative(*op2) || getPrecedence(*op2) < getPrecedence(*op1)))
            break;
          rhs = parseExpressionImpl(tokens, std::move(*rhs), getPrecedence(*op2));
          if (!rhs)
            return rhs.get_error();
          token = tokens.peek();
        } else
          break;
      }
      lhs = BinaryExpression::get(token.codeLoc, *op1, std::move(lhs), std::move(*rhs));
    } else
    if (token == Keyword::OPEN_SQUARE_BRACKET) {
      tokens.popNext();
      auto rhs = parseExpression(tokens);
      if (!rhs)
        return rhs.get_error();
      if (auto t = tokens.eat(Keyword::CLOSE_SQUARE_BRACKET); !t)
        return t.get_error();
      lhs = BinaryExpression::get(token.codeLoc, Operator::SUBSCRIPT, std::move(lhs), std::move(*rhs));
    } else
      break;
  }
  return std::move(lhs);
}

WithErrorLine<unique_ptr<Expression>> parseExpression(Tokens& tokens, int minPrecedence) {
  if (auto expr = parsePrimary(tokens))
    return parseExpressionImpl(tokens, std::move(*expr), minPrecedence);
  else
    return expr.get_error();
}

WithErrorLine<unique_ptr<Statement>> parseNonTopLevelStatement(Tokens&);

WithErrorLine<unique_ptr<StatementBlock>> parseBlock(Tokens& tokens) {
  auto openBlock = tokens.eat(Keyword::OPEN_BLOCK);
  if (!openBlock)
    return openBlock.get_error();
  auto block = unique<StatementBlock>(openBlock->codeLoc);
  while (1) {
    auto token2 = tokens.peek();
    if (token2 == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    if (auto s = parseNonTopLevelStatement(tokens))
      block->elems.push_back(std::move(*s));
    else
      return s.get_error();
  }
  return std::move(block);
}

WithErrorLine<unique_ptr<FunctionDefinition>> parseFunctionSignature(IdentifierInfo type, Tokens& tokens) {
  auto token2 = tokens.popNext();
  unique_ptr<FunctionDefinition> ret;
  if (token2 == Keyword::OPERATOR) {
    if (auto op = tokens.peek().getValueMaybe<Operator>()) {
      tokens.popNext();
      ret = unique<FunctionDefinition>(type.codeLoc, type, *op);
    } else {
      if (auto t = tokens.eat(Keyword::OPEN_SQUARE_BRACKET); !t)
        return t.get_error();
      if (auto t = tokens.eat(Keyword::CLOSE_SQUARE_BRACKET); !t)
        return t.get_error();
      ret = unique<FunctionDefinition>(type.codeLoc, type, Operator::SUBSCRIPT);
    }
  } else if (token2 == Keyword::OPEN_BRACKET) {
    if (auto typeName = type.asBasicIdentifier()) {
      ret = unique<FunctionDefinition>(type.codeLoc, type, ConstructorId { *typeName });
      tokens.rewind();
    } else
      return type.codeLoc.getError("Expected type name without template parameters");
  } else {
    if (!token2.contains<IdentifierToken>())
      return token2.codeLoc.getError("Expected identifier, got: " + quote(token2.value));
    ret = unique<FunctionDefinition>(type.codeLoc, type, token2.value);
  }
  if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
    return t.get_error();
  while (1) {
    if (tokens.eatMaybe(Keyword::CLOSE_BRACKET))
      break;
    if (!ret->parameters.empty())
      if (auto t = tokens.eat(Keyword::COMMA); !t)
        return t.get_error();
    bool isParamMutable = !!tokens.eatMaybe(Keyword::MUTABLE);
    bool isParamVirtual = !!tokens.eatMaybe(Keyword::VIRTUAL);
    isParamMutable |= !!tokens.eatMaybe(Keyword::MUTABLE);
    if (isParamMutable && isParamVirtual)
      return tokens.peek().codeLoc.getError("Parameter can't be both mutable and virtual");
    ret->isVirtual |= isParamVirtual;
    auto typeId = parseIdentifier(tokens, true);
    if (!typeId)
      return typeId.get_error();
    auto paramCodeLoc = typeId->codeLoc;
    optional<string> paramName;
    auto nameToken = tokens.peek();
    if (nameToken.contains<IdentifierToken>()) {
      paramName = nameToken.value;
      paramCodeLoc = nameToken.codeLoc;
      tokens.popNext();
    }
    ret->parameters.push_back({paramCodeLoc, *typeId, paramName, isParamMutable, isParamVirtual});
  }
  if (ret->parameters.size() == 1)
    if (auto op = ret->name.getValueMaybe<Operator>())
      if (auto opUnary = getUnary(*op))
        ret->name = *opUnary;
  return std::move(ret);
}

WithErrorLine<unique_ptr<FunctionDefinition>> parseFunctionDefinition(IdentifierInfo type, Tokens& tokens) {
  auto ret = parseFunctionSignature(type, tokens);
  if (!ret)
    return ret.get_error();
  if (tokens.eatMaybe(Operator::ASSIGNMENT)) {
    if (auto t = tokens.eat(Keyword::DEFAULT); !t)
      return t.get_error();
    ret.get()->isDefault = true;
    if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
      return t.get_error();
  } else
  if (!ret.get()->isVirtual || !tokens.eatMaybe(Keyword::SEMICOLON)) {
    if (auto block = parseBlock(tokens))
      ret.get()->body = std::move(*block);
    else
      return block.get_error();
  }
  return ret;
}

static WithErrorLine<TemplateInfo> parseTemplateInfo(Tokens& tokens) {
  if (auto t = tokens.eat(Keyword::TEMPLATE); !t)
    return t.get_error();
  if (auto t = tokens.eat(Operator::LESS_THAN); !t)
    return t.get_error();
  TemplateInfo ret;
  while (tokens.peek() != Operator::MORE_THAN) {
    auto paramToken = tokens.popNext();
    if (!paramToken.contains<IdentifierToken>())
      return paramToken.codeLoc.getError("Template parameter name expected");
    optional<string> typeName;
    if (tokens.peek() != Operator::MORE_THAN) {
      if (tokens.peek() != Keyword::COMMA) {
        typeName = paramToken.value;
        paramToken = tokens.popNext();
        if (!paramToken.contains<IdentifierToken>())
          return paramToken.codeLoc.getError("Template parameter name expected");
        if (tokens.peek() != Operator::MORE_THAN) {
          if (auto t = tokens.eat(Keyword::COMMA); !t)
            return t.get_error();
        }
      } else
        tokens.popNext();
    }
    ret.params.push_back({paramToken.value, typeName, paramToken.codeLoc});
  }
  if (auto t = tokens.eat(Operator::MORE_THAN); !t)
    return t.get_error();
  if (tokens.eatMaybe(Keyword::REQUIRES))
    while (1) {
      if (auto id = parseIdentifier(tokens, false))
        ret.requirements.push_back(*id);
      else
        return id.get_error();
      if (!tokens.eatMaybe(Keyword::COMMA))
        break;
    }
  return ret;
}

WithErrorLine<unique_ptr<StructDefinition>> parseStructDefinition(Tokens& tokens, bool external) {
  if (auto t = tokens.eat(Keyword::STRUCT); !t)
    return t.get_error();
  auto token2 = tokens.popNext();
  if (!token2.contains<IdentifierToken>())
    return token2.codeLoc.getError("Expected struct name");
  auto ret = unique<StructDefinition>(token2.codeLoc, token2.value);
  if (external)
    ret->external = true;
  else {
    if (tokens.eatMaybe(Keyword::OPEN_BLOCK))
      while (1) {
        auto memberToken = tokens.peek();
        if (memberToken == Keyword::CLOSE_BLOCK) {
          tokens.popNext();
          break;
        }
        auto typeIdent = parseIdentifier(tokens, true);
        if (!typeIdent)
          return typeIdent.get_error();
        auto memberName = tokens.popNext();
        if (!memberName.contains<IdentifierToken>())
          return memberName.codeLoc.getError("Expected identifier");
        ret->members.push_back({*typeIdent, memberName.value, memberToken.codeLoc});
        if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
          return t.get_error();
      }
    else
      ret->incomplete = true;
  }
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return std::move(ret);
}

WithErrorLine<unique_ptr<ConceptDefinition>> parseConceptDefinition(Tokens& tokens) {
  if (auto t = tokens.eat(Keyword::CONCEPT); !t)
    return t.get_error();
  auto token2 = tokens.popNext();
  if (!token2.contains<IdentifierToken>())
    return token2.codeLoc.getError("Expected struct name");
  auto ret = unique<ConceptDefinition>(token2.codeLoc, token2.value);
  if (auto t = tokens.eat(Keyword::OPEN_BLOCK); !t)
    return t.get_error();
  while (1) {
    auto memberToken = tokens.peek();
    if (memberToken == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    auto typeId = parseIdentifier(tokens, true);
    if (!typeId)
      return typeId.get_error();
    bool constructor = false;
    if (tokens.peek() == Keyword::OPEN_BRACKET) {
      tokens.rewind();
      constructor = true;
    }
    if (auto sig = parseFunctionSignature(*typeId, tokens))
      ret->functions.push_back(std::move(*sig));
    else
      return sig.get_error();
    if (constructor)
      ret->functions.back()->name = ConstructorId{*ret->functions.back()->name.getValueMaybe<string>()};
    if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
      return t.get_error();
  }
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return std::move(ret);
}

WithErrorLine<unique_ptr<ReturnStatement>> parseReturnStatement(Tokens& tokens) {
  auto returnToken = tokens.eat(Keyword::RETURN);
  if (!returnToken)
    returnToken.get_error();
  auto ret = unique<ReturnStatement>(returnToken->codeLoc);
  auto token2 = tokens.peek();
  if (auto keyword = token2.getReferenceMaybe<Keyword>())
    if (*keyword == Keyword::SEMICOLON) {
      tokens.popNext();
      return std::move(ret);
    }
  if (auto expr = parseExpression(tokens))
    ret->expr = std::move(*expr);
  else
    return expr.get_error();
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return std::move(ret);
}

WithErrorLine<unique_ptr<BreakStatement>> parseBreakStatement(Tokens& tokens) {
  auto breakToken = tokens.eat(Keyword::BREAK);
  if (!breakToken)
    breakToken.get_error();
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return unique<BreakStatement>(breakToken->codeLoc);
}

WithErrorLine<unique_ptr<ContinueStatement>> parseContinueStatement(Tokens& tokens) {
  auto continueToken = tokens.eat(Keyword::CONTINUE);
  if (!continueToken)
    continueToken.get_error();
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return unique<ContinueStatement>(continueToken->codeLoc);
}

WithErrorLine<unique_ptr<VariantDefinition>> parseVariantDefinition(Tokens& tokens) {
  if (auto t = tokens.eat(Keyword::VARIANT); !t)
    return t.get_error();
  auto nameToken = tokens.popNext();
  if (!nameToken.contains<IdentifierToken>())
    return nameToken.codeLoc.getError("Expected variant name");
  auto ret = unique<VariantDefinition>(nameToken.codeLoc, nameToken.value);
  if (auto t = tokens.eat(Keyword::OPEN_BLOCK); !t)
    return t.get_error();
  while (1) {
    auto memberToken = tokens.peek();
    if (memberToken == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    TemplateInfo templateParams;
    if (memberToken == Keyword::TEMPLATE) {
      if (auto info = parseTemplateInfo(tokens))
        templateParams = *info;
      else
        return info.get_error();
    }
    auto typeIdent = parseIdentifier(tokens, true);
    if (!typeIdent)
      return typeIdent.get_error();
    auto token2 = tokens.popNext();
    if (!token2.contains<IdentifierToken>())
      return token2.codeLoc.getError("Expected name of a variant alternative");
    ret->elements.push_back(VariantDefinition::Element{*typeIdent, token2.value, token2.codeLoc});
    if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
      return t.get_error();
  }
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return std::move(ret);
}

WithErrorLine<unique_ptr<Statement>> parseForLoopStatement(Tokens& tokens) {
  auto codeLoc = tokens.peek().codeLoc;
  if (auto t = tokens.eat(Keyword::FOR); !t)
    return t.get_error();
  if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
    return t.get_error();
  auto normalForBookmark = tokens.getBookmark();
  if (tokens.eatMaybe(Keyword::MUTABLE))
    if (tokens.peek().contains<IdentifierToken>()) {
      auto id = tokens.popNext();
      if (tokens.eatMaybe(Keyword::COLON)) {
        auto container = parseExpression(tokens);
        if (!container)
          return container.get_error();
        if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
          return t.get_error();
        auto body = parseNonTopLevelStatement(tokens);
        if (!body)
          return body.get_error();
        return cast<Statement>(unique<RangedLoopStatement>(codeLoc,
            unique<VariableDeclaration>(codeLoc, none, id.value, nullptr),
            std::move(*container),
            std::move(*body)));
      }
    }
  tokens.rewind(normalForBookmark);
  auto init = parseNonTopLevelStatement(tokens);
  if (!init)
    return init.get_error();
  auto cond = parseExpression(tokens);
  if (!cond)
    return cond.get_error();
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  auto iter = parseExpression(tokens);
  if (!iter)
    return iter.get_error();
  if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
    return t.get_error();
  auto body = parseNonTopLevelStatement(tokens);
  if (!body)
    return body.get_error();
  return cast<Statement>(
      unique<ForLoopStatement>(codeLoc, std::move(*init), std::move(*cond), std::move(*iter), std::move(*body)));
}

WithErrorLine<unique_ptr<WhileLoopStatement>> parseWhileLoopStatement(Tokens& tokens) {
  auto codeLoc = tokens.peek().codeLoc;
  if (auto t = tokens.eat(Keyword::WHILE); !t)
    return t.get_error();
  if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
    return t.get_error();
  auto cond = parseExpression(tokens);
  if (!cond)
    return cond.get_error();
  if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
    return t.get_error();
  auto body = parseNonTopLevelStatement(tokens);
  if (!body)
    return body.get_error();
  return unique<WhileLoopStatement>(codeLoc, std::move(*cond), std::move(*body));
}

WithErrorLine<unique_ptr<SwitchStatement>> parseSwitchStatement(Tokens& tokens) {
  auto switchToken = tokens.eat(Keyword::SWITCH);
  if (!switchToken)
    return switchToken.get_error();
  if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
    return t.get_error();
  auto expr = parseExpression(tokens);
  if (!expr)
    return expr.get_error();
  if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
    return t.get_error();
  if (auto t = tokens.eat(Keyword::OPEN_BLOCK); !t)
    return t.get_error();
  auto ret = unique<SwitchStatement>(switchToken->codeLoc, std::move(*expr));
  while (1) {
    auto token2 = tokens.peek();
    if (token2 == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    if (token2 == Keyword::DEFAULT) {
      if (ret->defaultBlock)
        return token2.codeLoc.getError("Default switch statement is repeated");
      tokens.popNext();
      if (auto block = parseBlock(tokens))
        ret->defaultBlock = std::move(*block);
      else
        return block.get_error();
    } else {
      if (auto t = tokens.eat(Keyword::CASE); !t)
        return t.get_error();
      if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
        return t.get_error();
      SwitchStatement::CaseElem caseElem;
      auto identifier = parseIdentifier(tokens, true);
      if (!identifier)
        return identifier.get_error();
      caseElem.codeloc = token2.codeLoc;
      token2 = tokens.popNext();
      if (token2 != Keyword::CLOSE_BRACKET) {
        if (!token2.contains<IdentifierToken>())
          return token2.codeLoc.getError("Expected variable identifier, got " + quote(token2.value));
        caseElem.id = token2.value;
        caseElem.type = *identifier;
        if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
          return t.get_error();
      } else {
        if (auto s = identifier->asBasicIdentifier())
          caseElem.id = *s;
        else
          return identifier->codeLoc.getError("Identifier " + quote(identifier->prettyString()) + " is not a variable");
      }
      if (auto block = parseBlock(tokens))
        caseElem.block = std::move(*block);
      else
        return block.get_error();
      ret->caseElems.push_back(std::move(caseElem));
    }
  }
  return std::move(ret);
}

WithErrorLine<unique_ptr<VariableDeclaration>> parseVariableDeclaration(Tokens& tokens, bool eatSemicolon = true) {
  auto typeLoc = tokens.peek().codeLoc;
  optional<IdentifierInfo> type;
  bool isMutable = false;
  bool isDeclaration = false;
  if (tokens.eatMaybe(Keyword::MUTABLE)) {
    isMutable = true;
    isDeclaration = true;
  } else if (tokens.eatMaybe(Keyword::CONST))
    isDeclaration = true;
  auto id1 = parseIdentifier(tokens, true);
  if (!id1)
    return id1.get_error();
  string variableName;
  if (tokens.peek() == Operator::ASSIGNMENT && isDeclaration) {
    if (auto name = id1->asBasicIdentifier())
      variableName = *name;
    else
      return id1->codeLoc.getError("Expected variable name");
  } else {
    type = *id1;
    auto id2 = tokens.popNext();
    if (id2.contains<IdentifierToken>())
      variableName = id2.value;
    else if (!isDeclaration)
      return unique_ptr<VariableDeclaration>();
    else
      return id2.codeLoc.getError("Expected variable name, got " + quote(id2.value));

  }
  unique_ptr<Expression> initExpression;
  if (tokens.eatMaybe(Operator::ASSIGNMENT)) {
    if (auto expr = parseExpression(tokens))
      initExpression = std::move(*expr);
    else
      return expr.get_error();
  }
  if (eatSemicolon)
    if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
      return t.get_error();
  auto ret = unique<VariableDeclaration>(typeLoc, type, variableName, std::move(initExpression));
  ret->isMutable = isMutable;
  return std::move(ret);
}

WithErrorLine<unique_ptr<Statement>> parseIfStatement(Tokens& tokens) {
  auto ifToken = tokens.eat(Keyword::IF);
  if (!ifToken)
    return ifToken.get_error();
  if (auto t = tokens.eat(Keyword::OPEN_BRACKET); !t)
    return t.get_error();
  unique_ptr<VariableDeclaration> decl;
  unique_ptr<Expression> cond;
  if (tokens.peek() == Keyword::CONST || tokens.peek() == Keyword::MUTABLE) {
    if (auto tmp = parseVariableDeclaration(tokens, false))
      decl = std::move(*tmp);
    else
      return tmp.get_error();
    if (tokens.peek() == Keyword::SEMICOLON) {
      tokens.popNext();
      if (auto expr = parseExpression(tokens))
        cond = std::move(*expr);
      else
        return expr.get_error();
    }
  } else
  if (auto expr = parseExpression(tokens))
    cond = std::move(*expr);
  else
    return expr.get_error();
  if (!decl && !cond)
    return ifToken->codeLoc.getError("Expected condition expression or declaration");
  if (auto t = tokens.eat(Keyword::CLOSE_BRACKET); !t)
    return t.get_error();
  auto ifTrue = parseNonTopLevelStatement(tokens);
  if (!ifTrue)
    return ifTrue.get_error();
  unique_ptr<Statement> ifFalse;
  if (!tokens.empty()) {
    auto token2 = tokens.peek();
    if (auto k1 = token2.getReferenceMaybe<Keyword>())
      if (*k1 == Keyword::ELSE) {
        tokens.popNext();
        if (auto s = parseNonTopLevelStatement(tokens))
          ifFalse = std::move(*s);
        else
          return s.get_error();
      }
  }
  return cast<Statement>(
      unique<IfStatement>(ifToken->codeLoc, std::move(decl), std::move(cond), std::move(*ifTrue), std::move(ifFalse)));
}

WithErrorLine<unique_ptr<Statement>> parseTemplateDefinition(Tokens& tokens) {
  auto templateInfo = parseTemplateInfo(tokens);
  if (!templateInfo)
    return templateInfo.get_error();
  auto nextToken = tokens.peek();
  auto checkNameConflict = [&templateInfo] (const string& name, const string& type) -> optional<ErrorLoc> {
    for (auto& param : templateInfo->params)
      if (param.name == name)
        return param.codeLoc.getError("Template parameter conflicts with " + type + " name");
    return none;
  };
  if (nextToken == Keyword::EXTERN) {
    tokens.popNext();
    if (tokens.peek() == Keyword::STRUCT) {
      auto ret = parseStructDefinition(tokens, true);
      if (!ret)
        return ret.get_error();
      if (auto err = checkNameConflict(ret.get()->name, "struct"))
        return *err;
      ret.get()->templateInfo = *templateInfo;
      return cast<Statement>(std::move(*ret));
    } else {
      unique_ptr<FunctionDefinition> ret;
      if (auto id = parseIdentifier(tokens, true)) {
        if (auto sig = parseFunctionSignature(*id, tokens))
          ret = std::move(*sig);
        else
          return sig.get_error();
      } else
        return id.get_error();
      if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
        return t.get_error();
      if (auto name = ret->name.getReferenceMaybe<string>())
        if (auto err = checkNameConflict(*name, "function"))
          return *err;
      ret->templateInfo = *templateInfo;
      ret->external = true;
      if (ret->name.contains<ConstructorId>())
        for (auto& elem : templateInfo->params)
          ret->returnType.parts[0].templateArguments.push_back(IdentifierInfo(elem.name, elem.codeLoc));
      return cast<Statement>(std::move(ret));
    }
  } else {
    auto addTemplate = [&] (auto structure, const char* name) -> WithErrorLine<unique_ptr<Statement>> {
      if (!structure)
        return structure.get_error();
      if (auto err = checkNameConflict(structure.get()->name, name))
        return *err;
      structure.get()->templateInfo = *templateInfo;
      return cast<Statement>(std::move(*structure));
    };
    if (nextToken == Keyword::STRUCT)
      return addTemplate(parseStructDefinition(tokens, false), "struct");
    if (nextToken == Keyword::VARIANT)
      return addTemplate(parseVariantDefinition(tokens), "variant");
    if (nextToken == Keyword::CONCEPT)
      return addTemplate(parseConceptDefinition(tokens), "concept");
    else {
      unique_ptr<FunctionDefinition> ret;
      if (auto id = parseIdentifier(tokens, true)) {
        if (auto def = parseFunctionDefinition(*id, tokens))
          ret = std::move(*def);
        else
          return def.get_error();
      } else
        return id.get_error();
      if (auto name = ret->name.getReferenceMaybe<string>())
        if (auto err = checkNameConflict(*name, "function"))
          return *err;
      ret->templateInfo = *templateInfo;
      if (ret->name.contains<ConstructorId>()) {
        for (auto& elem : templateInfo->params)
          ret->returnType.parts[0].templateArguments.push_back(IdentifierInfo(elem.name, elem.codeLoc));
      }
      return cast<Statement>(std::move(ret));
    }
  }
}

WithErrorLine<unique_ptr<Statement>> parseImportStatement(Tokens& tokens, bool isPublic) {
  auto codeLoc = tokens.peek().codeLoc;
  if (auto t = tokens.eat(Keyword::IMPORT); !t)
    return t.get_error();
  auto path = tokens.eat(StringToken{});
  if (!path)
    return path.get_error();
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return cast<Statement>(unique<ImportStatement>(codeLoc, path->value, isPublic, false));
}

WithErrorLine<unique_ptr<Statement>> parseEnumStatement(Tokens& tokens) {
  if (auto t = tokens.eat(Keyword::ENUM); !t)
    return t.get_error();
  auto name = tokens.popNext();
  if (!name.contains<IdentifierToken>())
    return name.codeLoc.getError("Expected enum name, got: " + quote(name.value));
  auto ret = unique<EnumDefinition>(tokens.peek().codeLoc, name.value);
  if (auto t = tokens.eat(Keyword::OPEN_BLOCK); !t)
    return t.get_error();
  while (1) {
    auto element = tokens.popNext();
    if (!element.contains<IdentifierToken>())
      return element.codeLoc.getError("Expected enum element, got: " + quote(element.value));
    ret->elements.push_back(element.value);
    if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
      break;
    if (auto t = tokens.eat(Keyword::COMMA); !t)
      return t.get_error();
    if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
      break;
  }
  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
    return t.get_error();
  return cast<Statement>(std::move(ret));
}

template <typename T, typename U>
static WithErrorLine<unique_ptr<T>> cast(WithErrorLine<unique_ptr<U>> elem) {
  if (!elem)
    return elem.get_error();
  return cast<T>(std::move(*elem));
}

WithErrorLine<unique_ptr<Statement>> parseStatement(Tokens& tokens, bool topLevel) {
  auto parseExpressionAndSemicolon = [&] () -> WithErrorLine<unique_ptr<ExpressionStatement>> {
    auto ret = parseExpression(tokens);
    if (!ret)
      return ret.get_error();
    if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
      return t.get_error();
    return unique<ExpressionStatement>(std::move(*ret));
  };
  auto token = tokens.peek();
  return token.visit(
      [](EofToken) -> WithErrorLine<unique_ptr<Statement>> {
        return unique_ptr<Statement>();
      },
      [&](const Keyword& k) -> WithErrorLine<unique_ptr<Statement>> {
        switch (k) {
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
            else {
              if (auto id = parseIdentifier(tokens, true)) {
                if (auto ret = parseFunctionSignature(*id, tokens)) {
                  ret.get()->external = true;
                  if (auto t = tokens.eat(Keyword::SEMICOLON); !t)
                    return t.get_error();
                  return cast<Statement>(std::move(ret));
                } else
                  return ret.get_error();
              } else
                return id.get_error();
            }
          case Keyword::STRUCT:
            return cast<Statement>(parseStructDefinition(tokens, false));
          case Keyword::VARIANT:
            return cast<Statement>(parseVariantDefinition(tokens));
          case Keyword::SWITCH:
            return cast<Statement>(parseSwitchStatement(tokens));
          case Keyword::ENUM:
            return parseEnumStatement(tokens);
          case Keyword::FOR:
            return parseForLoopStatement(tokens);
          case Keyword::WHILE:
            return cast<Statement>(parseWhileLoopStatement(tokens));
          case Keyword::PUBLIC: {
            tokens.popNext();
            auto next = tokens.peek();
            if (next == Keyword::IMPORT)
              return parseImportStatement(tokens, true);
            if (next.contains<EmbedToken>()) {
              auto ret = unique<EmbedStatement>(next.codeLoc, next.value);
              ret->isPublic = true;
              ret->isTopLevel = true;
              tokens.popNext();
              return cast<Statement>(std::move(ret));
            } else
              return next.codeLoc.getError("Unexpected definition after " + quote("public") + " keyword: " + quote(next.value));
          }
          case Keyword::IMPORT:
            return parseImportStatement(tokens, false);
          case Keyword::OPEN_BRACKET:
            return cast<Statement>(parseExpressionAndSemicolon());
          case Keyword::MUTABLE:
            if (topLevel) {
              if (auto id = parseIdentifier(tokens, true))
                return cast<Statement>(parseFunctionDefinition(*id, tokens));
              else
                return id.get_error();
            } else
              return cast<Statement>(parseVariableDeclaration(tokens));
          case Keyword::CONST:
            return cast<Statement>(parseVariableDeclaration(tokens));
          case Keyword::MOVE:
            return cast<Statement>(parseExpressionAndSemicolon());
          case Keyword::DISCARD: {
            tokens.popNext();
            auto ret = parseExpressionAndSemicolon();
            if (ret)
              ret.get()->canDiscard = true;
            return cast<Statement>(std::move(ret));
          }
          default:
            return token.codeLoc.getError("Unexpected keyword: " + quote(token.value));
        }
      },
      [&](const IdentifierToken&) -> WithErrorLine<unique_ptr<Statement>> {
        if (topLevel) {
          if (auto id = parseIdentifier(tokens, true))
            return cast<Statement>(parseFunctionDefinition(*id, tokens));
          else
            return id.get_error();
        } else {
          auto bookmark = tokens.getBookmark();
          auto decl = parseVariableDeclaration(tokens);
          if (!decl)
            return decl.get_error();
          if (*decl) {
            return cast<Statement>(std::move(decl));
          } else {
            tokens.rewind(bookmark);
            return cast<Statement>(parseExpressionAndSemicolon());
          }
        }
      },
      [&](EmbedToken) -> WithErrorLine<unique_ptr<Statement>> {
        auto text = token.value;
        auto ret = unique<EmbedStatement>(token.codeLoc, text);
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
  auto ret = parseStatement(tokens, false);
  if (!ret)
    return ret.get_error();
  if (ret.get()->allowTopLevel() == Statement::TopLevelAllowance::MUST)
    return ret.get()->codeLoc.getError("Statement only allowed in the top level of the program");
  return ret;
}

WithErrorLine<unique_ptr<Statement>> parseTopLevelStatement(Tokens& tokens) {
  auto statement = parseStatement(tokens, true);
  if (!statement)
    return statement.get_error();
  if (*statement && statement.get()->allowTopLevel() == Statement::TopLevelAllowance::CANT)
    return statement.get()->codeLoc.getError(
        "Statement not allowed in the top level of the program");
  return statement;
}

WithErrorLine<AST> parse(Tokens tokens) {
  AST ret;
  while (1) {
    if (auto s = parseTopLevelStatement(tokens)) {
      if (*s == nullptr)
        break;
      ret.elems.push_back(std::move(*s));
    } else
      return s.get_error();
  }
  return std::move(ret);
}
