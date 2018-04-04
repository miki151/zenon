#include <vector>
#include "parser.h"
#include "token.h"
#include "ast.h"

unique_ptr<Expression> parseExpression(Tokens&, int minPrecedence = 0);

unique_ptr<FunctionCallNamedArgs> parseFunctionCallWithNamedArguments(IdentifierInfo id, Tokens& tokens) {
  auto ret = unique<FunctionCallNamedArgs>(tokens.peek("function call").codeLoc, id);
  tokens.eat(Keyword::OPEN_BRACKET);
  tokens.eat(Keyword::OPEN_BLOCK);
  while (tokens.peek("function call argument") != Keyword::CLOSE_BLOCK) {
    auto token = tokens.popNext("function parameter initializer");
    token.codeLoc.check(token.contains<IdentifierToken>(), "function parameter expected");
    tokens.eat(Operator::ASSIGNMENT);
    ret->arguments.push_back({token.codeLoc, token.value, parseExpression(tokens)});
    if (tokens.peek("function parameter") == Keyword::COMMA)
      tokens.popNext();
  }
  tokens.eat(Keyword::CLOSE_BLOCK);
  tokens.eat(Keyword::CLOSE_BRACKET);
  return ret;
}

unique_ptr<Expression> parseFunctionCall(IdentifierInfo id, Tokens& tokens) {
  auto ret = unique<FunctionCall>(tokens.peek("function call").codeLoc, id);
  tokens.eat(Keyword::OPEN_BRACKET);
  if (tokens.peek("function call") == Keyword::OPEN_BLOCK) {
    tokens.rewind();
    return parseFunctionCallWithNamedArguments(id, tokens);
  }
  while (tokens.peek("function call argument") != Keyword::CLOSE_BRACKET) {
    ret->arguments.push_back(parseExpression(tokens));
    if (tokens.peek("function call argument") == Keyword::COMMA)
      tokens.popNext();
    else
      break;
  }
  tokens.eat(Keyword::CLOSE_BRACKET);
  return ret;
}

unique_ptr<Expression> parsePrimary(Tokens& tokens) {
  auto token = tokens.peek("primary expression");
  return token.visit(
      [&](const Keyword& k) -> unique_ptr<Expression> {
        switch (k) {
          case Keyword::OPEN_BRACKET: {
            tokens.popNext();
            auto ret = parseExpression(tokens);
            tokens.eat(Keyword::CLOSE_BRACKET);
            return ret;
          }
          case Keyword::FALSE:
          case Keyword::TRUE:
            tokens.popNext();
            return unique<Constant>(token.codeLoc, ArithmeticType::BOOL, token.value);
          default:
            token.codeLoc.error("Expected primary expression, got: " + quote(token.value));
            return {};
        }
      },
      [&](const IdentifierToken&) -> unique_ptr<Expression> {
        auto identifier = IdentifierInfo::parseFrom(tokens, false);
        auto token2 = tokens.peek("something");
        if (token2 == Keyword::OPEN_BRACKET) {
          auto ret = parseFunctionCall(identifier, tokens);
          return ret;
        } else
          return unique<Variable>(token.codeLoc, identifier);
      },
      [&](const Number&) -> unique_ptr<Expression> {
        tokens.popNext();
        return unique<Constant>(token.codeLoc, ArithmeticType::INT, token.value);
      },
      [&](const StringToken&) -> unique_ptr<Expression> {
        tokens.popNext();
        return unique<Constant>(token.codeLoc, ArithmeticType::STRING, token.value);
      },
      [&](const CharToken&) -> unique_ptr<Expression> {
        tokens.popNext();
        return unique<Constant>(token.codeLoc, ArithmeticType::CHAR, token.value);
      },
      [&](const Operator& op) -> unique_ptr<Expression> {
        tokens.popNext();
        token.codeLoc.check(isUnary(op), getString(op) + " is not a unary operator"s);
        auto exp = parseExpression(tokens, getPrecedence(op) + 1);
        return unique<UnaryExpression>(token.codeLoc, op, std::move(exp));
      },
      [&](const auto&) -> unique_ptr<Expression> {
        token.codeLoc.error("Expected primary expression, got: " + quote(token.value));
        return {};
      }
  );
}

unique_ptr<Expression> parseExpressionImpl(Tokens& tokens, unique_ptr<Expression> lhs, int minPrecedence) {
  while (1) {
    auto token = tokens.peek("arithmetic operator");
    if (auto op1 = token.getValueMaybe<Operator>()) {
      if (getPrecedence(*op1) < minPrecedence)
        break;
      tokens.popNext("arithmetic operator");
      auto rhs = parsePrimary(tokens);
      token = tokens.peek("arithmetic operator");
      while (1) {
        if (auto op2 = token.getReferenceMaybe<Operator>()) {
          INFO << "Comparing operators op1 " << getString(*op1) << " and op2 " << getString(*op2);
          if (getPrecedence(*op2) <= getPrecedence(*op1) &&
              (!isRightAssociative(*op2) || getPrecedence(*op2) < getPrecedence(*op1)))
            break;
          rhs = parseExpressionImpl(tokens, std::move(rhs), getPrecedence(*op2));
          token = tokens.peek("arithmetic operator");
        } else
          break;
      }
      lhs = unique<BinaryExpression>(token.codeLoc, *op1, std::move(lhs), std::move(rhs));
    } else
    if (token == Keyword::OPEN_SQUARE_BRACKET) {
      tokens.popNext();
      auto rhs = parseExpression(tokens);
      tokens.eat(Keyword::CLOSE_SQUARE_BRACKET);
      lhs = unique<BinaryExpression>(token.codeLoc, Operator::SUBSCRIPT, std::move(lhs), std::move(rhs));
    } else
      break;
  }
  return lhs;
}

unique_ptr<Expression> parseExpression(Tokens& tokens, int minPrecedence) {
  return parseExpressionImpl(tokens, parsePrimary(tokens), minPrecedence);
}

unique_ptr<Statement> parseNonTopLevelStatement(Tokens&);

unique_ptr<StatementBlock> parseBlock(Tokens& tokens) {
  auto block = unique<StatementBlock>(tokens.eat(Keyword::OPEN_BLOCK).codeLoc);
  while (1) {
    auto token2 = tokens.peek("statement");
    if (token2 == Keyword::CLOSE_BLOCK) {
      tokens.popNext("statement");
      break;
    }
    block->elems.push_back(parseNonTopLevelStatement(tokens));
  }
  return block;
}

unique_ptr<FunctionDefinition> parseFunctionSignature(IdentifierInfo type, Tokens& tokens) {
  auto token2 = tokens.popNext("function definition");
  token2.codeLoc.check(token2.contains<IdentifierToken>(), "Expected identifier, got: " + quote(token2.value));
  auto ret = unique<FunctionDefinition>(type.codeLoc, type, token2.value);
  tokens.eat(Keyword::OPEN_BRACKET);
  while (1) {
    if (auto keyword = tokens.peek("function parameter").getReferenceMaybe<Keyword>()) {
      if (*keyword == Keyword::CLOSE_BRACKET) {
        tokens.popNext("function parameter");
        break;
      } else
        tokens.eat(Keyword::COMMA);
    }
    auto typeId = IdentifierInfo::parseFrom(tokens, true);
    tokens.check(token2.contains<IdentifierToken>(), "Expected function parameter");
    auto nameToken = tokens.popNext("identifier");
    ret->parameters.push_back({type.codeLoc, typeId, nameToken.value});
  }
  return ret;
}

unique_ptr<FunctionDefinition> parseFunctionDefinition(IdentifierInfo type, Tokens& tokens) {
  auto ret = parseFunctionSignature(type, tokens);
  ret->body = parseBlock(tokens);
  return ret;
}

static vector<string> parseTemplateParams(Tokens& tokens) {
  tokens.eat(Keyword::TEMPLATE);
  tokens.eat(Operator::LESS_THAN);
  vector<string> params;
  while (tokens.peek("template definition") != Operator::MORE_THAN) {
    auto paramToken = tokens.popNext();
    paramToken.codeLoc.check(paramToken.contains<IdentifierToken>(), "Type parameter expected");
    params.push_back(paramToken.value);
    if (tokens.peek("template definition") != Operator::MORE_THAN)
      tokens.eat(Keyword::COMMA);
  }
  tokens.eat(Operator::MORE_THAN);
  return params;
}

unique_ptr<StructDefinition> parseStructDefinition(Tokens& tokens, bool external) {
  tokens.eat(Keyword::STRUCT);
  auto token2 = tokens.popNext("struct name");
  token2.codeLoc.check(token2.contains<IdentifierToken>(), "Expected struct name");
  auto ret = unique<StructDefinition>(token2.codeLoc, token2.value);
  if (external)
    ret->external = true;
  tokens.eat(Keyword::OPEN_BLOCK);
  while (1) {
    auto memberToken = tokens.peek("member declaration");
    if (memberToken == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    vector<string> templateParams;
    if (memberToken == Keyword::TEMPLATE)
      templateParams = parseTemplateParams(tokens);
    auto typeIdent = IdentifierInfo::parseFrom(tokens, true);
    auto memberName = tokens.popNext("member name");
    if (tokens.peek("struct definition") == Keyword::OPEN_BRACKET) {
      tokens.rewind();
      if (external) {
        ret->methods.push_back(parseFunctionSignature(typeIdent, tokens));
        tokens.eat(Keyword::SEMICOLON);
      } else
        ret->methods.push_back(parseFunctionDefinition(typeIdent, tokens));
      ret->methods.back()->templateParams = templateParams;
    } else {
      memberToken.codeLoc.check(templateParams.empty(), "Member variable can't have template parameters");
      memberName.codeLoc.check(memberName.contains<IdentifierToken>(), "Expected identifier");
      ret->members.push_back({typeIdent, memberName.value, token2.codeLoc});
      tokens.eat(Keyword::SEMICOLON);
    }
  }
  tokens.eat(Keyword::SEMICOLON);
  return ret;
}

unique_ptr<IfStatement> parseIfStatement(Tokens& tokens) {
  auto codeLoc = tokens.eat(Keyword::IF).codeLoc;
  tokens.eat(Keyword::OPEN_BRACKET);
  auto cond = parseExpression(tokens);
  tokens.eat(Keyword::CLOSE_BRACKET);
  auto ifTrue = parseNonTopLevelStatement(tokens);
  unique_ptr<Statement> ifFalse;
  if (!tokens.empty()) {
    auto token2 = tokens.peek();
    if (auto k1 = token2.getReferenceMaybe<Keyword>())
      if (*k1 == Keyword::ELSE) {
        tokens.popNext();
        ifFalse = parseNonTopLevelStatement(tokens);
      }
  }
  return unique<IfStatement>(codeLoc, std::move(cond), std::move(ifTrue), std::move(ifFalse));
}

unique_ptr<ReturnStatement> parseReturnStatement(Tokens& tokens) {
  auto ret = unique<ReturnStatement>(tokens.eat(Keyword::RETURN).codeLoc);
  auto token2 = tokens.peek("expression or " + quote(";"));
  if (auto keyword = token2.getReferenceMaybe<Keyword>())
    if (*keyword == Keyword::SEMICOLON) {
      tokens.eat(Keyword::SEMICOLON);
      return ret;
    }
  ret->expr = parseExpression(tokens);
  tokens.eat(Keyword::SEMICOLON);
  return ret;
}

unique_ptr<VariantDefinition> parseVariantDefinition(Tokens& tokens) {
  tokens.eat(Keyword::VARIANT);
  auto nameToken = tokens.popNext("variant name");
  nameToken.codeLoc.check(nameToken.contains<IdentifierToken>(), "Expected variant name");
  auto ret = unique<VariantDefinition>(nameToken.codeLoc, nameToken.value);
  tokens.eat(Keyword::OPEN_BLOCK);
  while (1) {
    auto memberToken = tokens.peek("variant definition");
    if (memberToken == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    vector<string> templateParams;
    if (memberToken == Keyword::TEMPLATE)
      templateParams = parseTemplateParams(tokens);
    auto typeIdent = IdentifierInfo::parseFrom(tokens, true);
    auto token2 = tokens.popNext("name of a variant alternative");
    token2.codeLoc.check(token2.contains<IdentifierToken>(), "Expected name of a variant alternative");
    if (tokens.peek("variant definition") == Keyword::OPEN_BRACKET) {
      tokens.rewind();
      ret->methods.push_back(parseFunctionDefinition(typeIdent, tokens));
      ret->methods.back()->templateParams = templateParams;
    } else {
      ret->elements.push_back(VariantDefinition::Element{typeIdent, token2.value, token2.codeLoc});
      tokens.eat(Keyword::SEMICOLON);
    }
  }
  tokens.eat(Keyword::SEMICOLON);
  return ret;
}

unique_ptr<ForLoopStatement> parseForLoopStatement(Tokens& tokens) {
  auto codeLoc = tokens.peek().codeLoc;
  tokens.eat(Keyword::FOR);
  tokens.eat(Keyword::OPEN_BRACKET);
  auto init = parseNonTopLevelStatement(tokens);
  auto cond = parseExpression(tokens);
  tokens.eat(Keyword::SEMICOLON);
  auto iter = parseExpression(tokens);
  tokens.eat(Keyword::CLOSE_BRACKET);
  auto body = parseNonTopLevelStatement(tokens);
  return unique<ForLoopStatement>(codeLoc, std::move(init), std::move(cond), std::move(iter), std::move(body));
}

unique_ptr<SwitchStatement> parseSwitchStatement(Tokens& tokens) {
  auto codeLoc = tokens.eat(Keyword::SWITCH).codeLoc;
  tokens.eat(Keyword::OPEN_BRACKET);
  auto expr = parseExpression(tokens);
  tokens.eat(Keyword::CLOSE_BRACKET);
  tokens.eat(Keyword::OPEN_BLOCK);
  auto ret = unique<SwitchStatement>(codeLoc, std::move(expr));
  while (1) {
    auto token2 = tokens.peek("switch body");
    if (token2 == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    if (token2 == Keyword::DEFAULT) {
      token2.codeLoc.check(!ret->defaultBlock, "Default switch statement is repeated");
      tokens.popNext();
      ret->defaultBlock = parseBlock(tokens);
    } else {
      tokens.eat(Keyword::CASE);
      tokens.eat(Keyword::OPEN_BRACKET);
      auto identifier = IdentifierInfo::parseFrom(tokens, true);
      SwitchStatement::CaseElem caseElem;
      caseElem.codeloc = token2.codeLoc;
      token2 = tokens.popNext("case statement");
      if (token2 != Keyword::CLOSE_BRACKET) {
        token2.codeLoc.check(token2.contains<IdentifierToken>(), "Expected variable identifier, got "
            + quote(token2.value));
        caseElem.id = token2.value;
        caseElem.type = identifier;
        tokens.eat(Keyword::CLOSE_BRACKET);
      } else {
        identifier.codeLoc.check(identifier.parts.size() == 1 && identifier.parts.at(0).templateArguments.empty(),
            "Identifier " + quote(identifier.toString()) + " is not a variable");
        caseElem.id = identifier.toString();
      }
      caseElem.block = parseBlock(tokens);
      ret->caseElems.push_back(std::move(caseElem));
    }
  }
  return ret;
}

unique_ptr<Statement> parseVariableDeclaration(Tokens& tokens) {
  auto typeLoc = tokens.peek().codeLoc;
  optional<IdentifierInfo> type;
  if (!tokens.eatMaybe(Keyword::AUTO))
    type = IdentifierInfo::parseFrom(tokens, true);
  auto token2 = tokens.peek("identifier");
  if (token2.contains<IdentifierToken>()) {
    tokens.popNext("identifier");
    if (tokens.peek("variable or function declaration") == Keyword::OPEN_BRACKET) {
      tokens.rewind();
      typeLoc.check(!!type, "Auto return type not supported for functions");
      return parseFunctionDefinition(*type, tokens);
    }
    unique_ptr<Expression> initExpression;
    if (tokens.peek("expression or " + quote(";")) != Keyword::SEMICOLON) {
      tokens.eat(Operator::ASSIGNMENT);
      initExpression = parseExpression(tokens);
    }
    tokens.eat(Keyword::SEMICOLON);
    return unique<VariableDeclaration>(typeLoc, type, token2.value, std::move(initExpression));
  } else
    return {};
}

unique_ptr<Statement> parseTemplateDefinition(Tokens& tokens) {
  auto params = parseTemplateParams(tokens);
  if (tokens.peek("Function or type definition") == Keyword::EXTERN) {
    tokens.popNext();
    auto ret = parseStructDefinition(tokens, true);
    ret->templateParams = params;
    return ret;
  } else
  if (tokens.peek("Function or type definition") == Keyword::STRUCT) {
    auto ret = parseStructDefinition(tokens, false);
    ret->templateParams = params;
    return ret;
  } else
  if (tokens.peek("Function or type definition") == Keyword::VARIANT) {
    auto ret = parseVariantDefinition(tokens);
    ret->templateParams = params;
    return ret;
  } else {
    auto ret = parseFunctionDefinition(IdentifierInfo::parseFrom(tokens, true), tokens);
    ret->templateParams = params;
    return ret;
  }
}

unique_ptr<Statement> parseImportStatement(Tokens& tokens, bool isPublic) {
  auto codeLoc = tokens.peek().codeLoc;
  tokens.eat(Keyword::IMPORT);
  auto path = tokens.eat(StringToken{}).value;
  path = getParentPath(codeLoc.file) + "/" + path;
  return unique<ImportStatement>(codeLoc, path, isPublic);
}

unique_ptr<Statement> parseEnumStatement(Tokens& tokens) {
  tokens.eat(Keyword::ENUM);
  auto name = tokens.popNext();
  name.codeLoc.check(name.contains<IdentifierToken>(), "Expected enum name, got: " + quote(name.value));
  auto ret = unique<EnumDefinition>(tokens.peek().codeLoc, name.value);
  tokens.eat(Keyword::OPEN_BLOCK);
  while (1) {
    auto element = tokens.popNext("enum elements");
    element.codeLoc.check(element.contains<IdentifierToken>(), "Expected enum element, got: " + quote(element.value));
    ret->elements.push_back(element.value);
    if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
      break;
    tokens.eat(Keyword::COMMA);
    if (tokens.eatMaybe(Keyword::CLOSE_BLOCK))
      break;
  }
  return ret;
}

unique_ptr<Statement> parseStatement(Tokens& tokens) {
  auto parseExpressionAndSemicolon = [&] {
    auto ret = parseExpression(tokens);
    tokens.eat(Keyword::SEMICOLON);
    return unique<ExpressionStatement>(std::move(ret));
  };
  auto token = tokens.peek("statement");
  return token.visit(
      [&](const Keyword& k) -> unique_ptr<Statement> {
        switch (k) {
          case Keyword::TEMPLATE:
            return parseTemplateDefinition(tokens);
          case Keyword::IF:
            return parseIfStatement(tokens);
          case Keyword::OPEN_BLOCK:
            return parseBlock(tokens);
          case Keyword::RETURN:
            return parseReturnStatement(tokens);
          case Keyword::STRUCT:
            return parseStructDefinition(tokens, false);
          case Keyword::VARIANT:
            return parseVariantDefinition(tokens);
          case Keyword::SWITCH:
            return parseSwitchStatement(tokens);
          case Keyword::ENUM:
            return parseEnumStatement(tokens);
          case Keyword::FOR:
            return parseForLoopStatement(tokens);
          case Keyword::PUBLIC: {
            tokens.popNext();
            auto next = tokens.peek("Import or embed block definition");
            if (next == Keyword::IMPORT)
              return parseImportStatement(tokens, true);
            if (next.contains<EmbedToken>()) {
              auto ret = unique<EmbedStatement>(next.codeLoc, next.value);
              ret->isPublic = true;
              tokens.popNext();
              return ret;
            } else {
              next.codeLoc.error("Unexpected definition after " + quote("public") + " keyword: " + quote(next.value));
              return {};
            }
          }
          case Keyword::IMPORT:
            return parseImportStatement(tokens, false);
          case Keyword::OPEN_BRACKET:
            return parseExpressionAndSemicolon();
          case Keyword::AUTO: {
            return parseVariableDeclaration(tokens);
          }
          default:
            token.codeLoc.error("Unexpected keyword: " + quote(token.value));
            return {};
        }
      },
      [&](const IdentifierToken&) -> unique_ptr<Statement> {
        auto bookmark = tokens.getBookmark();
        if (auto decl = parseVariableDeclaration(tokens))
          return decl;
        else {
          tokens.rewind(bookmark);
          return parseExpressionAndSemicolon();
        }
      },
      [&](EmbedToken) {
        auto text = token.value;
        auto ret = unique<EmbedStatement>(token.codeLoc, text);
        tokens.popNext();
        return ret;
      },
      [&](const auto&) -> unique_ptr<Statement> {
        return parseExpressionAndSemicolon();
      }
  );
}

unique_ptr<Statement> parseNonTopLevelStatement(Tokens& tokens) {
  auto ret = parseStatement(tokens);
  ret->codeLoc.check(ret->allowTopLevel() != Statement::TopLevelAllowance::MUST,
      "Statement only allowed in the top level of the program");
  return ret;
}

unique_ptr<Statement> parseTopLevelStatement(Tokens& tokens) {
  if (tokens.empty())
    return nullptr;
  auto statement = parseStatement(tokens);
  statement->codeLoc.check(statement->allowTopLevel() != Statement::TopLevelAllowance::CANT,
      "Statement not allowed in the top level of the program");
  return statement;
}

AST parse(Tokens tokens) {
  AST ret;
  while (auto s = parseTopLevelStatement(tokens))
    ret.elems.push_back(std::move(s));
  return ret;
}
