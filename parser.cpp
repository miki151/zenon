#include <vector>
#include "parser.h"
#include "token.h"
#include "ast.h"

unique_ptr<Expression> parseExpression(Tokens&);

unique_ptr<FunctionCallNamedArgs> parseFunctionCallWithNamedArguments(Tokens& tokens) {
  auto token = tokens.popNext("function call");
  auto ret = unique<FunctionCallNamedArgs>(token.codeLoc, token.value);
  tokens.eat(Keyword::OPEN_BLOCK);
  while (tokens.peek("function call argument") != Keyword::CLOSE_BLOCK) {
    token = tokens.popNext("function parameter initializer");
    token.codeLoc.check(token.contains<Identifier>(), "function parameter expected");
    tokens.eat(BinaryOperator::ASSIGNMENT);
    ret->arguments.push_back({token.codeLoc, token.value, parseExpression(tokens)});
    if (tokens.peek("function parameter") == Keyword::COMMA)
      tokens.popNext();
  }
  tokens.eat(Keyword::CLOSE_BLOCK);
  return ret;
}

unique_ptr<FunctionCall> parseFunctionCall(Tokens& tokens) {
  auto token = tokens.popNext("function call");
  auto ret = unique<FunctionCall>(token.codeLoc, token.value);
  tokens.eat(Keyword::OPEN_BRACKET);
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

unique_ptr<Expression> parsePrimary(Tokens& tokens, optional<BinaryOperator> precedingOp) {
  auto token = tokens.popNext("primary expression");
  return token.visit(
      [&](const Keyword& k) -> unique_ptr<Expression> {
        switch (k) {
          case Keyword::OPEN_BRACKET: {
            auto ret = parseExpression(tokens);
            tokens.eat(Keyword::CLOSE_BRACKET);
            return ret;
          }
          case Keyword::FALSE:
          case Keyword::TRUE:
            return unique<Constant>(token.codeLoc, ArithmeticType::BOOL, token.value);
          default:
            token.codeLoc.error("Expected primary expression, got: " + quote(token.value));
            return {};
        }
      },
      [&](const Identifier&) -> unique_ptr<Expression> {
        auto token2 = tokens.peek("something");
        if (token2 == Keyword::OPEN_BRACKET) {
          tokens.rewind();
          return parseFunctionCall(tokens);
        } else
        if (token2 == Keyword::OPEN_BLOCK) {
          tokens.rewind();
          return parseFunctionCallWithNamedArguments(tokens);
        }
        if (precedingOp != BinaryOperator::MEMBER_ACCESS)
          return unique<Variable>(token.codeLoc, token.value);
        else
          return unique<MemberAccessType>(token.codeLoc, token.value);
      },
      [&](const Number&) -> unique_ptr<Expression> {
        return unique<Constant>(token.codeLoc, ArithmeticType::INT, token.value);
      },
      [&](const auto&) -> unique_ptr<Expression> {
        token.codeLoc.error("Expected primary expression, got: " + quote(token.value));
        return {};
      }
  );
}

unique_ptr<Expression> parseExpressionImpl(Tokens& tokens, unique_ptr<Expression> lhs, int minPrecedence) {
  auto token = tokens.peek("arithmetic operator");
  while (1) {
    if (auto op1 = token.getValueMaybe<BinaryOperator>()) {
      if (getPrecedence(*op1) < minPrecedence)
        break;
      tokens.popNext("arithmetic operator");
      auto rhs = parsePrimary(tokens, *op1);
      token = tokens.peek("arithmetic operator");
      while (1) {
        if (auto op2 = token.getReferenceMaybe<BinaryOperator>()) {
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
      break;
  }
  return lhs;
}

unique_ptr<Expression> parseExpression(Tokens& tokens) {
  return parseExpressionImpl(tokens, parsePrimary(tokens, none), 0);
}

unique_ptr<Statement> parseStatement(Tokens&);

unique_ptr<StatementBlock> parseBlock(Tokens& tokens) {
  auto block = unique<StatementBlock>(tokens.peek("statement").codeLoc);
  while (1) {
    auto token2 = tokens.peek("statement");
    if (token2 == Keyword::CLOSE_BLOCK) {
      tokens.popNext("statement");
      break;
    }
    block->elems.push_back(parseStatement(tokens));
  }
  return block;
}

unique_ptr<EmbedBlock> parseEmbedBlock(Tokens& tokens) {
  auto block = unique<EmbedBlock>(tokens.peek("statement").codeLoc);
  int bracketCount = 1;
  while (1) {
    auto token2 = tokens.popNext("embedded function body");
    if (token2 == Keyword::CLOSE_BLOCK && --bracketCount == 0)
      break;
    block->content.append(token2.value);
  }
  return block;
}

unique_ptr<Statement> parseFunctionDefinition(Token idToken, Tokens& tokens) {
  auto token2 = tokens.popNext("function definition");
  CHECK_SYNTAX(token2.contains<Identifier>()) << "Expected identifier, got: " + quote(token2.value);
  auto ret = unique<FunctionDefinition>(idToken.codeLoc, idToken.value, token2.value);
  tokens.eat(Keyword::OPEN_BRACKET);
  while (1) {
    if (auto keyword = tokens.peek("function parameter").getReferenceMaybe<Keyword>()) {
      if (*keyword == Keyword::CLOSE_BRACKET) {
        tokens.popNext("function parameter");
        break;
      } else
        tokens.eat(Keyword::COMMA);
    }
    auto typeToken = tokens.popNext("identifier");
    tokens.check(token2.contains<Identifier>(), "Expected function parameter");
    auto nameToken = tokens.popNext("identifier");
    ret->parameters.push_back({idToken.codeLoc, typeToken.value, nameToken.value});
  }
  token2 = tokens.popNext("function body");
  if (token2 == Keyword::OPEN_BLOCK)
    ret->body = parseBlock(tokens);
  else
  if (token2 == Keyword::EMBED) {
    tokens.eat(Keyword::OPEN_BLOCK);
    ret->body = parseEmbedBlock(tokens);
    ret->embed = true;
  }
  return ret;
}

unique_ptr<Statement> parseStatement(Tokens& tokens) {
  auto token = tokens.popNext("statement");
  auto rewindAndParseExpression = [&] {
    tokens.rewind();
    auto ret = parseExpression(tokens);
    tokens.eat(Keyword::SEMICOLON);
    return unique<ExpressionStatement>(std::move(ret));
  };
  return token.visit(
      [&](const Keyword& k) -> unique_ptr<Statement> {
        switch (k) {
          case Keyword::IF: {
            tokens.eat(Keyword::OPEN_BRACKET);
            auto cond = parseExpression(tokens);
            tokens.eat(Keyword::CLOSE_BRACKET);
            auto ifTrue = parseStatement(tokens);
            unique_ptr<Statement> ifFalse;
            if (!tokens.empty()) {
              auto& token2 = tokens.peek();
              if (auto k1 = token2.getReferenceMaybe<Keyword>())
                if (*k1 == Keyword::ELSE) {
                  tokens.popNext();
                  ifFalse = parseStatement(tokens);
                }
            }
            return unique<IfStatement>(token.codeLoc, std::move(cond), std::move(ifTrue), std::move(ifFalse));
          }
          case Keyword::OPEN_BLOCK:
            return parseBlock(tokens);
          case Keyword::RETURN: {
            auto ret = unique<ReturnStatement>(token.codeLoc);
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
          case Keyword::STRUCT: {
            auto token2 = tokens.popNext("struct name");
            token2.codeLoc.check(token2.contains<Identifier>(), "Expected struct name");
            auto ret = unique<StructDeclaration>(token.codeLoc, token2.value);
            tokens.eat(Keyword::OPEN_BLOCK);
            while (1) {
              token2 = tokens.popNext("member declaration");
              if (token2 == Keyword::CLOSE_BLOCK)
                break;
              auto memberName = tokens.popNext("member name");
              token2.codeLoc.check(token2.contains<Identifier>(), "Expected identifier");
              memberName.codeLoc.check(memberName.contains<Identifier>(), "Expected identifier");
              ret->members.push_back({token2.value, memberName.value, token2.codeLoc});
              tokens.eat(Keyword::SEMICOLON);
            }
            tokens.eat(Keyword::SEMICOLON);
            return ret;
          }
          case Keyword::EMBED: {
            auto content = tokens.popNext("Embedded statement");
            return unique<EmbedInclude>(content.codeLoc, content.value);
          }
          default:
            tokens.error("Unexpected keyword: " + quote(token.value));
            return {};
        }
      },
      [&](const Identifier&) -> unique_ptr<Statement> {
        auto token2 = tokens.peek("identifier");
        if (token2.contains<Identifier>()) {
          tokens.popNext("identifier");
          if (tokens.peek("variable or function declaration") == Keyword::OPEN_BRACKET) {
            tokens.rewind();
            return parseFunctionDefinition(token, tokens);
          }
          unique_ptr<Expression> initExpression;
          if (tokens.peek("expression or " + quote(";")) != Keyword::SEMICOLON) {
            tokens.eat(BinaryOperator::ASSIGNMENT);
            initExpression = parseExpression(tokens);
          }
          tokens.eat(Keyword::SEMICOLON);
          return unique<VariableDeclaration>(token.codeLoc, token.value, token2.value, std::move(initExpression));
        } else
          return rewindAndParseExpression();
      },
      [&](const auto&) -> unique_ptr<Statement> {
        return rewindAndParseExpression();
      }
  );
}

unique_ptr<Statement> parseTopLevelStatement(Tokens& tokens) {
  if (tokens.empty())
    return nullptr;
  auto statement = parseStatement(tokens);
  statement->codeLoc.check(statement->allowTopLevel(), "Statement not allowed in the top level of the program");
  return statement;
}

AST parse(Tokens tokens) {
  AST ret;
  while (auto s = parseTopLevelStatement(tokens))
    ret.elems.push_back(std::move(s));
  return ret;
}
