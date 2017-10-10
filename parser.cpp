#include <vector>
#include "parser.h"
#include "token.h"
#include "ast.h"

unique_ptr<Expression> parseExpression(Tokens&);
unique_ptr<FunctionCall> parseFunctionCall(Tokens&);

unique_ptr<Expression> parsePrimary(Tokens& tokens) {
  auto token = tokens.popNext("primary expression");
  return token.visit(
      [&](const Keyword& k) -> unique_ptr<Expression> {
        switch (k.type) {
          case Keyword::OPEN_BRACKET: {
            auto ret = parseExpression(tokens);
            tokens.eat(Keyword::CLOSE_BRACKET);
            return ret;
          }
          case Keyword::FALSE:
          case Keyword::TRUE:
            return unique<Constant>(token.codeLoc, ArithmeticType::BOOL, token.value);
          default:
            token.codeLoc.error("Expected primary expression, got: \"" + token.value + "\"");
            return {};
        }
      },
      [&](const Identifier&) -> unique_ptr<Expression> {
        auto token2 = tokens.peek("something");
        if (auto keyword = token2.getReferenceMaybe<Keyword>())
          if (keyword->type == Keyword::OPEN_BRACKET) {
            tokens.rewind();
            return parseFunctionCall(tokens);
          }
        return unique<Variable>(token.codeLoc, token.value);
      },
      [&](const Number& n) -> unique_ptr<Expression> {
        return unique<Constant>(token.codeLoc, ArithmeticType::INT, n.value);
      },
      [&](const auto&) -> unique_ptr<Expression> {
        token.codeLoc.error("Expected primary expression, got: \"" + token.value + "\"");
        return {};
      }
  );
}

static int getPrecedence(Operator op) {
  switch (op.type) {
    case '<':
    case '>':
      return 1;
    case '+':
    case '-':
      return 2;
    case '=':
      return 3;
    default:
      FATAL << "Unrecognized operator: " << op.type;
      return 0;
  }
}

bool isRightAssociative(Operator op) {
  switch (op.type) {
    case '=':
      return true;
    default:
      return false;
  }
}

unique_ptr<Expression> parseExpressionImpl(Tokens& tokens, unique_ptr<Expression> lhs, int minPrecedence) {
  auto token = tokens.peek("arithmetic operator");
  while (1) {
    if (auto op1 = token.getReferenceMaybe<Operator>()) {
      char opSym = op1->type;
      if (getPrecedence(*op1) < minPrecedence)
        break;
      tokens.popNext("arithmetic operator");
      auto rhs = parsePrimary(tokens);
      token = tokens.peek("arithmetic operator");
      while (1) {
        if (auto op2 = token.getReferenceMaybe<Operator>()) {
          if (getPrecedence(*op2) <= getPrecedence(*op1) &&
              (!isRightAssociative(*op2) || getPrecedence(*op2) < getPrecedence(*op1)))
            break;
          rhs = parseExpressionImpl(tokens, std::move(rhs), getPrecedence(*op2));
          token = tokens.peek("arithmetic operator");
        } else
          break;
          //ERROR << "2 Expected operator, got: \"" << token.value << "\"";
      }
      lhs = unique<BinaryExpression>(token.codeLoc, opSym, std::move(lhs), std::move(rhs));
    } else
      break;
      //ERROR << "3 Expected operator, got: \"" << token.value << "\"";
  }
  return lhs;
}

unique_ptr<Expression> parseExpression(Tokens& tokens) {
  return parseExpressionImpl(tokens, parsePrimary(tokens), 0);
}

unique_ptr<FunctionCall> parseFunctionCall(Tokens& tokens) {
  auto token = tokens.popNext("function call");
  auto ret = unique<FunctionCall>(token.codeLoc, token.value);
  tokens.eat(Keyword::OPEN_BRACKET);
  while (1) {
    if (auto keyword = tokens.peek("function argument").getReferenceMaybe<Keyword>()) {
      if (keyword->type == Keyword::CLOSE_BRACKET) {
        tokens.popNext("function argument");
        break;
      } else
        tokens.eat(Keyword::COMMA);
    }
    ret->arguments.push_back(parseExpression(tokens));
  }
  return ret;
}

unique_ptr<Statement> parseStatement(Tokens&);

unique_ptr<StatementBlock> parseBlock(Tokens& tokens) {
  auto block = unique<StatementBlock>(tokens.peek("statement").codeLoc);
  while (1) {
    auto token2 = tokens.peek("statement");
    if (auto k1 = token2.getReferenceMaybe<Keyword>())
      if (k1->type == Keyword::CLOSE_BLOCK) {
        tokens.popNext("statement");
        break;
      }
    block->elems.push_back(parseStatement(tokens));
  }
  return block;
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
        switch (k.type) {
          case Keyword::IF: {
            tokens.eat(Keyword::OPEN_BRACKET);
            auto cond = parseExpression(tokens);
            tokens.eat(Keyword::CLOSE_BRACKET);
            auto ifTrue = parseStatement(tokens);
            unique_ptr<Statement> ifFalse;
            if (!tokens.empty()) {
              auto& token2 = tokens.peek();
              if (auto k1 = token2.getReferenceMaybe<Keyword>())
                if (k1->type == Keyword::ELSE) {
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
            auto token2 = tokens.peek("expression or \";\"");
            if (auto keyword = token2.getReferenceMaybe<Keyword>())
              if (keyword->type == Keyword::SEMICOLON) {
                tokens.eat(Keyword::SEMICOLON);
                return ret;
              }
            ret->expr = parseExpression(tokens);
            tokens.eat(Keyword::SEMICOLON);
            return ret;
          }
          default:
            tokens.error("Unexpected keyword: \"" + token.value + "\"");
            return {};
        }
      },
      [&](const Identifier&) -> unique_ptr<Statement> {
        auto token2 = tokens.peek("identifier");
        if (token2.contains<Identifier>()) {
          tokens.popNext("identifier");
          tokens.eat(Keyword::SEMICOLON);
          return unique<VariableDecl>(token.codeLoc, token.value, token2.value);
        } else
          return rewindAndParseExpression();
      },
      [&](const auto&) -> unique_ptr<Statement> {
        return rewindAndParseExpression();
      }
  );
}

unique_ptr<TopLevelStatement> parseTopLevelStatement(Tokens& tokens) {
  if (tokens.empty())
    return nullptr;
  auto token = tokens.popNext();
  return token.visit(
      [&](const Identifier&) -> unique_ptr<TopLevelStatement> {
        auto token2 = tokens.popNext("function definition");
        CHECK_SYNTAX(token2.contains<Identifier>()) << "Expected identifier, got: \"" + token2.value + "\"";
        auto ret = unique<FunctionDefinition>(token.codeLoc, token.value, token2.value);
        tokens.eat(Keyword::OPEN_BRACKET);
        while (1) {
          if (auto keyword = tokens.peek("function parameter").getReferenceMaybe<Keyword>()) {
            if (keyword->type == Keyword::CLOSE_BRACKET) {
              tokens.popNext("function parameter");
              break;
            } else
              tokens.eat(Keyword::COMMA);
          }
          auto typeToken = tokens.popNext("identifier");
          tokens.check(token2.contains<Identifier>(), "Expected function parameter");
          auto nameToken = tokens.popNext("identifier");
          ret->parameters.push_back(unique<VariableDecl>(token.codeLoc, typeToken.value, nameToken.value));
        }
        tokens.eat(Keyword::OPEN_BLOCK);
        ret->body = parseBlock(tokens);
        return ret;
      },
      [&](const auto&) -> unique_ptr<TopLevelStatement> {
        ERROR << "Expected function definition, got \"" << token.value << "\"";
        return {};
      }
  );
}

AST parse(Tokens tokens) {
  AST ret;
  while (auto s = parseTopLevelStatement(tokens))
    ret.elems.push_back(std::move(s));
  return ret;
}
