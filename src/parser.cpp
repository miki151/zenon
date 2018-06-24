#include <vector>
#include "parser.h"
#include "token.h"
#include "ast.h"
#include "lexer.h"

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

static string getInputReg() {
  return "((?:(?:\\\\\\{)|[^\\{])+)|(\\{.*?\\})";
  /*string inputReg;
  std::cin >> inputReg;
  return inputReg;*/
}

unique_ptr<Expression> parseStringLiteral(CodeLoc initialLoc, string literal) {
  if (literal.empty())
    return unique<Constant>(initialLoc, ArithmeticType::STRING, "");
  unique_ptr<Expression> left;
  auto addElem = [&left] (unique_ptr<Expression> right, CodeLoc loc) {
    if (!left)
      left = std::move(right);
    else
      left = unique<BinaryExpression>(loc, Operator::PLUS, std::move(left), std::move(right));
  };
  regex re(getInputReg());
  int lastPos = 0;
  auto words_begin = sregex_iterator(literal.begin(), literal.end(), re);
  auto words_end = sregex_iterator();
  for (auto it = words_begin; it != words_end; ++it)
    for (int index = 0 ; index < it->size(); ++index)
      if (!it->str(index + 1).empty()) { // determine which submatch was matched
        CodeLoc loc = initialLoc.plus(0, (int) it->position() + 1);
        //cout << "Matched \"" << it->str() << "\" with rule " << index << " at " << loc.line << ":" << loc.column << endl;
        if (index == 0) {
          addElem(unique<Constant>(loc, ArithmeticType::STRING,
              std::regex_replace(it->str(), std::regex("\\\\([\\{\\}])"), "$1")), loc);
        } else {
          loc = loc.plus(0, 1);
          auto call = unique<FunctionCall>(loc, IdentifierInfo("to_string"));
          auto tokens = lex("(" + it->str().substr(1, it->str().size() - 2) + ")", loc);
          call->arguments.push_back(unique<UnaryExpression>(loc, Operator::GET_ADDRESS, parseExpression(tokens)));
          addElem(std::move(call), loc);
        }
      }
  return left;
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
          case Keyword::MOVE: {
            tokens.popNext();
            tokens.eat(Keyword::OPEN_BRACKET);
            auto var = tokens.popNext();
            tokens.eat(Keyword::CLOSE_BRACKET);
            var.codeLoc.check(var.contains<IdentifierToken>(), "Expected variable identifier");
            return unique<MoveExpression>(token.codeLoc, var.value);
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
        } else {
          CHECK(identifier.parts[0].templateArguments.empty()) << identifier.toString();
          if (identifier.parts.size() == 1)
            return unique<Variable>(token.codeLoc, identifier.parts[0].name);
          else
            return unique<EnumConstant>(token.codeLoc, identifier.parts[0].name, identifier.parts[1].name);
        }
      },
      [&](const Number&) -> unique_ptr<Expression> {
        tokens.popNext();
        return unique<Constant>(token.codeLoc, ArithmeticType::INT, token.value);
      },
      [&](const RealNumber&) -> unique_ptr<Expression> {
        tokens.popNext();
        return unique<Constant>(token.codeLoc, ArithmeticType::DOUBLE, token.value);
      },
      [&](const StringToken&) -> unique_ptr<Expression> {
        tokens.popNext();
        return parseStringLiteral(token.codeLoc, token.value);
      },
      [&](const CharToken&) -> unique_ptr<Expression> {
        tokens.popNext();
        return unique<Constant>(token.codeLoc, ArithmeticType::CHAR, token.value);
      },
      [&](const Operator& op) -> unique_ptr<Expression> {
        tokens.popNext();
        if (auto opUnary = getUnary(op)) {
          auto exp = parseExpression(tokens, getPrecedence(*opUnary) + 1);
          return unique<UnaryExpression>(token.codeLoc, *opUnary, std::move(exp));
        } else
          token.codeLoc.error(getString(op) + " is not a unary operator"s);
      },
      [&](const auto&) -> unique_ptr<Expression> {
        token.codeLoc.error("Expected primary expression, got: " + quote(token.value));
        return {};
      }
  );
}

unique_ptr<Expression> parseExpressionImpl(Tokens& tokens, unique_ptr<Expression> lhs, int minPrecedence) {
  while (1) {
    if (tokens.empty())
      break;
    auto token = tokens.peek("arithmetic operator");
    if (auto op1 = token.getValueMaybe<Operator>()) {
      if (getPrecedence(*op1) < minPrecedence)
        break;
      tokens.popNext("arithmetic operator");
      auto rhs = parsePrimary(tokens);
      token = tokens.peek("arithmetic operator");
      while (1) {
        auto op2 = token.getValueMaybe<Operator>();
        if (token == Keyword::OPEN_SQUARE_BRACKET)
          op2 = Operator::SUBSCRIPT;
        if (op2) {
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
  unique_ptr<FunctionDefinition> ret;
  if (token2 == Keyword::OPERATOR) {
    if (auto op = tokens.peek("operator").getValueMaybe<Operator>()) {
      tokens.popNext();
      ret = unique<FunctionDefinition>(type.codeLoc, type, *op);
    } else {
      tokens.eat(Keyword::OPEN_SQUARE_BRACKET);
      tokens.eat(Keyword::CLOSE_SQUARE_BRACKET);
      ret = unique<FunctionDefinition>(type.codeLoc, type, Operator::SUBSCRIPT);
    }
  } else {
    token2.codeLoc.check(token2.contains<IdentifierToken>(), "Expected identifier, got: " + quote(token2.value));
    ret = unique<FunctionDefinition>(type.codeLoc, type, token2.value);
  }
  tokens.eat(Keyword::OPEN_BRACKET);
  while (1) {
    if (tokens.eatMaybe(Keyword::CLOSE_BRACKET))
      break;
    if (!ret->parameters.empty())
      tokens.eat(Keyword::COMMA);
    bool isParamMutable = !!tokens.eatMaybe(Keyword::MUTABLE);
    auto typeId = IdentifierInfo::parseFrom(tokens, true);
    optional<string> paramName;
    auto nameToken = tokens.peek("identifier");
    if (nameToken.contains<IdentifierToken>()) {
      paramName = nameToken.value;
      tokens.popNext();
    }
    ret->parameters.push_back({type.codeLoc, typeId, paramName, isParamMutable});
  }
  if (ret->parameters.size() == 1)
    if (auto op = ret->name.getValueMaybe<Operator>())
      if (auto opUnary = getUnary(*op))
        ret->name = *opUnary;
  return ret;
}

unique_ptr<FunctionDefinition> parseFunctionDefinition(IdentifierInfo type, Tokens& tokens) {
  auto ret = parseFunctionSignature(type, tokens);
  if (tokens.peek("Function definition") == Keyword::OPEN_BLOCK)
    ret->body = parseBlock(tokens);
  else
    tokens.eat(Keyword::SEMICOLON);
  return ret;
}

unique_ptr<FunctionDefinition> parseConstructorDefinition(IdentifierInfo type, Tokens& tokens) {
  auto ret = parseFunctionSignature(type, tokens);
  if (tokens.eatMaybe(Keyword::COLON)) {
    while (1) {
      auto id = tokens.popNext("member name");
      id.codeLoc.check(id.contains<IdentifierToken>(), "Expected member name");
      tokens.eat(Keyword::OPEN_BRACKET);
      auto expr = parseExpression(tokens);
      tokens.eat(Keyword::CLOSE_BRACKET);
      ret->initializers.push_back(FunctionDefinition::Initializer{id.codeLoc, id.value, std::move(expr)});
      if (tokens.peek("constructor definition") == Keyword::OPEN_BLOCK)
        break;
      tokens.eat(Keyword::COMMA);
    }
  }
  ret->name = ConstructorId{*ret->name.getValueMaybe<string>()};
  ret->body = parseBlock(tokens);
  return ret;
}

static TemplateInfo parseTemplateInfo(Tokens& tokens) {
  tokens.eat(Keyword::TEMPLATE);
  tokens.eat(Operator::LESS_THAN);
  TemplateInfo ret;
  while (tokens.peek("template definition") != Operator::MORE_THAN) {
    auto paramToken = tokens.popNext();
    paramToken.codeLoc.check(paramToken.contains<IdentifierToken>(), "Type parameter expected");
    ret.params.push_back({paramToken.value, paramToken.codeLoc});
    if (tokens.peek("template definition") != Operator::MORE_THAN)
      tokens.eat(Keyword::COMMA);
  }
  tokens.eat(Operator::MORE_THAN);
  if (tokens.peek("function or struct definition") == Keyword::REQUIRES) {
    tokens.popNext();
    while (1) {
      ret.requirements.push_back(IdentifierInfo::parseFrom(tokens, false));
      if (!tokens.eatMaybe(Keyword::COMMA))
        break;
    }
  }
  return ret;
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
    TemplateInfo templateParams;
    if (memberToken == Keyword::TEMPLATE)
      templateParams = parseTemplateInfo(tokens);
    auto typeIdent = IdentifierInfo::parseFrom(tokens, true);
    auto memberName = tokens.popNext("member name");
    if (memberName == Keyword::OPEN_BRACKET) { // constructor
      tokens.rewind();
      tokens.rewind();
      if (external) {
        ret->methods.push_back(parseFunctionSignature(typeIdent, tokens));
        ret->methods.back()->name = ConstructorId{*ret->methods.back()->name.getValueMaybe<string>()};
        tokens.eat(Keyword::SEMICOLON);
      } else
        ret->methods.push_back(parseConstructorDefinition(typeIdent, tokens));
      ret->methods.back()->templateInfo = templateParams;
    } else { // member
      memberToken.codeLoc.check(templateParams.params.empty(), "Member variable can't have template parameters");
      memberName.codeLoc.check(memberName.contains<IdentifierToken>(), "Expected identifier");
      ret->members.push_back({typeIdent, memberName.value, memberToken.codeLoc});
      tokens.eat(Keyword::SEMICOLON);
    }
  }
  tokens.eat(Keyword::SEMICOLON);
  return ret;
}

unique_ptr<ConceptDefinition> parseConceptDefinition(Tokens& tokens) {
  tokens.eat(Keyword::CONCEPT);
  auto token2 = tokens.popNext("concept name");
  token2.codeLoc.check(token2.contains<IdentifierToken>(), "Expected struct name");
  auto ret = unique<ConceptDefinition>(token2.codeLoc, token2.value);
  tokens.eat(Keyword::OPEN_BLOCK);
  while (1) {
    auto memberToken = tokens.peek("type or function signature");
    if (memberToken == Keyword::CLOSE_BLOCK) {
      tokens.popNext();
      break;
    }
    auto typeId = IdentifierInfo::parseFrom(tokens, true);
    bool constructor = false;
    if (tokens.peek() == Keyword::OPEN_BRACKET) {
      tokens.rewind();
      constructor = true;
    }
    ret->functions.push_back(parseFunctionSignature(typeId, tokens));
    if (constructor)
      ret->functions.back()->name = ConstructorId{*ret->functions.back()->name.getValueMaybe<string>()};
    tokens.eat(Keyword::SEMICOLON);
  }
  tokens.eat(Keyword::SEMICOLON);
  return ret;
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
    TemplateInfo templateParams;
    if (memberToken == Keyword::TEMPLATE)
      templateParams = parseTemplateInfo(tokens);
    auto typeIdent = IdentifierInfo::parseFrom(tokens, true);
    auto token2 = tokens.popNext("name of a variant alternative");
    token2.codeLoc.check(token2.contains<IdentifierToken>(), "Expected name of a variant alternative");
    ret->elements.push_back(VariantDefinition::Element{typeIdent, token2.value, token2.codeLoc});
    tokens.eat(Keyword::SEMICOLON);
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

unique_ptr<WhileLoopStatement> parseWhileLoopStatement(Tokens& tokens) {
  auto codeLoc = tokens.peek().codeLoc;
  tokens.eat(Keyword::WHILE);
  tokens.eat(Keyword::OPEN_BRACKET);
  auto cond = parseExpression(tokens);
  tokens.eat(Keyword::CLOSE_BRACKET);
  auto body = parseNonTopLevelStatement(tokens);
  return unique<WhileLoopStatement>(codeLoc, std::move(cond), std::move(body));
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

unique_ptr<VariableDeclaration> parseVariableDeclaration(Tokens& tokens, bool eatSemicolon = true) {
  auto typeLoc = tokens.peek().codeLoc;
  optional<IdentifierInfo> type;
  bool isMutable = false;
  bool isDeclaration = false;
  if (tokens.eatMaybe(Keyword::MUTABLE)) {
    isMutable = true;
    isDeclaration = true;
  } else if (tokens.eatMaybe(Keyword::CONST))
    isDeclaration = true;
  auto id1 = IdentifierInfo::parseFrom(tokens, true);
  string variableName;
  if (tokens.peek("Variable declaraion") == Operator::ASSIGNMENT && isDeclaration) {
    if (auto name = id1.asBasicIdentifier())
      variableName = *name;
    else
      id1.codeLoc.error("Expected variable name");
  } else {
    type = id1;
    auto id2 = tokens.popNext("Variable name");
    if (id2.contains<IdentifierToken>())
      variableName = id2.value;
    else if (!isDeclaration)
      return {};
    else id2.codeLoc.error("Expected variable name, got " + quote(id2.value));

  }
  unique_ptr<Expression> initExpression;
  if (tokens.eatMaybe(Operator::ASSIGNMENT))
    initExpression = parseExpression(tokens);
  if (eatSemicolon)
    tokens.eat(Keyword::SEMICOLON);
  auto ret = unique<VariableDeclaration>(typeLoc, type, variableName, std::move(initExpression));
  ret->isMutable = isMutable;
  return ret;
}

unique_ptr<IfStatement> parseIfStatement(Tokens& tokens) {
  auto codeLoc = tokens.eat(Keyword::IF).codeLoc;
  tokens.eat(Keyword::OPEN_BRACKET);
  unique_ptr<VariableDeclaration> decl;
  unique_ptr<Expression> cond;
  if (tokens.peek("if statement") == Keyword::CONST || tokens.peek() == Keyword::MUTABLE) {
    decl = parseVariableDeclaration(tokens, false);
    if (tokens.peek("if statement") == Keyword::SEMICOLON) {
      tokens.popNext();
      cond = parseExpression(tokens);
    }
  } else
    cond = parseExpression(tokens);
  codeLoc.check(!!decl || !!cond, "Expected condition expression or declaration");
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
  return unique<IfStatement>(codeLoc, std::move(decl), std::move(cond), std::move(ifTrue), std::move(ifFalse));
}

unique_ptr<Statement> parseTemplateDefinition(Tokens& tokens) {
  auto templateInfo = parseTemplateInfo(tokens);
  auto nextToken = tokens.peek("Function or type definition");
  auto checkNameConflict = [&templateInfo] (const string& name, const string& type) {
    for (auto& param : templateInfo.params)
      param.codeLoc.check(param.name != name, "Template parameter conflicts with " + type + " name");
  };
  if (nextToken == Keyword::EXTERN) {
    tokens.popNext();
    auto ret = parseStructDefinition(tokens, true);
    checkNameConflict(ret->name, "struct");
    ret->templateInfo = templateInfo;
    return ret;
  } else
  if (nextToken == Keyword::STRUCT) {
    auto ret = parseStructDefinition(tokens, false);
    checkNameConflict(ret->name, "struct");
    ret->templateInfo = templateInfo;
    return ret;
  } else
  if (nextToken == Keyword::VARIANT) {
    auto ret = parseVariantDefinition(tokens);
    checkNameConflict(ret->name, "variant");
    ret->templateInfo = templateInfo;
    return ret;
  } else
  if (nextToken == Keyword::CONCEPT) {
    auto ret = parseConceptDefinition(tokens);
    checkNameConflict(ret->name, "concept");
    ret->templateInfo = templateInfo;
    return ret;
  } else {
    auto ret = parseFunctionDefinition(IdentifierInfo::parseFrom(tokens, true), tokens);
    if (auto name = ret->name.getReferenceMaybe<string>())
      checkNameConflict(*name, "function");
    ret->templateInfo = templateInfo;
    return ret;
  }
}

unique_ptr<Statement> parseImportStatement(Tokens& tokens, bool isPublic) {
  auto codeLoc = tokens.peek().codeLoc;
  tokens.eat(Keyword::IMPORT);
  auto path = tokens.eat(StringToken{}).value;
  tokens.eat(Keyword::SEMICOLON);
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

unique_ptr<Statement> parseStatement(Tokens& tokens, bool topLevel) {
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
          case Keyword::EXTERN:
            tokens.popNext();
            return parseStructDefinition(tokens, true);
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
          case Keyword::WHILE:
            return parseWhileLoopStatement(tokens);
          case Keyword::PUBLIC: {
            tokens.popNext();
            auto next = tokens.peek("Import or embed block definition");
            if (next == Keyword::IMPORT)
              return parseImportStatement(tokens, true);
            if (next.contains<EmbedToken>()) {
              auto ret = unique<EmbedStatement>(next.codeLoc, next.value);
              ret->isPublic = true;
              ret->isTopLevel = true;
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
          case Keyword::MUTABLE:
            if (topLevel)
              return parseFunctionDefinition(IdentifierInfo::parseFrom(tokens, true), tokens);
            else
              return parseVariableDeclaration(tokens);
          case Keyword::CONST:
            return parseVariableDeclaration(tokens);
          case Keyword::MOVE:
            return parseExpressionAndSemicolon();
          default:
            token.codeLoc.error("Unexpected keyword: " + quote(token.value));
            return {};
        }
      },
      [&](const IdentifierToken&) -> unique_ptr<Statement> {
        if (topLevel)
          return parseFunctionDefinition(IdentifierInfo::parseFrom(tokens, true), tokens);
        else {
          auto bookmark = tokens.getBookmark();
          if (auto decl = parseVariableDeclaration(tokens))
            return decl;
          else {
            tokens.rewind(bookmark);
            return parseExpressionAndSemicolon();
          }
        }
      },
      [&](EmbedToken) {
        auto text = token.value;
        auto ret = unique<EmbedStatement>(token.codeLoc, text);
        ret->isTopLevel = topLevel;
        tokens.popNext();
        return ret;
      },
      [&](const auto&) -> unique_ptr<Statement> {
        return parseExpressionAndSemicolon();
      }
  );
}

unique_ptr<Statement> parseNonTopLevelStatement(Tokens& tokens) {
  auto ret = parseStatement(tokens, false);
  ret->codeLoc.check(ret->allowTopLevel() != Statement::TopLevelAllowance::MUST,
      "Statement only allowed in the top level of the program");
  return ret;
}

unique_ptr<Statement> parseTopLevelStatement(Tokens& tokens) {
  if (tokens.empty())
    return nullptr;
  auto statement = parseStatement(tokens, true);
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
