#include "identifier.h"

IdentifierInfo::IdentifierInfo(string n) : parts(1, IdentifierPart{n, {}}) {
  CHECK(!n.empty());
}

IdentifierInfo::IdentifierInfo(IdentifierInfo::IdentifierPart part) : parts(1, part) {
}

/*IdentifierInfo::IdentifierInfo(vector<string> s, string n, vector<IdentifierInfo> t)
    : namespaces(s), templateParams(t), name(n) {
  CHECK(!name.empty());
}*/

IdentifierInfo IdentifierInfo::parseFrom(Tokens& tokens, bool allowPointer) {
  IdentifierInfo ret;
  while (1) {
    ret.parts.emplace_back();
    auto token = tokens.popNext("identifier");
    token.codeLoc.check(token.contains<IdentifierToken>(), "Expected identifier");
    ret.codeLoc = token.codeLoc;
    ret.parts.back().name = token.value;
    if (tokens.peek("identifier") == Operator::LESS_THAN) {
      tokens.popNext();
      bool firstParam = true;
      while (1) {
        auto templateParamToken = tokens.popNext("template parameter");
        if (firstParam) {
          auto nextToken = tokens.peek("Expression or template parameter");
          if (!templateParamToken.contains<IdentifierToken>() ||
              (nextToken != Keyword::COMMA && nextToken != Operator::LESS_THAN && nextToken != Operator::MORE_THAN &&
                  nextToken != Operator::MULTIPLY && nextToken != Keyword::MUTABLE)) {
            tokens.rewind();
            tokens.rewind();
            break;
          }
          firstParam = false;
        }
        if (templateParamToken == Operator::MORE_THAN)
          break;
        if (templateParamToken.contains<IdentifierToken>() || templateParamToken == Keyword::MUTABLE) {
          tokens.rewind();
          ret.parts.back().templateArguments.push_back(IdentifierInfo::parseFrom(tokens, true));
          if (tokens.peek("template parameter") != Operator::MORE_THAN)
            tokens.eat(Keyword::COMMA);
        } else
          templateParamToken.codeLoc.error("Couldn't parse template parameters");
      }
    }
    if (tokens.peek("something") == Keyword::NAMESPACE_ACCESS) {
      tokens.popNext();
      continue;
    } else
      break;
  }
  if (allowPointer) {
    if (auto t = tokens.eatMaybe(Keyword::MUTABLE)) {
      tokens.eat(Operator::MULTIPLY);
      ret.pointerType = MUTABLE;
    }
    else if (auto t = tokens.eatMaybe(Operator::MULTIPLY)) {
      ret.pointerType = CONST;
    }
  }
  INFO << "Identifier " << ret.toString();
  return ret;
}

IdentifierInfo IdentifierInfo::getWithoutFirstPart() const {
  IdentifierInfo ret(*this);
  ret.parts.clear();
  for (int i = 1; i < parts.size(); ++i)
    ret.parts.push_back(parts[i]);
  return ret;
}

optional<string> IdentifierInfo::asBasicIdentifier() const {
  if (parts.size() > 1 || !parts[0].templateArguments.empty() || pointerType)
    return none;
  else
    return parts[0].name;
}

string IdentifierInfo::toString() const {
  string ret;
  for (auto& part : parts) {
    if (!ret.empty())
      ret.append("::");
    ret.append(part.toString());
  }
  if (pointerType)
    switch (*pointerType) {
      case MUTABLE:
        ret.append("*");
        break;
      case CONST:
        ret = "const " + ret + "*";
        break;
    }
  return ret;
}

bool IdentifierInfo::operator ==(const IdentifierInfo& o) const {
  return parts == o.parts;
}

/*bool IdentifierInfo::operator ==(const IdentifierInfo& id) const {
  return name == id.name && namespaces == id.namespaces && templateParams == id.templateParams;
}*/

IdentifierInfo::IdentifierInfo() {

}

std::string IdentifierInfo::IdentifierPart::toString() const {
  string ret = name;
  if (!templateArguments.empty()) {
    auto paramsNames = transform(templateArguments, [](auto& e) { return e.toString(); });
    ret = ret + "<" + combine(paramsNames, ",") + ">";
  }
  return ret;
}

bool IdentifierInfo::IdentifierPart::operator == (const IdentifierPart& o) const {
  return name == o.name && templateArguments == o.templateArguments;
}
