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

IdentifierInfo IdentifierInfo::parseFrom(Tokens& tokens) {
  IdentifierInfo ret;
  while (1) {
    ret.parts.emplace_back();
    auto token = tokens.popNext("identifier");
    ret.codeLoc = token.codeLoc;
    ret.parts.back().name = token.value;
    if (tokens.peek("identifier") == Operator::LESS_THAN) {
      tokens.popNext();
      while (1) {
        auto templateParamToken = tokens.popNext("template parameter");
        if (templateParamToken == Operator::MORE_THAN)
          break;
        if (templateParamToken.contains<IdentifierToken>()) {
          tokens.rewind();
          ret.parts.back().templateArguments.push_back(IdentifierInfo::parseFrom(tokens));
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
  if (tokens.eatMaybe(Keyword::REFERENCE))
    ret.reference = true;
  INFO << "Identifier " << ret.toString();
  return ret;
}

string IdentifierInfo::toString() const {
  string ret;
  for (auto& part : parts) {
    if (!ret.empty())
      ret.append("::");
    ret.append(part.toString());
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
