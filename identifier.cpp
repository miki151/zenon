#include "identifier.h"

IdentifierInfo::IdentifierInfo(string n) : name(n) {}

IdentifierInfo::IdentifierInfo(vector<string> s, string n) : namespaces(s), name(n) {
}

IdentifierInfo IdentifierInfo::parseFrom(Token& idToken, Tokens& tokens) {
  IdentifierInfo ret;
  while (tokens.peek("something") == Keyword::NAMESPACE_ACCESS) {
    ret.namespaces.push_back(idToken.value);
    tokens.popNext();
    idToken = tokens.popNext("identifier");
  }
  ret.name = idToken.value;
  //tokens.popNext("something");
  return ret;
}

string IdentifierInfo::toString() const {
  if (namespaces.empty())
    return name;
  else
    return combine(namespaces, "::") + "::" + name;
}

bool IdentifierInfo::operator ==(const IdentifierInfo& id) const {
  return name == id.name && namespaces == id.namespaces;
}

IdentifierInfo::IdentifierInfo() {

}
