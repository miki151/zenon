#include <unordered_map>
#include "token.h"
#include "binary_operator.h"
#include "util.h"

using namespace std;

static const unordered_map<string, Keyword> keywords {
  {"if", Keyword::IF},
  {"else", Keyword::ELSE},
  {"return", Keyword::RETURN},
  {"true", Keyword::TRUE},
  {"false", Keyword::FALSE},
  {"struct", Keyword::STRUCT},
  {"embed", Keyword::EMBED},
  {"variant", Keyword::VARIANT},
  {"::", Keyword::NAMESPACE_ACCESS},
  {"(", Keyword::OPEN_BRACKET},
  {")", Keyword::CLOSE_BRACKET},
  {"{", Keyword::OPEN_BLOCK},
  {"}", Keyword::CLOSE_BLOCK},
  {";", Keyword::SEMICOLON},
  {",", Keyword::COMMA},
};

vector<string> getAllKeywords() {
  vector<string> ret;
  for (auto& elem : keywords)
    ret.push_back(elem.first);
  return ret;
}

Keyword getKeyword(const string& s) {
  if (keywords.count(s))
    return Keyword{keywords.at(s)};
  else
    FATAL << "No keyword recognized: " << s;
  return {};
}

string getString(Token t) {
  return t.visit(
      [](Keyword k) {
        for (auto& elem : keywords)
          if (elem.second == k)
            return elem.first.c_str();
        FATAL << "No keyword string found: " << (int)k;
        return "";
      },
      [](Number) {
        return "number";
      },
      [](const IdentifierToken&) {
        return "identifier";
      },
      [](BinaryOperator op) {
        return getString(op);
      },
      [](Unknown) {
        return "unknown";
      }
  );
}

Tokens::Tokens(std::vector<Token> d) : data(d) {}

const Token& Tokens::peek(string expected) const {
  if (!expected.empty()) {
    check(index < data.size(), "Expected " + quote(expected) + ", got end-of-file.");
  } else
    CHECK(index < data.size());
  return data[index];
}

Token Tokens::popNext(string expected) {
  if (!expected.empty()) {
    check(index < data.size(), "Expected " + quote(expected) + ", got end-of-file.");
  } else
    CHECK(index < data.size());
  //INFO << "Popping token " << getString(data[index]);
  return data[index++];
}

const Token& Tokens::peekPrevious() const {
  CHECK(index > 0);
  return data[index - 1];
}

bool Tokens::empty() const {
  return index >= data.size();
}

void Tokens::rewind() {
  CHECK(index > 0);
  --index;
}

void Tokens::error(const string& e) const {
  auto& lastToken = empty() ? data[index - 1] : data[index];
  lastToken.codeLoc.error(e);
}

void Tokens::check(bool b, const string& e) const {
  if (!b)
    error(e);
}

void Tokens::eat(Token t) {
  auto expected = "Expected "s + quote(getString(t));
  check(!empty(), expected + ", got EOF.");
  auto& token = peek();
  check(token == t, expected + ", got " + quote(token.value));
  popNext();
}
