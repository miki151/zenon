#include <unordered_map>
#include "token.h"

using namespace std;

static const unordered_map<string, Keyword::Type> keywords {
  {"if", Keyword::IF},
  {"else", Keyword::ELSE},
  {"return", Keyword::RETURN},
  {"true", Keyword::TRUE},
  {"false", Keyword::FALSE},
  {"struct", Keyword::STRUCT},
  {"(", Keyword::OPEN_BRACKET},
  {")", Keyword::CLOSE_BRACKET},
  {"{", Keyword::OPEN_BLOCK},
  {"}", Keyword::CLOSE_BLOCK},
  {";", Keyword::SEMICOLON},
  {",", Keyword::COMMA},
};

vector<string> Keyword::getAll() {
  vector<string> ret;
  for (auto& elem : keywords)
    ret.push_back(elem.first);
  return ret;
}

Keyword::Keyword(Keyword::Type t) : type(t) { }

Token Keyword::get(const string& s) {
  if (keywords.count(s))
    return Keyword{keywords.at(s)};
  else
    FATAL << "No keyword recognized: " << s;
  return {};
}

const char* Keyword::getString() const {
  for (auto& elem : keywords)
    if (elem.second == type)
      return elem.first.c_str();
  FATAL << "No keyword string found: " << type;
  return {};
}


Tokens::Tokens(std::vector<Token> d) : data(d) {}

const Token& Tokens::peek(string expected) const {
  if (!expected.empty()) {
    check(index < data.size(), "Expected \"" + expected + "\", got end-of-file.");
  } else
    CHECK(index < data.size());
  return data[index];
}

Token Tokens::popNext(string expected) {
  if (!expected.empty()) {
    check(index < data.size(), "Expected \"" + expected + "\", got end-of-file.");
  } else
    CHECK(index < data.size());
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

void Tokens::eat(BinaryOperator op) {
  auto expected = "Expected \""s + getString(op) + "\"";
  check(!empty(), expected + ", got EOF.");
  auto& token = peek();
  check(token == op, expected + ", got \"" + token.value + "\"");
  popNext();
}

void Tokens::eat(Keyword keyword) {
  auto expected = "Expected \""s + keyword.getString() + "\"";
  check(!empty(), expected + ", got EOF.");
  auto& token = peek();
  check(token == keyword, expected + ", got \"" + token.value + "\"");
  popNext();
}
