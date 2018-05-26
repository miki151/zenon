#include <unordered_map>
#include "token.h"
#include "operator.h"
#include "util.h"

static const unordered_map<string, Keyword> keywords {
  {"if", Keyword::IF},
  {"else", Keyword::ELSE},
  {"return", Keyword::RETURN},
  {"true", Keyword::TRUE},
  {"false", Keyword::FALSE},
  {"struct", Keyword::STRUCT},
  {"extern", Keyword::EXTERN},
  {"embed", Keyword::EMBED},
  {"variant", Keyword::VARIANT},
  {"switch", Keyword::SWITCH},
  {"case", Keyword::CASE},
  {"default", Keyword::DEFAULT},
  {"template", Keyword::TEMPLATE},
  {"for", Keyword::FOR},
  {"while", Keyword::WHILE},
  {"import", Keyword::IMPORT},
  {"public", Keyword::PUBLIC},
  {"const", Keyword::CONST},
  {"enum", Keyword::ENUM},
  {"operator", Keyword::OPERATOR},
  {"concept", Keyword::CONCEPT},
  {"requires", Keyword::REQUIRES},
  {"move", Keyword::MOVE},
  {"mutable", Keyword::MUTABLE},
  {"?", Keyword::MAYBE},
  {"::", Keyword::NAMESPACE_ACCESS},
  {"(", Keyword::OPEN_BRACKET},
  {")", Keyword::CLOSE_BRACKET},
  {"[", Keyword::OPEN_SQUARE_BRACKET},
  {"]", Keyword::CLOSE_SQUARE_BRACKET},
  {"{", Keyword::OPEN_BLOCK},
  {"}", Keyword::CLOSE_BLOCK},
  {";", Keyword::SEMICOLON},
  {":", Keyword::COLON},
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
      [](Operator op) {
        return getString(op);
      },
      [](EmbedToken) {
        return "embed block";
      },
      [](StringToken) {
        return "string literal";
      },
      [](CharToken) {
        return "character";
      }
  );
}

Tokens::Tokens(std::vector<Token> d) : data(d) {}

Token Tokens::peek(string expected) const {
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

Tokens::Bookmark Tokens::getBookmark() const {
  return index;
}

void Tokens::rewind(Tokens::Bookmark b){
  index = b;
}

void Tokens::error(const string& e) const {
  auto& lastToken = empty() ? data[index - 1] : data[index];
  lastToken.codeLoc.error(e);
}

void Tokens::check(bool b, const string& e) const {
  if (!b)
    error(e);
}

Token Tokens::eat(Token t) {
  auto expected = "Expected "s + quote(getString(t));
  auto token = peek(expected + ", got EOF.");
  check(token == t, expected + ", got " + quote(token.value));
  return popNext();
}

optional<Token> Tokens::eatMaybe(Token t) {
  auto token = peek("Expected "s + quote(getString(t)) + ", got EOF.");
  if (token == t) {
    popNext();
    return token;
  } else
    return none;
}

string process(Token t, string matched) {
  return t.visit(
      [&](StringToken) {
        CHECK(matched.size() >= 2 && matched.front() == '\"' && matched.back() == '\"')
            << "Bad string literal " << quote(matched);
        return matched.substr(1, matched.size() - 2);
      },
      [&](CharToken) {
        CHECK(matched.size() >= 2 && matched.front() == '\'' && matched.back() == '\'')
            << "Bad char literal " << quote(matched);
        return matched.substr(1, matched.size() - 2);
      },
      [&](const auto&) {
        return matched;
      }
  );
}
