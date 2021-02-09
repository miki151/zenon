#include "stdafx.h"
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
  {"embed_returns", Keyword::EMBED_RETURNS},
  {"union", Keyword::UNION},
  {"switch", Keyword::SWITCH},
  {"case", Keyword::CASE},
  {"default", Keyword::DEFAULT},
  {"template", Keyword::TEMPLATE},
  {"for", Keyword::FOR},
  {"while", Keyword::WHILE},
  {"import", Keyword::IMPORT},
  {"export", Keyword::EXPORT},
  {"const", Keyword::CONST},
  {"enum", Keyword::ENUM},
  {"operator", Keyword::OPERATOR},
  {"concept", Keyword::CONCEPT},
  {"requires", Keyword::REQUIRES},
  {"move", Keyword::MOVE},
  {"mutable", Keyword::MUTABLE},
  {"break", Keyword::BREAK},
  {"continue", Keyword::CONTINUE},
  {"virtual", Keyword::VIRTUAL},
  {"discard", Keyword::DISCARD},
  {"null", Keyword::NULL_TOKEN},
  {"static", Keyword::STATIC},
  {"countof", Keyword::COUNTOF},
  {"unchecked", Keyword::UNCHECKED},
  {"attribute", Keyword::ATTRIBUTE},
  {"try", Keyword::TRY},
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
  {".", Keyword::MEMBER_ACCESS},
  {"->", Keyword::ARROW_MEMBER_ACCESS},
  {"...", Keyword::ELLIPSIS}
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
      [](RealNumber) {
        return "real number";
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
    [](EmbedReturnsToken) {
      return "returning embed block";
    },
      [](StringToken) {
        return "string literal";
      },
      [](CharToken) {
        return "character";
      },
      [&t](EofToken) {
        return t.value.c_str();
      }
  );
}

Tokens::Tokens(vector<Token> d) : data(d) {}

const Token& Tokens::peek() const {
  return data[index];
}

const Token& Tokens::peekNext() const {
  return data[index + 1];
}

const Token& Tokens::popNext() {
  return data[index++];
}

const Token& Tokens::peekPrevious() const {
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

WithErrorLine<Token> Tokens::eat(Token t) {
  auto token = peek();
  if (!(token == t))
    return peek().codeLoc.getError("Expected "s + quote(getString(t)) + ", got " + quote(token.value));
  return popNext();
}

optional<Token> Tokens::eatMaybe(Token t) {
  auto token = peek();
  if (token == t) {
    popNext();
    return token;
  } else
    return none;
}

int Tokens::getSize() const {
  return data.size();
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
