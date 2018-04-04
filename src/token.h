#pragma once

#include <deque>
#include "variant.h"
#include "code_loc.h"
#include "operator.h"

class Token;

struct Number {
  bool operator == (const Number&) const { return true; }
};

struct IdentifierToken {
  bool operator == (const IdentifierToken&) const { return true; }
};

enum class Keyword {
  IF,
  ELSE,
  OPEN_BRACKET,
  CLOSE_BRACKET,
  OPEN_SQUARE_BRACKET,
  CLOSE_SQUARE_BRACKET,
  OPEN_BLOCK,
  CLOSE_BLOCK,
  SEMICOLON,
  COMMA,
  RETURN,
  TRUE,
  FALSE,
  STRUCT,
  EXTERN,
  EMBED,
  VARIANT,
  NAMESPACE_ACCESS,
  SWITCH,
  CASE,
  DEFAULT,
  MAYBE,
  TEMPLATE,
  FOR,
  IMPORT,
  PUBLIC,
  AUTO,
  ENUM
};

struct EmbedToken {
  bool operator == (const EmbedToken&) const { return true; }
};

struct StringToken {
  bool operator == (const StringToken&) const { return true; }
};

struct CharToken {
  bool operator == (const CharToken&) const { return true; }
};

extern Keyword getKeyword(const string&);
string getString(Token);
extern vector<string> getAllKeywords();
string process(Token, string matched);

class Token : public variant<Number, IdentifierToken, Keyword, Operator, EmbedToken, StringToken, CharToken> {
  public:
  using variant::variant;
  string value;
  CodeLoc codeLoc;
};

class Tokens {
  public:
  Tokens(std::vector<Token>);
  Token peek(string expected = "") const;
  Token popNext(string expected = "");
  const Token& peekPrevious() const;
  bool empty() const;
  void rewind();
  using Bookmark = int;
  Bookmark getBookmark() const;
  void rewind(Bookmark);
  void error(const string&) const;
  void check(bool, const string&) const;
  Token eat(Token);
  optional<Token> eatMaybe(Token);

  private:
  std::vector<Token> data;
  int index = 0;
};
