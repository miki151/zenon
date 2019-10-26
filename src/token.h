#pragma once

#include <deque>
#include "variant.h"
#include "code_loc.h"
#include "operator.h"

class Token;

struct Number {
  bool operator == (const Number&) const { return true; }
};

struct RealNumber {
  bool operator == (const RealNumber&) const { return true; }
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
  NULL_TOKEN,
  TEMPLATE,
  FOR,
  WHILE,
  IMPORT,
  EXPORT,
  CONST,
  ENUM,
  OPERATOR,
  CONCEPT,
  REQUIRES,
  COLON,
  MOVE,
  MUTABLE,
  BREAK,
  CONTINUE,
  VIRTUAL,
  DISCARD,
  ELLIPSIS,
  STATIC,
  COUNTOF
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

struct EofToken {
  bool operator == (const EofToken&) const { return true; }
};

extern Keyword getKeyword(const string&);
string getString(Token);
extern vector<string> getAllKeywords();
string process(Token, string matched);

class Token : public variant<Number, RealNumber, IdentifierToken, Keyword, Operator, EmbedToken, StringToken,
    CharToken, EofToken> {
  public:
  using variant::variant;
  bool operator == (const Token& t) {
    return variant::operator==(t);
  }
  string value;
  CodeLoc codeLoc;
};

class Tokens {
  public:
  Tokens(std::vector<Token>);
  Token peek() const;
  Token popNext();
  const Token& peekPrevious() const;
  bool empty() const;
  void rewind();
  using Bookmark = int;
  Bookmark getBookmark() const;
  void rewind(Bookmark);
  NODISCARD WithErrorLine<Token> eat(Token);
  template <typename TokenType>
  NODISCARD WithErrorLine<Token> eat(string error) {
    auto t = popNext();
    if (!t.contains<TokenType>())
      return t.codeLoc.getError(error);
    else
      return std::move(t);
  }
  optional<Token> eatMaybe(Token);
  int getSize() const;

  private:
  std::vector<Token> data;
  int index = 0;
};
