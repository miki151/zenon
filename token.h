#pragma once

#include <deque>
#include "variant.h"
#include "code_loc.h"
#include "binary_operator.h"

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
  OPEN_BLOCK,
  CLOSE_BLOCK,
  SEMICOLON,
  COMMA,
  RETURN,
  TRUE,
  FALSE,
  STRUCT,
  EMBED,
  VARIANT,
  NAMESPACE_ACCESS
};

struct Unknown {
  bool operator == (const Unknown&) const { return true; }
};

extern Keyword getKeyword(const string&);
string getString(Token);
extern vector<string> getAllKeywords();

class Token : public variant<Number, IdentifierToken, Keyword, BinaryOperator, Unknown> {
  public:
  using variant::variant;
  string value;
  CodeLoc codeLoc;
};

class Tokens {
  public:
  Tokens(std::vector<Token>);
  const Token& peek(string expected = "") const;
  Token popNext(string expected = "");
  const Token& peekPrevious() const;
  bool empty() const;
  void rewind();
  void error(const string&) const;
  void check(bool, const string&) const;
  void eat(Token);

  private:
  std::vector<Token> data;
  int index = 0;
};
