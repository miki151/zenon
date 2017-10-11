#pragma once

#include <deque>
#include "variant.h"
#include "code_loc.h"
#include "binary_operator.h"

class Token;

struct Number {
  string value;
};

struct Identifier {
};

struct Keyword {
  enum Type {
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
  } type;
  Keyword(Type);
  static Token get(const string&);
  const char* getString() const;
  static vector<string> getAll();
};

struct Operator {
  BinaryOperator type;
};

class Token : public variant<Number, Identifier, Keyword, Operator> {
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
  void eat(Keyword keyword);

  private:
  std::vector<Token> data;
  int index = 0;
};
