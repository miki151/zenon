#pragma once

#include "stdafx.h"
#include "util.h"

struct ErrorLoc;

struct CodeLoc {
  CodeLoc(string file, int l, int c);
  CodeLoc();
  [[noreturn]] void error(const string&) const;
  void check(bool, const string&) const;
  void checkNoError(optional<string> error) const;
  ErrorLoc getError(string) const;
  CodeLoc plus(int numLines, int numColumns);

  string file;
  int line;
  int column;
};

struct ErrorLoc {
  CodeLoc loc;
  string error;
  [[noreturn]] void execute() const;
};

template <typename T>
class [[nodiscard]] WithErrorLine : public expected<T, ErrorLoc> {
  public:
  using expected<T, ErrorLoc>::expected;
  [[nodiscard]] T get() const {
    if (!(*this))
      this->get_error().execute();
    else
      return **this;
  }
  void unpack(optional<T>& value, optional<ErrorLoc>& error) {
    if (*this) {
      CHECK(!value);
      value = *this->get_value_maybe();
    }
    else if (error)
      error->error.append("\n" + this->get_error().error);
    else
      error = this->get_error();
  }
};

template <typename T>
class [[nodiscard]] WithError : public expected<T, string> {
  public:
  using expected<T, string>::expected;
  [[nodiscard]] T get(CodeLoc loc) const {
    if (!(*this))
      loc.error(this->get_error());
    else
      return **this;
  }
};
