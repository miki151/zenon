#pragma once

#include "stdafx.h"
#include "util.h"

struct ErrorLoc;

struct CodeLoc {
  CodeLoc(string file, int l, int c);
  CodeLoc();
  ErrorLoc getError(string) const;
  CodeLoc plus(int numLines, int numColumns);
  string toString() const;

  string file;
  int line;
  int column;
};

struct ErrorLoc {
  CodeLoc loc;
  string error;
};

template <typename T>
class [[nodiscard]] WithErrorLine : public expected<T, ErrorLoc> {
  public:
  using expected<T, ErrorLoc>::expected;
  [[nodiscard]] T& get() {
    if (!*this)
      FATAL << "Getting element failed: " << this->get_error().loc.toString() << " " << this->get_error().error;
    return **this;
  }
  [[nodiscard]] const T& get() const {
    if (!*this)
      FATAL << "Getting element failed: " << this->get_error().loc.toString() << " " << this->get_error().error;
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
  void unpack(nullable<T>& value, optional<ErrorLoc>& error) {
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

  WithErrorLine<T> addCodeLoc(CodeLoc loc) const {
    if (*this)
      return **this;
    else
      return WithErrorLine<T>(ErrorLoc{loc, this->get_error()});
  }
};

using ErrorBuffer = vector<ErrorLoc>;
