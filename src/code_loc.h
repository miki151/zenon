#pragma once

#include "stdafx.h"
#include "util.h"

struct ErrorLoc;

struct CodeLoc {
  CodeLoc(string file, int l, int c);
  CodeLoc();
  NODISCARD ErrorLoc getError(string) const;
  CodeLoc plus(int numLines, int numColumns);
  string toString() const;
  bool operator < (const CodeLoc&) const;

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
      return WithErrorLine<T>(loc.getError(this->get_error()));
  }
};

template <typename T>
class [[nodiscard]] JustError : public expected<none_t, T> {
  public:
  using expected<none_t, T>::expected;
};

template <>
class [[nodiscard]] JustError<string> : public expected<none_t, string> {
  public:
  using expected<none_t, string>::expected;
  JustError<ErrorLoc> addCodeLoc(CodeLoc loc) const {
    if (*this)
      return **this;
    else
      return JustError<ErrorLoc>(loc.getError(this->get_error()));
  }
};

template <typename T>
class [[nodiscard]] JustResult : public optional<T> {
  public:
  using optional<T>::optional;
  WithError<T> addError(string s) const {
    if (*this)
      return **this;
    else
      return s;
  }
  WithErrorLine<T> addError(ErrorLoc l) const {
    if (*this)
      return **this;
    else
      return l;
  }
  auto get_error() const {
    CHECK(!*this);
    return none;
  }
};

using ErrorLocBuffer = vector<ErrorLoc>;
using ErrorBuffer = vector<string>;
void merge(ErrorLocBuffer&, const ErrorBuffer&, CodeLoc);
