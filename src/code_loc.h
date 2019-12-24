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
  string toString() const;
};

inline std::ostream& operator<<(std::ostream& d, const ErrorLoc& error) {
  d << error.toString();
  return d;
}

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

struct success_t
{
  struct init{};
  constexpr explicit success_t(init){}
};
constexpr success_t success{success_t::init()};

template <typename T>
class [[nodiscard]] JustError : public expected<success_t, T> {
  public:
  using expected<success_t, T>::expected;
};

template <>
class [[nodiscard]] JustError<string> : public expected<success_t, string> {
  public:
  using expected<success_t, string>::expected;
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

struct EvalError {
  // The case when expression can't be evaluated at compile time
  static EvalError noEval();
  // Expression can be evaluated, but caused a compile error
  static EvalError withError(string error);
  bool canEval;
  string error;
};

template <typename T>
class [[nodiscard]] WithEvalError : public expected<T, EvalError> {
  public:
  using expected<T, EvalError>::expected;

  WithErrorLine<T> addNoEvalError(const ErrorLoc& error) const {
    return addNoEvalError(error.error).addCodeLoc(error.loc);
  }

  WithError<T> addNoEvalError(const string& error) const {
    if (*this)
      return **this;
    if (!this->get_error().canEval)
      return error;
    return this->get_error().error;
  }
};

using ErrorLocBuffer = vector<ErrorLoc>;
using ErrorBuffer = vector<string>;
void merge(ErrorLocBuffer&, const ErrorBuffer&, CodeLoc);
