#pragma once

#include "stdafx.h"

template <typename T, typename Error>
class expected {
  public:
  expected(Error error) : elem(std::move(error)) {}
  expected(T value) : elem(std::move(value)) {}
  expected(optional<T> value, Error error) {
    if (value)
      elem = std::move(*value);
    else
      elem = error;
  }

  explicit operator bool () const {
    return elem.index() == 1;
  }

  void unpack(optional<T>& value, Error& error) {
    if (*this)
      value = *elem.template getValueMaybe<T>();
    else
      error = *elem.template getValueMaybe<Error>();
  }

  void unpack(nullable<T>& value, Error& error) {
    if (*this)
      value = *elem.template getValueMaybe<T>();
    else
      error = *elem.template getValueMaybe<Error>();
  }

  optional<const T&> get_reference_maybe() const {
    return elem.template getReferenceMaybe<T>();
  }

  optional<T&> get_reference_maybe() {
    return elem.template getReferenceMaybe<T>();
  }

  optional<T> get_value_maybe() const {
    return elem.template getValueMaybe<T>();
  }

  const T& operator * () const {
    return elem.template get<T>();
  }

  T& operator * () {
    return elem.template get<T>();
  }
  
  const T& get_value() const {
    return *elem.template getReferenceMaybe<T>();
  }

  T& get_value() {
    return *elem.template getReferenceMaybe<T>();
  }

  const T* operator -> () const {
    return &*elem.template getReferenceMaybe<T>();
  }

  T* operator -> () {
    return &*elem.template getReferenceMaybe<T>();
  }

  const Error& get_error() const {
    return *elem.template getReferenceMaybe<Error>();
  }

  private:
  variant<Error, T> elem;
};
