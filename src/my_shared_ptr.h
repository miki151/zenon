#pragma once

#include <memory>
#include "optional.h"

template <typename T>
class shared_ptr : public std::shared_ptr<T> {
  public:
  using std::shared_ptr<T>::shared_ptr;
  shared_ptr() = delete;
  shared_ptr(std::nullptr_t) = delete;
  shared_ptr& operator = (std::nullptr_t) = delete;
  void reset() = delete;
};

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args&&...args) {
  return shared_ptr<T>(new T(std::forward<Args...>(args)...));
}

template <typename T>
class nullable {
  public:
  nullable(std::nullptr_t) {}
  nullable() {}
  //nullable(T&& t) : elem(std::move(t)) {}
  nullable(T t) : elem(std::move(t)) {}
  /*nullable& operator = (T&& t) {
    elem = std::move(t);
    return *this;
  }*/
  nullable& operator = (T t) {
    elem = std::move(t);
    return *this;
  }
  explicit operator bool () const {
    return !!elem;
  }
  bool operator == (const T& o) const {
    return elem == o;
  }
  bool operator != (const T& o) const {
    return elem != o;
  }
  T get() const {
    return *elem;
  }
  using Param = typename T::element_type;
  Param& operator*() const {
    return **elem;
  }
  Param* operator->() const {
    return elem->get();
  }
  private:
  optional<T> elem;
};
