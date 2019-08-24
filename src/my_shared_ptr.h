#pragma once

#include <memory>
#include "optional.h"

template <typename T>
class shared_ptr : public std::shared_ptr<T> {
  public:
  using std::shared_ptr<T>::shared_ptr;
  shared_ptr() = delete;
  shared_ptr(std::nullptr_t) = delete;
  shared_ptr(std::shared_ptr<T> p) : std::shared_ptr<T>(p) {}
  shared_ptr& operator = (std::nullptr_t) = delete;
  void reset() = delete;
  template <typename U>
  shared_ptr<U> dynamicCast() const {
    return shared_ptr<U>(std::dynamic_pointer_cast<U>(*this));
  }
  bool operator == (const T* o) const {
    return this->get() == o;
  }
  bool operator != (const T* o) const {
    return !(*this == o);
  }
};

template <typename T>
class weak_ptr : public std::weak_ptr<T> {
  public:
  using std::weak_ptr<T>::weak_ptr;
  shared_ptr<T> get() {
    return shared_ptr<T>(std::weak_ptr<T>::lock());
  }
};

template <typename T, typename... Args>
shared_ptr<T> shared(Args&&...args) {
  auto ret = shared_ptr<T>(new T(std::forward<Args>(args)...));
  ret->weakPointer = ret;
  return ret;
}

template <typename T>
class nullable {
  public:
  using Param = typename T::element_type;
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
  nullable& operator = (std::nullptr_t) {
    elem = none;
    return *this;
  }
  explicit operator bool () const {
    return !!elem;
  }
  bool operator == (const T& o) const {
    return elem == o;
  }
  bool operator == (const nullable<T>& o) const {
    return elem == o.elem;
  }
  bool operator < (const nullable<T>& o) const {
    return elem < o.elem;
  }
  bool operator != (const T& o) const {
    return elem != o;
  }
  bool operator == (const Param* o) const {
    return (!o && !elem) || (*elem == o);
  }
  bool operator != (const Param* o) const {
    return !(*this == o);
  }
  T get() const {
    return *elem;
  }
  T value_or(T t) const {
    return elem.value_or(t);
  }
  Param& operator*() const {
    return **elem;
  }
  Param* operator->() const {
    return elem->get();
  }
  private:
  optional<T> elem;
};

template <typename T>
class owned_object {
  public:
  weak_ptr<T> get_this() {
    CHECK(!!weakPointer.lock());
    return weakPointer;
  }

  weak_ptr<T> get_this() const {
    CHECK(!!weakPointer.lock());
    return weakPointer;
  }

  weak_ptr<T> weakPointer;
};
