#pragma once

#include <memory>

template <typename T>
class shared_ptr : public std::shared_ptr<T> {
  public:
  using std::shared_ptr<T>::shared_ptr;
  shared_ptr(std::shared_ptr<T> p) : std::shared_ptr<T>(p) {}
  
  template <typename U>
  shared_ptr<U> dynamicCast() const {
    return shared_ptr<U>(std::dynamic_pointer_cast<U>(*this));
  }

  using NoConst = typename std::remove_const<T>::type;

  auto removeConst() const {
    return std::const_pointer_cast<NoConst>(*this);
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
