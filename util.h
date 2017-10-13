#pragma once

#include "stdafx.h"

string combine(const vector<string>& adj);
string quote(const string&);

template <class T>
class HeapAllocated {
  public:
  template <typename... Args>
  HeapAllocated(Args... a) : elem(new T(a...)) {}

  HeapAllocated(T&& o) : elem(new T(std::move(o))) {}

  HeapAllocated(const HeapAllocated& o) : elem(new T(*o)) {}

  T* operator -> () {
    return elem.get();
  }

  const T* operator -> () const {
    return elem.get();
  }

  T& operator * () {
    return *elem.get();
  }

  const T& operator * () const {
    return *elem.get();
  }

  const T* get() const {
    return elem.get();
  }

  T* get() {
    return elem.get();
  }

  void reset(T&& t) {
    elem.reset(new T(std::move(t)));
  }

  bool operator == (const HeapAllocated& t) const {
    return *elem == *t.elem;
  }

  HeapAllocated& operator = (const HeapAllocated& t) {
    *elem.get() = *t;
    return *this;
  }

  HeapAllocated& operator = (HeapAllocated&& t) {
    elem = std::move(t.elem);
    return *this;
  }

  private:
  unique_ptr<T> elem;
};

template <typename T, typename Fun>
auto transform(const vector<T>& v, Fun fun) {
  vector<decltype(fun(*v.begin()))> ret;
  ret.reserve(v.size());
  for (const auto& elem : v)
    ret.push_back(fun(elem));
  return ret;
}
