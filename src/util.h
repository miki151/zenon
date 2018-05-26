#pragma once

#include "stdafx.h"
#include "expected.h"

extern string combine(const vector<string>& adj, const string& separator);
extern string quote(const string&);

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

template<class T>
vector<T*> extractRefs(const vector<unique_ptr<T>>& v) {
  vector<T*> ret;
  ret.reserve(v.size());
  for (auto& el : v)
    ret.push_back(el.get());
  return ret;
}

template <typename Container, typename V>
bool contains(const Container& v, const V& elem) {
  return std::find(v.begin(), v.end(), elem) != v.end();
}

extern vector<string> split(const string& s, const std::set<char>& delim);

extern string getParentPath(const string&);

template <typename Container>
struct ReverseRange {
  ReverseRange(const Container& c) : container(c) {}

  using iterator = typename Container::const_reverse_iterator;

  iterator begin() const {
    return container.crbegin();
  }

  iterator end() const {
    return container.crend();
  }

  const Container& container;
};

template <typename Container>
auto reverse(const Container& c) {
  return ReverseRange<Container>(c);
}

template <typename T>
void append(vector<T>& v, const vector<T>& w) {
  v.reserve(v.size() + w.size());
  for (T elem : w)
    v.push_back(elem);
}

template <typename T>
vector<T> concat(vector<T> v, const vector<T>& w) {
  append(v, w);
  return v;
}

template <typename T>
vector<T> getSubsequence(const vector<T>& v, int start, optional<int> lengthOption = none) {
  auto length = lengthOption.value_or(v.size() - start);
  CHECK(start >= 0 && length >= 0 && start + length <= v.size());
  vector<T> ret;
  ret.reserve(length);
  for (int i = start; i < start + length; ++i)
    ret.push_back(v[i]);
  return ret;
}

template <typename Key, typename Map>
optional<const typename Map::mapped_type&> getReferenceMaybe(const Map& m, const Key& key) {
  auto it = m.find(key);
  if (it != m.end())
    return it->second;
  else
    return none;
}

template <typename Key, typename Map>
optional<typename Map::mapped_type&> getReferenceMaybe(Map& m, const Key& key) {
  auto it = m.find(key);
  if (it != m.end())
    return it->second;
  else
    return none;
}

template <typename Key, typename Map>
optional<typename Map::mapped_type> getValueMaybe(const Map& m, const Key& key) {
  auto it = m.find(key);
  if (it != m.end())
    return it->second;
  else
    return none;
}

template <typename T>
optional<T> getOnlyElement(const vector<T>& v) {
  CHECK(v.size() <= 1);
  if (!v.empty())
    return v[0];
  else
    return none;
}

template <typename T>
T copyOf(const T& t) {
  return t;
}
