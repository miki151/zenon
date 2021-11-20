#pragma once

#include "stdafx.h"
#include "expected.h"

extern string combine(const vector<string>& adj, const string& separator);
extern string quote(const string&);
bool endsWith(const string&, const string& suffix);
bool startsWith(const string&, const string& prefix);
bool contains(const string&, const string& substring, unsigned long index);

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

template <typename Container, typename T>
optional<int> findElement(const Container& v, const T& element) {
  for (int i = 0; i < v.size(); ++i)
    if (v[i] == element)
      return i;
  return none;
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
void append(vector<T>& v, vector<T>&& w) {
  v.reserve(v.size() + w.size());
  for (T& elem : w)
    v.push_back(std::move(elem));
}

template <typename T>
vector<T> concat(vector<T> v, vector<T>&& w) {
  append(v, std::move(w));
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
const typename Map::mapped_type* getReferenceMaybe(const Map& m, const Key& key) {
  auto it = m.find(key);
  if (it != m.end())
    return &it->second;
  else
    return nullptr;
}

template <typename Key, typename Map>
typename Map::mapped_type* getReferenceMaybe(Map& m, const Key& key) {
  auto it = m.find(key);
  if (it != m.end())
    return &it->second;
  else
    return nullptr;
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

template <typename T>
vector<T*> extractPtrs(const vector<unique_ptr<T>>& v) {
  return transform(v, [](const auto& elem) { return elem.get(); });
}

template <typename T>
void emplaceBack(vector<T>&) {}

template <typename T, typename First, typename... Args>
void emplaceBack(vector<T>& v, First&& first, Args&&... args) {
  v.emplace_back(std::move(std::forward<First>(first)));
  emplaceBack(v, std::forward<Args>(args)...);
}

template <typename T, typename... Args>
vector<T> makeVec(T&& f, Args&&... args) {
  vector<T> ret;
  ret.reserve(sizeof...(Args) + 1);
  ret.push_back(std::forward<T>(f));
  emplaceBack(ret, std::forward<Args>(args)...);
  return ret;
}

template <typename T, typename U>
unique_ptr<T> cast(unique_ptr<U> p) {
  return unique_ptr<T>((T*)p.release());
}

template <typename T>
void ignore(T) {}

#define COMPARABLE(Type, ...)\
auto asTuple() const {\
  return std::forward_as_tuple(__VA_ARGS__);\
}\
bool operator == (const Type& o) const {\
  return asTuple() == o.asTuple();\
}\
bool operator < (const Type& o) const {\
  return asTuple() < o.asTuple();\
}

class OnExit {
  public:
  OnExit(function<void()> f) : fun(f) {}

  ~OnExit() { fun(); }

  private:
  function<void()> fun;
};

namespace std {
namespace {
  template <class T>
  inline void hash_combine(std::size_t& seed, T const& v) {
    seed ^= std::hash<T>()(v) + 0x9e3779b9 + (seed<<6) + (seed>>2);
  }

  template <class Tuple, size_t Index = std::tuple_size<Tuple>::value - 1>
  struct HashValueImpl {
    static void apply(size_t& seed, Tuple const& tuple) {
      HashValueImpl<Tuple, Index-1>::apply(seed, tuple);
      hash_combine(seed, std::get<Index>(tuple));
    }
  };

  template <class Tuple>
  struct HashValueImpl<Tuple,0> {
    static void apply(size_t& seed, Tuple const& tuple) {
      hash_combine(seed, std::get<0>(tuple));
    }
  };
}

template <typename ... TT>
struct hash<std::tuple<TT...>> {
  size_t operator()(std::tuple<TT...> const& tt) const {
    size_t seed = 0;
    HashValueImpl<std::tuple<TT...> >::apply(seed, tt);
    return seed;
  }
};
}
