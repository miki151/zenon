#pragma once

#include <type_traits>

template<typename T>
struct destroy_helper { static void destroy(T& ptr) { ptr.~T(); }; };

template<typename T>
struct CopyVisitor {
  CopyVisitor(T& myElem) : myElem(myElem) {}
  void operator()(const T& hisElem) {
    myElem = hisElem;
  }
  template <typename U>
  void operator()(const U&) {}

  T& myElem;
};

template <typename T>
struct VariantHelper {
  static void copy(const T& from, T& to) {
    to.unionElem = from.unionElem;
    to.visit([&](auto& myElem) {
      from.visit(CopyVisitor<std::remove_reference_t<decltype(myElem)>>{myElem});
    });
  }
  static void destroy(T& t) {
    t.visit([&](auto& elem) {
      destroy_helper<typename std::remove_reference<decltype(elem)>::type>::destroy(elem);
    });
  }
};
