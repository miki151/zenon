#pragma once

#include <type_traits>
#include <new>

template<typename T>
struct destroy_helper {
  static void destroy(T& ptr) { ptr.~T(); };
};

template<typename T>
struct MoveConstructorVisitor {
  MoveConstructorVisitor(T& myElem) : myElem(myElem) {}
  void operator()(T& hisElem) {
    new (&myElem) T(std::move(hisElem));
  }
  template <typename U>
  void operator()(const U&) {}

  T& myElem;
};

template<typename T>
struct MoveAssignmentVisitor {
  MoveAssignmentVisitor(T& myElem) : myElem(myElem) {}
  void operator()(T& hisElem) {
    myElem = std::move(hisElem);
  }
  template <typename U>
  void operator()(const U&) {}

  T& myElem;
};

template <typename T>
struct VariantHelper {
   static void assign(T&& from, T& to) {
    if (from.unionElem == to.unionElem) {
      to.visit([&](auto& myElem) {
        from.visit(MoveAssignmentVisitor<std::remove_reference_t<decltype(myElem)>>{myElem});
      });
    } else {
      to.unionElem = from.unionElem;
      to.visit([&](auto& myElem) {
        from.visit(MoveConstructorVisitor<std::remove_reference_t<decltype(myElem)>>{myElem});
      });
    }
  }
  static void move(T&& from, T& to) {
    to.unionElem = from.unionElem;
    to.visit([&](auto& myElem) {
      from.visit(MoveConstructorVisitor<std::remove_reference_t<decltype(myElem)>>{myElem});
    });
  }
  static void destroy(T& t) {
    t.visit([&](auto& elem) {
      destroy_helper<typename std::remove_reference<decltype(elem)>::type>::destroy(elem);
    });
  }
};

