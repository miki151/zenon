#ifndef ALL_H
#define ALL_H

#include <optional>
#include "variant_helpers.h"
#include "array_utils.h"
#include "lite_str.h"
#include "stacktrace.h"
#include "fat_pointers.h"
using zenon_string = lite_str<>;
constexpr auto null = std::nullopt;
using null_type = std::nullopt_t;

template <typename T>
const T* op_get_address(const T& t) {
  return &t;
}

template <typename T, typename Fun>
struct temporary_holder {
  temporary_holder(const T& t, Fun f) : t(t), f(std::move(f)) {
  }

  const T& operator*() const {
    return t;
  }

  ~temporary_holder() {
    f(&t);
  }

  const T& t;
  Fun f;
};

template <typename T, typename Fun>
temporary_holder<T, Fun> get_temporary_holder(const T& t, Fun f) {
  return temporary_holder<T, Fun>(t, std::move(f));
}

template <typename T>
T* op_get_address(T& t) {
  return &t;
}

template <typename Fun>
struct DeferDestruct {
  ~DeferDestruct() {
    if (!wasMoved)
      f();
  }
  Fun f;
  bool wasMoved = false;
};

template <typename Fun>
auto deferDestruct(Fun f) {
  return DeferDestruct<Fun>{std::move(f)};
}

template<typename T>
typename std::remove_reference<T>::type&& moveAndSetMoved(T&& t, bool* wasMoved) noexcept {
  *wasMoved = true;
  return static_cast<typename std::remove_reference<T>::type&&>(t);
}

template<typename T, typename GetMember, typename Destruct>
auto moveAndGetMember(T t, GetMember getMember, Destruct destruct) noexcept {
  destruct(&t);
  return getMember(std::move(t));
}

template <typename T>
void set_moved_in_embed(const T&) {}

template <typename T>
struct slice_t {
  T const* begin;
  T const* end;
};

struct void_t
{
  struct init{};
  constexpr explicit void_t(init){}
};
constexpr void_t void_value{void_t::init()};

#endif
