#ifndef ALL_H
#define ALL_H

#include <optional>
#include "variant_helpers.h"
#include "array_utils.h"
#include "lite_str.h"
#include "stacktrace.h"
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

template <typename VTable>
struct const_fat_ref {
  void const* object;
  VTable* vTable;
};

template <typename VTable>
struct const_fat_ptr {
  void const* object;
  VTable* vTable;
  const_fat_ref<VTable> operator*() const {
    return const_fat_ref<VTable>{object, vTable};
  }
};

template <typename VTable>
struct fat_ref {
  void* object;
  VTable* vTable;
};

template <typename VTable>
struct fat_ptr {
  void* object;
  VTable* vTable;
  fat_ref<VTable> operator*() const {
    return fat_ref<VTable>{object, vTable};
  }
  operator const_fat_ptr<VTable>() {
    return const_fat_ptr<VTable>{object, vTable};   
  }
};

template <typename VTable>
fat_ptr<VTable> make_fat_ptr(void* object, VTable* vTable) {
  return fat_ptr<VTable>{object, vTable};
}

template <typename VTable>
const_fat_ptr<VTable> op_get_address(const_fat_ref<VTable> r) {
  return const_fat_ptr<VTable>{r.object, r.vTable};
}

template <typename VTable>
fat_ptr<VTable> op_get_address(fat_ref<VTable> r) {
  return fat_ptr<VTable>{r.object, r.vTable};
}

template <typename VTable>
const_fat_ptr<VTable> make_const_fat_ptr(void const* object, VTable* vTable) {
  return const_fat_ptr<VTable>{object, vTable};
}

template <typename T>
void set_moved_in_embed(const T&) {}

template <typename T>
struct slice_t {
  T const* begin;
  T const* end;
};

#endif
