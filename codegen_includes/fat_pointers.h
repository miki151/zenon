#ifndef FAT_POINTERS_H
#define FAT_POINTERS_H

#include "any.hpp"

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
struct fat_value {
  nonstd::any object;
  VTable* vTable;
};

template <typename Object, typename VTable>
fat_value<VTable> make_fat_value(Object&& object, VTable* vTable) {
  return fat_value<VTable>{std::move(object), vTable};
}

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
fat_ptr<VTable> op_get_address(const fat_value<VTable>& r) {
  return fat_ptr<VTable>{r.object.get_ptr(), r.vTable};
}

template <typename VTable>
fat_ptr<VTable> op_get_address(fat_value<VTable>& r) {
  return fat_ptr<VTable>{r.object.get_ptr(), r.vTable};
}

template <typename VTable>
const_fat_ptr<VTable> make_const_fat_ptr(void const* object, VTable* vTable) {
  return const_fat_ptr<VTable>{object, vTable};
}

#endif
