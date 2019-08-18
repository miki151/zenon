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

template <typename T>
T* op_get_address(T& t) {
  return &t;
}

#endif
