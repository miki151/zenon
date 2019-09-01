#pragma once

#include <array>

template < class D, class... Types>
constexpr std::array<D, sizeof...(Types)> make_array(Types&&... t) {
  return {static_cast<D>(std::forward<Types>(t))... };
}
