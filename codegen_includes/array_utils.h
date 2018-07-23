#pragma once

#include <array>

namespace details {
  template<class> struct is_ref_wrapper : std::false_type {};
  template<class T> struct is_ref_wrapper<std::reference_wrapper<T>> : std::true_type {};
 
 
  template <class D, class...> struct return_type_helper { using type = D; };
  template <class... Types>
  struct return_type_helper<void, Types...> : std::common_type<Types...> {
  };
 
  template <class D, class... Types>
  using return_type = std::array<typename return_type_helper<D, Types...>::type,
                                 sizeof...(Types)>;
}
 
template < class D = void, class... Types>
constexpr details::return_type<D, Types...> make_array(Types&&... t) {
  return {std::forward<Types>(t)... };
}
