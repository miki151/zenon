/*
MIT License

Copyright (c) 2018 Michal Brzozowski

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#pragma once

#include <cstring>
#include <utility>
#include <cstdint>
#include <cctype>

namespace detail {
  template<typename char_type>
  struct lite_str_allocator {
    static char_type* allocate(size_t size) {
      return new char_type[size];
    }
    static void deallocate(const char_type* ptr) {
      delete [] ptr;
    }
  };

  struct char_char_traits {
    static size_t get_length(const char* s) {
      return strlen(s);
    }
  };
}

template <typename char_type, typename char_traits, typename allocator_t = detail::lite_str_allocator<char_type>>
class basic_lite_str {
  const char_type *ptr;
  typedef uint32_t length_t;
  length_t length;
  enum Type {
    OWNED,
    REFERENCE
  } type;

  // TODO: figure out if using an atomic counter is enough to support copying strings between threads.
  typedef uint32_t ref_counter_t;

  ref_counter_t& get_ref_counter() const {
    // the reference counter is at the end of the buffer, after the '\0'.
    return *((ref_counter_t*)(ptr + length + 1));
  }

  void destruct() {
    if (type == OWNED && --get_ref_counter() == 0) {
      allocator_t::deallocate(ptr);
    }
  }

  public:

  basic_lite_str() : ptr(""), length(0), type(REFERENCE) {}

  static basic_lite_str reference(const char_type* s = "") {
    basic_lite_str ret;
    ret.ptr = s;
    ret.length = char_traits::get_length(s);
    return ret;
  }

  static basic_lite_str owned(const char_type* s) {
    basic_lite_str ret;
    auto buf = ret.create_buffer(char_traits::get_length(s));
    strcpy(buf, s);
    return ret;
  }

  basic_lite_str(const basic_lite_str& other) {
    copy_from(other);
  }

  basic_lite_str(basic_lite_str&& other) {
    move_from(std::move(other));
  }

  basic_lite_str& operator = (const basic_lite_str& other) {
    destruct();
    copy_from(other);
    return *this;
  }

  basic_lite_str& operator = (basic_lite_str&& other) {
    destruct();
    move_from(std::move(other));
    return *this;
  }

  void move_from(basic_lite_str&& other) {
    ptr = other.ptr;
    type = other.type;
    length = other.length;
    // This stops o from decreasing the ref counter if there is one
    other.type = REFERENCE;
  }

  void copy_from(const basic_lite_str& other) {
    ptr = other.ptr;
    type = other.type;
    length = other.length;
    if (type == OWNED)
      ++get_ref_counter();
  }

  // creates a new allocated string and returns the non-const buffer so the user can fill it in.
  char_type* create_buffer(length_t l) {
    destruct();
    length = l;
    auto alloc_info_size = (sizeof(ref_counter_t) - 1) / sizeof(char_type) + 1;
    // Allocate enough for the string, the '\0' and the ref count at the end.
    auto buf = allocator_t::allocate(length + 1 + alloc_info_size);
    buf[length] = '\0';
    ptr = buf;
    get_ref_counter() = 1;
    type = OWNED;
    return buf;
  }

  ~basic_lite_str() {
    destruct();
  }

  char_type operator [](int ind) const {
    return ptr[ind];
  }

  length_t size() const {
    return length;
  }

  bool empty() const {
    return length == 0;
  }

  const char_type* data() const {
    return ptr;
  }

  basic_lite_str substring(int index, int length) const {
    basic_lite_str<char_type, char_traits, allocator_t> ret;
    auto buf = ret.create_buffer(length);
    memcpy(buf, ptr + index, length * sizeof(char_type));
    return ret;
  }
};

template <typename allocator = detail::lite_str_allocator<char>>
using lite_str = basic_lite_str<char, detail::char_char_traits, allocator>;

template <typename stream, typename char_type, typename char_traits, typename alloc>
stream& operator << (stream& os, const basic_lite_str<char_type, char_traits, alloc>& s) {
  os << s.data();
  return os;
}

template <typename stream, typename char_type, typename char_traits, typename alloc>
stream& operator >> (stream& os, basic_lite_str<char_type, char_traits, alloc>& s) {
  s = basic_lite_str<char_type, char_traits, alloc>();
  while (1) {
    char buf[2] = {0};
    os.get(buf[0]);
    if (isspace(buf[0]) || !os.good())
      break;
    else
      s = s + buf;
  }
  return os;
}

template <typename char_type, typename char_traits, typename alloc>
static basic_lite_str<char_type, char_traits, alloc> concatenate(const char_type* s1, const char_type* s2) {
  int l1 = char_traits::get_length(s1);
  int l2 = char_traits::get_length(s2);
  basic_lite_str<char_type, char_traits, alloc> ret;
  auto buf = ret.create_buffer(l1 + l2);
  memcpy(buf, s1, l1 * sizeof(char_type));
  memcpy(buf + l1 * sizeof(char_type), s2, (l2 + 1) * sizeof(char_type));
  return ret;
}

template <typename char_type, typename char_traits, typename alloc>
basic_lite_str<char_type, char_traits, alloc> operator + (const basic_lite_str<char_type, char_traits, alloc>& s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  return concatenate<char_type, char_traits, alloc>(s1.data(), s2.data());
}

template <typename char_type, typename char_traits, typename alloc>
basic_lite_str<char_type, char_traits, alloc> operator + (const basic_lite_str<char_type, char_traits, alloc>& s1, const char_type* s2) {
  return concatenate<char_type, char_traits, alloc>(s1.data(), s2);
}

template <typename char_type, typename char_traits, typename alloc>
basic_lite_str<char_type, char_traits, alloc> operator + (const char_type* s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  return concatenate<char_type, char_traits, alloc>(s1, s2.data());
}

template <typename char_type, typename char_traits, typename alloc>
basic_lite_str<char_type, char_traits, alloc> operator + (const basic_lite_str<char_type, char_traits, alloc>& s1, char_type s2) {
  char buf[2];
  buf[0] = s2;
  buf[1] = 0;
  return s1 + buf;
}

template <typename char_type, typename char_traits, typename alloc>
basic_lite_str<char_type, char_traits, alloc>& operator += (basic_lite_str<char_type, char_traits, alloc>& s1, const char_type* s2) {
  s1 = concatenate<char_type, char_traits, alloc>(s1.data(), s2);
  return s1;
}

template <typename char_type, typename char_traits, typename alloc>
basic_lite_str<char_type, char_traits, alloc>& operator += (basic_lite_str<char_type, char_traits, alloc>& s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  s1 = concatenate<char_type, char_traits, alloc>(s1.data(), s2.data());
  return s1;
}

template <typename char_type, typename char_traits, typename alloc>
bool operator == (const basic_lite_str<char_type, char_traits, alloc>& s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  return s1.data() == s2.data() || (s1.size() == s2.size() && memcmp(s1.data(), s2.data(), s1.size()) == 0);
}

template <typename char_type, typename char_traits, typename alloc>
bool operator == (const basic_lite_str<char_type, char_traits, alloc>& s1, const char_type* s2) {
  return s1.data() == s2 || (s1.size() == char_traits::get_length(s2) && memcmp(s1.data(), s2, s1.size()) == 0);
}

template <typename char_type, typename char_traits, typename alloc>
bool operator == (const char_type* s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  return s1 == s2.data() || (s2.size() == char_traits::get_length(s1) && memcmp(s2.data(), s1, s2.size()) == 0);
}

template <typename char_type, typename char_traits, typename alloc>
bool operator != (const basic_lite_str<char_type, char_traits, alloc>& s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  return !(s1 == s2);
}

template <typename char_type, typename char_traits, typename alloc>
bool operator != (const basic_lite_str<char_type, char_traits, alloc>& s1, const char_type* s2) {
  return !(s1 == s2);
}

template <typename char_type, typename char_traits, typename alloc>
bool operator != (const char_type* s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  return !(s1 == s2);
}

template <typename char_type, typename char_traits, typename alloc>
bool operator < (const basic_lite_str<char_type, char_traits, alloc>& s1, const basic_lite_str<char_type, char_traits, alloc>& s2) {
  return s1.data() != s2.data() && (strcmp(s1.data(), s2.data()) < 0);
}

inline lite_str<> operator "" _lstr(const char* str, size_t) {
  return lite_str<>::reference(str);
}
