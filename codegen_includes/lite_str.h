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
#include <cuchar>
#include <clocale>
#include <stdexcept>

class lite_str {
  public:
  using char_type = char;

  private:
  const char_type *ptr;
  typedef int32_t length_t;
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
      delete[] ptr;
    }
  }

  public:

  lite_str() : ptr(""), length(0), type(REFERENCE) {}

  static lite_str reference(const char_type* s = "") {
    lite_str ret;
    ret.ptr = s;
    ret.length = strlen(s);
    return ret;
  }

  static lite_str owned(const char_type* s) {
    lite_str ret;
    auto buf = ret.create_buffer(strlen(s));
    strcpy(buf, s);
    return ret;
  }

  lite_str(const lite_str& other) {
    copy_from(other);
  }

  lite_str(lite_str&& other) noexcept {
    move_from(std::move(other));
  }

  lite_str& operator = (const lite_str& other) {
    destruct();
    copy_from(other);
    return *this;
  }

  lite_str& operator = (lite_str&& other) {
    destruct();
    move_from(std::move(other));
    return *this;
  }

  void move_from(lite_str&& other) {
    ptr = other.ptr;
    type = other.type;
    length = other.length;
    // This stops o from decreasing the ref counter if there is one
    other.type = REFERENCE;
  }

  void copy_from(const lite_str& other) {
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
    auto buf = new char_type[length + 1 + alloc_info_size];
    buf[length] = '\0';
    ptr = buf;
    get_ref_counter() = 1;
    type = OWNED;
    return buf;
  }

  ~lite_str() {
    destruct();
  }


  char32_t char_at_index(int index) const {
    if (index >= length)
      throw std::runtime_error("lite_str index out of bounds");
    if (isascii(ptr[index]))
      return ptr[index];
    std::setlocale(LC_ALL, "en_US.utf8");
    char32_t c32{};
    std::mbstate_t mbstate {};
    std::mbrtoc32(&c32, ptr + index, length - index, &mbstate);
    return c32;
  }

  char32_t operator [](int index) const {
    std::setlocale(LC_ALL, "en_US.utf8");
    char32_t c32{};
    std::mbstate_t mbstate {};
    int curInd = 0;
    for (int i = 0; i <= index; ++i) {
      if (curInd == length)
        throw std::runtime_error("lite_str index out of bounds");
      auto rc = std::mbrtoc32(&c32, ptr + curInd, length - curInd, &mbstate);
      if (rc == (std::size_t)-1 || rc == (std::size_t)-2)
        throw std::runtime_error("lite_str encoding error");
      curInd += rc;
    }
    return c32;
  }

  length_t buffer_size() const {
    return length;
  }

  int next_char(int index) const {
    unsigned char c = ptr[index];
    auto jump = [&] {
      if (c <= 127)
        return 1;
      if ((c & 0xE0) == 0xC0)
        return 2;
      else if ((c & 0xF0) == 0xE0)
        return 3;
      else if ((c & 0xF8) == 0xF0)
        return 4;
      else
        throw std::runtime_error("lite_str encoding error");
    }();
    if (index + jump > length)
      throw std::runtime_error("lite_str encoding error");
    return index + jump;
  }

  length_t size() const {
    int res = 0;
    int index = 0;
    while (ptr[index] != '\0') {
      index = next_char(index);
      ++res;
    }
    return res;
  }

  bool empty() const {
    return length == 0;
  }

  const char_type* data() const {
    return ptr;
  }

  lite_str substring(int index, int count) const {
    int buf_index = 0;
    for (int i = 0; i < index && buf_index < length; ++i) {
      buf_index = next_char(buf_index);
    }
    int buf_end = buf_index;
    for (int i = 0; i < count && buf_end < length; ++i) {
      buf_end = next_char(buf_end);
    }
    lite_str ret;
    auto buf = ret.create_buffer(buf_end - buf_index);
    memcpy(buf, ptr + buf_index, (buf_end - buf_index) * sizeof(char_type));
    return ret;
  }
};

inline lite_str concatenate(const lite_str::char_type* s1, const lite_str::char_type* s2) {
  int l1 = strlen(s1);
  int l2 = strlen(s2);
  lite_str ret;
  auto buf = ret.create_buffer(l1 + l2);
  memcpy(buf, s1, l1 * sizeof(lite_str::char_type));
  memcpy(buf + l1 * sizeof(lite_str::char_type), s2, (l2 + 1) * sizeof(lite_str::char_type));
  return ret;
}

inline lite_str operator + (const lite_str& s1, const lite_str& s2) {
  return concatenate(s1.data(), s2.data());
}

inline lite_str operator + (const lite_str& s1, const lite_str::char_type* s2) {
  return concatenate(s1.data(), s2);
}

inline lite_str operator + (const lite_str::char_type* s1, const lite_str& s2) {
  return concatenate(s1, s2.data());
}

inline lite_str operator + (const lite_str& s1, char32_t s2) {
  char buf[5] = {0};
  if (isascii(s2))
    buf[0] = s2;
  else {
    std::setlocale(LC_ALL, "en_US.utf8");
    std::mbstate_t state{};
    std::c32rtomb(buf, s2, &state);
  }
  return s1 + buf;
}

inline lite_str& operator += (lite_str& s1, const lite_str::char_type* s2) {
  s1 = concatenate(s1.data(), s2);
  return s1;
}

inline lite_str& operator += (lite_str& s1, const lite_str& s2) {
  s1 = concatenate(s1.data(), s2.data());
  return s1;
}

inline bool operator == (const lite_str& s1, const lite_str& s2) {
  return s1.data() == s2.data() || (s1.buffer_size() == s2.buffer_size() && memcmp(s1.data(), s2.data(), s1.buffer_size()) == 0);
}

inline bool operator == (const lite_str& s1, const lite_str::char_type* s2) {
  return s1.data() == s2 || (s1.buffer_size() == strlen(s2) && memcmp(s1.data(), s2, s1.buffer_size()) == 0);
}

inline bool operator == (const lite_str::char_type* s1, const lite_str& s2) {
  return s1 == s2.data() || (s2.buffer_size() == strlen(s1) && memcmp(s2.data(), s1, s2.buffer_size()) == 0);
}

inline bool operator != (const lite_str& s1, const lite_str& s2) {
  return !(s1 == s2);
}

inline bool operator != (const lite_str& s1, const lite_str::char_type* s2) {
  return !(s1 == s2);
}

inline bool operator != (const lite_str::char_type* s1, const lite_str& s2) {
  return !(s1 == s2);
}

inline bool operator < (const lite_str& s1, const lite_str& s2) {
  return s1.data() != s2.data() && (strcmp(s1.data(), s2.data()) < 0);
}

inline lite_str operator "" _lstr(const char* str, size_t) {
  return lite_str::reference(str);
}

template <typename stream>
stream& operator << (stream& os, const lite_str& s) {
  os << s.data();
  return os;
}

template <typename stream>
stream& operator >> (stream& os, lite_str& s) {
  s = lite_str();
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

