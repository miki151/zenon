#pragma once
#include <fstream>
#include "codegen_includes/lite_str.h"


struct input_file {
  input_file(lite_str<> path) : s(path.data()) {}

  template <typename T>
  T read() {
    T t;
    s >> t;
    return t;
  }

  bool is_good() {
    return s.good();
  }

  std::ifstream s;
};
