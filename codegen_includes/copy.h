#include "lite_str.h"

inline int copy(const int* a) {
  return *a;
}

inline bool copy(const bool* b) {
  return *b;
}

inline lite_str<> copy(const lite_str<>* s) {
  return *s;
}

inline double copy(const double* a) {
  return *a;
}

inline char copy(const char* a) {
  return *a;
}

