
Embedding and interfacing with C++
==================================

.. contents:: :local:

``embed``
---------

Zenon allows embedding C++ code directly between statements or top-level definitions.

.. code-block:: c++

  embed {
  #include <cstdio>
  }

  void print(int a) {
    embed {
      printf("%d\n", a);
    }
  }

``extern``
----------

If you'd like to use C++ types or call C++ functions from Zenon code,
you only have to declare their existence.

.. code-block:: c++

  embed {
  #include <vector>
  using std::vector;
  }

  template <T>
  extern struct vector;

  template <T>
  extern vector(); // declares std::vector's default constructor.

  template <T>
  extern int size(vector<T>*) method;

  template <T>
  extern void push_back(T) method;

  int main() {
    const v = vector<int>();
    v.push_back(5);
    return v.size();
  }
