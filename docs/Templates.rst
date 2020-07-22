
Templates
=========

.. contents::  :local:

Template types
--------------

Template syntax is similar to C++, except that the ``typename`` keyword is skipped.

.. code-block:: c++

  template <T>
  struct Vec3 {
    T x;
    T y;
    T z;
  };

.. code-block:: c++

  template <Result, Error>
  union Expected {
    Result result;
    Error error;
  };


Template functions
------------------

.. code-block:: c++

  template <T>
  T chooseRandom(T a, T b, Rng rng) {
    if (rng.rand(2) == 0)
      return move(a);
    else
      return move(b);
  }

Template arguments are deduced from runtime arguments, if possible.

.. code-block:: c++

  const a = chooseRandom("red", "blue", rng);
  const v = Vec3(3, 4, 5);


Concepts and requirements
-------------------------

A ``concept`` specifies the interface of one or more template types. It consists simply of a number of 
functions.

.. code-block:: c++
  
  template <T>
  concept CanAdd {
    T operator + (T*, T*);
  };

  template <T, U>
  concept CanConvert {
    U convertTo(T);
    T convertFrom(U);
  };

A template function specifies the required concepts that its template parameters must implement using the ``requires`` keyword.

.. code-block:: c++

  template <T> requires CanAdd<T>
  void sum(T mutable* result, T[] elems) {
    for (it : elems)
      *result = *result + *it;
  }

.. code-block:: c++

  int main() {
    mutable result = 0;
    sum(&result, {1, 2, 3, 4, 5}.slice());
    return result;
  }

A template argument that doesn't satisfy the function's requirements causes a compile error at call site.

.. code-block:: c++

  int main() {
    mutable result = 'a';
    sum(&result, {'b', 'c', 'd'}.slice()); // error
    return 0;
  }

Unlike in C++, the body of a template function is only allowed operations on its template types
that were specified in the ``requires`` clause.

.. code-block:: c++

  template <T>
  T sum(T* t1, T* t2) {
    return *t1 + *t2; // error
  }

Value templates
---------------

Types and functions can also have template parameters that are constants. Their requirements
can include arbitrary (compile-time) expressions, surrounded by ``(`` and ``)``.

.. code-block:: c++

  template <T, int N> requires (N >= 0)
  struct Vec {
    T[N] elems;
  };

  template <T, int N> requires CanAdd<T>
  Vec<T, N> sum(Vec<T, N>* v1, Vec<T, N>* v2) {
    return Vec(array_generate<N, T>(
        [&](int index) { return v1->elems[index] + v2->elems[index]; })
    );
  }

Variadic templates
------------------

To Be Added
