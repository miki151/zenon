
Loops
=====

.. contents::
  :local:

'Standard for' loop
~~~~~~~~~~~~~~~~~~~

The 'standard for' loop in Zenon has a few differences compared to other languages. The first statement
in the loop header must be a variable declaration, i.e. it's not possible to use an existing variable
or do other things in it. It contains an optional ``mutable`` keyword, which allows modifying the loop
counter in the loop body.

.. code-block:: c++

  for (i = 0; i < 10; ++i)
    print(i);

  for (mutable i = 0; i < 10; ++i) {
    print(i);
    if (i % 2 == 1)
      ++i; // if mutable is skipped in i's definition then this causes a compile error
  }

'Range-based for' loop
~~~~~~~~~~~~~~~~~~~~~~

The 'range-based for' loop in Zenon is similar to its C++ equivalent with one small difference.
The loop exposes the actual iterator over the range, and not the element value. This allows
calling extra functions on the iterator, such as removing the current value or fetching the current index,
if the range supports it.

.. code-block:: c++

  void sumEven(int[] elements) {
    mutable ret = 0;
    for (it : elements)
      if (it.index % 2 == 0)
        ret += *it;
    return ret;
  }

'Static for' loop
~~~~~~~~~~~~~~~~~

The 'static for' loop is a regular for loop that is unrolled at compile time. This means
that all expressions in the loop header must be compile-time expressions. The loop body is evaluated
separately for each iteration, so its types and called functions may differ. This loop is mainly
used for metaprogramming.

.. code-block:: c++

  void handle(int*) {}
  void handle(double*) {}
  void handle(string*) {}

  struct X {
    int a;
    double b;
    string c;
  };

  void handle(X* x) {
    static for (i = 0; i < struct_count(X); ++i)
      handle(&x->(i));
  }
