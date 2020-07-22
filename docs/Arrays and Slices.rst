
Arrays and Slices
=================

.. contents:: :local:

Arrays
------

Arrays in Zenon have full value semantics, and are semantically similar to ``std::array`` in C++.

.. code-block:: c++

  int sum(int[10] elems) {
    mutable res = 0;
    for (elem : elems)
      res += *elem;
    return res;
  }

The ``{}`` brackets are used to define an array literal.

.. code-block:: c++
  
  const a = sum({1, 2, 3, 4, 5, 6, 7, 8, 9, 10});

The underlying type of the array literal is inferred from its elements, but can also be specified directly.

.. code-block:: c++

  const a = {<double> 1, 2, 3};

In particular, it's required to define an empty array, since in that case the type can't be inferred.

.. code-block:: c++

  const a = {<char>};

Slices
------

The *slice* modifier ``[]`` defines a pointer to a range of values. It can be iterated, checked for emptyness, etc. It knows its size, so there is no need to pass a size parameter along.

.. code-block:: c++

  int sum(int[] elems) {
    mutable res = 0;
    for (i = 0; i < elems.size(); ++i)
      res += elems[i];
    return res;
  }

  int main() {
    const my_array = { 1, 2, 3, 4, 5 };
    return sum(my_array.slice());
  }

Note that a slice doesn't actually contain the elements - it only points to them. The programmer needs to make sure that the the slice variable doesn't outlive the underlying container.

