
Operator Overloading
====================

Zenon allows overloading most operators for user-defined types.

.. code-block:: c++

  struct Vec {
    int x;
    int y;
  };

  Vec operator + (Vec a, Vec b) {
    return Vec(a.x + b.x, a.y + b.y);
  }


If arguments are passed by pointer, they are transparently converted from values at call site.

.. code-block:: c++

  int operator * (Vec* a, Vec* b) {
    a->x * b->x + a->y * b->y;
  }

.. code-block:: c++

  const v = Vec(3, 4) * Vec(5, 6); // no need to take addresses of the temporary values

The operators that can be overloaded are:

* ``[]`` - subscript
* ``+`` - binary and unary plus
* ``-`` - binary and unary plus
* ``*`` - multiplication and pointer dereference
* ``/`` - division
* ``%`` - modulo
* ``==`` - equals
* ``<`` - less than*
* ``>`` - more than*
* ``++`` - increment by one
* ``--`` - decrement by one
* ``!`` - logical not
* ``??`` - "value or"
  
\* *Note: Zenon rewrites operators* ``<=`` *and* ``>=`` *as not greater than or less than respectively. Therefore overloading* ``<=`` *and* ``>=`` *is not necessary.*

