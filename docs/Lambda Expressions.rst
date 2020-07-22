
Lambda Expressions
==================

Zenon offers lambda expressions with very similar syntax and semantics to C++.

.. code-block:: c++

  int main() {
    const lambda = [] (int a) { return a * 2; };
    return lambda(3);
  }

Like C++, Zenon gives you fine grained control over what and how the lambda expression captures.

By reference:

.. code-block:: c++

    mutable a = ...;
    const lambda1 = [&a] { ... };

Through an implicit copy:

.. code-block:: c++

    const lambda2 = [a] { ... }; 

Through a move, which invalides the original variable:

.. code-block:: c++

    const lambda3 = [move(a)] { ... };

Through an explicit copy, if ``a`` doesn't support implicit copying:

.. code-block:: c++

    const lambda3 = [copy(a)] { ... };

