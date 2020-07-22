
Move and Copy Semantics
=======================

.. contents::
  :local:

Move Semantics
~~~~~~~~~~~~~~

In Zenon, ``struct`` and ``union`` types can't be copied by default, but they can be moved. The ``move`` keyword
transforms a variable into an rvalue and prevents it from being used again.

.. code-block:: c++

  struct X {};

  void eat(X x) {}

  int main() {
    const x = X(5);
    eat(move(x));
    return x.a; // compile error: variable x has been moved
  }

Copy Semantics
~~~~~~~~~~~~~~

The recommended way of copying ``struct`` types in Zenon is through an explicit ``copy`` function. You can
instruct Zenon to autogenerate it using the ``default`` keyword.

.. code-block:: c++

  struct Bicycle {
    string name;
    int gears;
  };

  Bicycle copy(Bicycle*) = default;

  int main() {
    const bike1 = Bicycle("Cannondale", 21);
    const bike2 = bike1.copy();
    return 0;
  }

If your type can be copied cheaply, you can choose to support implicit copying.

.. code-block:: c++

  struct Vec {
    int x;
    int y;
  };

  Vec implicit_copy(Vec2*) = default;

  int main() {
    const v = Vec(10, 5);
    const w = v;
    return 0;
  }

Of course you are free to implement the bodies of the ``copy`` or ``implicit_copy`` functions yourself.
