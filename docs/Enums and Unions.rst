
Enums and Unions
================

.. contents::
  :local:

Enums
~~~~~

Enums are very similar to C++'s ``enum class``, except that defining explicit values of elements is not supported.

.. code-block:: c++

  enum animal {
    SPIDER,
    FISH,
    KANGAROO,
    DOG
  };

The ``switch`` statement has a slightly different syntax, which allows specifying multiple values instead of using "breakthroughs".

.. code-block:: c++

  void handle(animal a) {
    switch (a) {
      case (SPIDER) { handleSpider(); }
      case (KANGAROO, DOG) { handleMammal(); }
      default { handleFish(); }
    }
  }

  int main() {
    handle(animal::FISH);
    // enum_size is a built-in function that returns the number of elements in an enum.
    return enum_size(animal);
  }

"std/enum.znn" contains some convenience functions for iterating through enum values and
converting to and from integers.

.. code-block:: c++

  import "std/enum.znn";

  void handleAll() {
    for (it : enum_range<animal>())
      handle(*it);
    int i = asInt(animal::DOG);
    animal a = fromInt<animal>(i);
  }

Unions
~~~~~~

Like in C/C++, a ``union`` is a type that holds a single value that can be one of several types.
But in Zenon, a ``union`` type also *knows* which type it's holding, so it can be switched on and examined.

.. code-block:: c++

  struct Circle { int radius; };
  struct Rectangle { int width; int height; };
  
  union Shape {
    Circle circle;
    Rectangle rectangle;
  };
  
  double get_area(Shape shape) {
    switch (shape) {
      case (circle) {
        return circle.radius * 3.14;
      }
      case (rectangle) {
        return rectangle.width * rectangle.height;
      }
    }
  }

Declaring a union type generates a constructor for each of its members in the form of ``UnionType::memberName(MemberType)``.
Implicit conversions from member types to the union type are currently not supported.
  
.. code-block:: c++

  int main() {
    const area = get_area(&Shape::circle(Circle(1)));
    return 0;
  }

