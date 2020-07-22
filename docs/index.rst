.. Zenon Programming Language documentation master file, created by
   sphinx-quickstart on Sat May 23 11:24:47 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

About Zenon
===========

Introduction
------------

Zenon is a statically typed language inspired by C++. Its mission is to offer safer and easier application development thanks to high-level features and fast compile times, while keeping C++'s performance and familiar syntax. It's also massively simpler than C++.


.. contents::  Here are the major differences compared to C++:
  :local:

No headers
~~~~~~~~~~
Including code from another file is as simple as using the *import* directive. Only *exported* definitions from that file are visible.

.. code-block:: c++

  import "file2.znn";

  int main() {
    return get(5);
  }

file2.znn:

.. code-block:: c++
  
  export int get(int a) {
    return hidden(a);
  }

  // this function is not visible outside this file
  int hidden(int a) {
    return a + 1;
  }

Semantically this is simliar to splitting *file2.znn* into a header and a source file in C++ and declaring *int get(int)* in the header.
In Zenon, however, each file is evaluated in its own separate context, and symbols don't leak from one imported file to the next one.

Improved move semantics
~~~~~~~~~~~~~~~~~~~~~~~

Zenon inherits move semantics from C++, but takes them a step further by having a ``move`` keyword that prevents further use of the variable.

.. code-block:: c++

  struct X {
    int a;
  };

  void eat(X x) {}

  int main() {
    const x = X(5);
    eat(move(x));
    return x.a; // compile error: variable x has been moved
  }

No member functions
~~~~~~~~~~~~~~~~~~~

Zenon has only free functions, but they can be called using the "dot" syntax.

.. code-block:: c++

  struct X {
    int a;
  };

  int get_a(X* x) {
    return x->a;
  }

  int main() {
    const x = X(5);
    return x.get_a();
  }

Pointers are non-null and const by default
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To increase safety, nullable or mutable pointers are declared using extra syntax.

.. code-block:: c++

  struct X {
    int a;
  };

  int set_a(X mutable* x, int value) {
    x->a = value;
  }

  void process(X*? x) {
    if (x != null)
      ...
  }

Tagged unions
~~~~~~~~~~~~~

Union types are tagged with the currently held type, which can be accessed in a switch statement.

.. code-block:: c++

  struct Circle { int radius; };
  struct Rectangle { int width; int height; };

  union Shape {
    Circle circle;
    Rectangle rectangle;
  };

  double get_area(Shape* shape) {
    switch (*shape) {
      case (circle) {
        return circle.radius * 3.14;
      }
      case (rectangle) {
        return rectangle.width * rectangle.height;
      }
    }
  }

No reference types
~~~~~~~~~~~~~~~~~~

In a language that contains pointers, reference types are redundant, therefore they have not been added to Zenon.
This greatly simplifies the language, especially the semantics of type deduction and templates.

Taking the address of a temporary value to pass it as a const pointer argument to a function is allowed:

.. code-block:: c++

  void f(int* value) { ... }

.. code-block:: c++

  f(&getValue());
  f(&5);


No inheritance
~~~~~~~~~~~~~~

Since virtual polymorphism can be achieved in Zenon using *unions* or *concept types*, inheritance
is not supported in Zenon.

Compiles to C++
~~~~~~~~~~~~~~~

Zenon is compiled to C++ and requires a C++ compiler present in the system to create executables.
This provides many advantages:

* Easy binding with C/C++. Using STL types, such as std::vector or std::unordered_map is trivial.
* Performance is as good as C++ thanks to using the C++ compiler for optimization.
* Excellent portability.


.. toctree::
   :maxdepth: 10
   :hidden:

   About Zenon <self>
   Getting Started
   Language Basics
   Move and Copy Semantics
   Loops
   Arrays and Slices
   Enums and Unions
   Constructors and Destructors
   Lambda Expressions
   Templates
   Run-time polymorphism
   Operator Overloading
   Embedding and interfacing with C++
   Reflection and Metaprogramming
   Standard Library



