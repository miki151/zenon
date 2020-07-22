
Language Basics
===============

.. contents::
  :local:

Writing a function
------------------

Functions are declared similarly to C and C++:

<Return type> <name>(<parameter list>) { <body> }

The "main" function is the entry point to the program. It can have either zero parameters or one ``string[]`` parameter.
It must return an ``int``. 

.. code-block:: c++

  int main(string[] args) {
    return 0;
  }

Zenon requires that all paths through a function's body end with a ``return`` statement (unless the function returns ``void``).

.. code-block:: c++

  int main(string[] args) {
    if (args.size() < 2)
      return 1;
    // error: missing return statement
  }

A function parameter may be declared ``mutable``, if it needs to be modified in the body of the function. Otherwise modifying parameters is not allowed.

.. code-block:: c++

  int get(mutable int a) {
    a = a + 1;
    return a;
  }

Declaring a variable
--------------------

Variables are either ``const`` or ``mutable``, and must always be initialized. The simplest way to declare a variable is:

.. code-block:: c++
  
  const my_var = 1;

You may optionally specify the type, in which case the initial value will be converted to it.

.. code-block:: c++
  
  mutable double my_var2 = 10;

If the type is specified, then the ``const`` modifier may be skipped;

.. code-block:: c++
  
  double my_var2 = 10;
  my_var2 = 3.14 // error - my_var2 is constant

Declaring a struct type
-----------------------

Struct types in Zenon are very simple, and only contain a list of member types and names.

.. code-block:: c++

  struct person {
    string name;
    int age;
  };

Declaring a struct automatically creates a *constructor* function that takes all the members as arguments.

.. code-block:: c++

  int main() {
    const myself = person("Bilbo Baggins", 111);
    return myself.age;
  }


Built-in types
--------------

The types ``int``, ``double``, ``bool``, ``void``, and ``char`` work pretty much the same as in C/C++, except that
some implicit conversions, like from ``double`` to ``bool`` are not allowed.

Zenon has a built-in ``string`` type, which is immutable and can be thought of as an extended ``char const*`` from C/C++.
It is reference-counted, but encapsulates string literals directly without any allocations.

.. code-block:: c++

  string repeat(string text, int count) {
    mutable ret = "repeated: "; // no allocation here
    for (i = 0; i < count; ++i)
      ret += text;
    return ret;
  }

Type modifiers
--------------

A pointer type can be ``mutable``, which tells you that the pointed-to value can be modified. Note that pointers
can't be null by default. Zenon doesn't support pointer arithmetics, except the equality comparison.

.. code-block:: c++

  void f(int* ptr) {
    *ptr = *ptr + 1; // error
  }

  void f(int mutable* ptr) {
    *ptr = *ptr + 1;
  }


The nullable modifier ``?`` creates a type that can be set to ``null``. It can be applied to any type, not just pointers.

.. code-block:: c++

  int main() {
    mutable int? x = null;
    x = 5;
    if (x)
      return *x;
    else
      return 0;
  }

The last four lines can be shortened to ``return x ?? 0``. The ``??`` operator returns the LHS if it evaluates to true, and RHS otherwise.

Importing other files
---------------------

Projects often consist of multiple files, and you usually also use some libraries, for example the *std* library.

.. code-block:: c++

  import "std/io.znn";

  int main(string[] args) {
    for (arg : args)
      printf("Argument {arg.index} is: {*arg}\n");
    return 0;
  }


Definitions that are to be imported by other files must be preceded by the ``export`` keyword.

.. code-block:: c++

  export void print(string text) {
    ...
  }


