
Constructors and Destructors
============================

.. contents::
  :local:

Constructors
~~~~~~~~~~~~

Defining a ``struct`` automatically creates a default constructor with the struct members as parameters.
You can also implement custom constructors. A constructor in Zenon is a normal function that returns the given type, usually after calling another (typically the default) constructor.

.. code-block:: c++

  struct Table {
    int legs;
    string color;
  };

  Table(string color) {
    return Table(4, move(color));
  }

You can also override the default constructor. Calling the original constructor is then done
using special syntax, shown below.

.. code-block:: c++

  Table(int legs, string color) {
    mutable result = Table::Table(legs, move(color));
    peform_extra_logic(&result);
    return move(result);
  }

Destructors
~~~~~~~~~~~

A type's destructor is called when a variable goes out of scope, but *not* if it was moved. Destructors
are defined using a special ``destruct`` function.

.. code-block:: c++

  void destruct(Table* table) {
    print("Destroying a table of color {table->color}\n");
  }

  void eat(Table t) {
    // Table's destructor called at the end of this function.
  }

  int main() {
    const table = Table(3, "brown");
    eat(move(table));
    // Since 'table' was moved, its destructor won't be called.
    return 0;
  }
