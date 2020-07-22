
Run-time polymorphism
=====================

.. contents:: :local:

Zenon provides two distinct mechanisms of run-time polymorphism. 

``union``-based dynamic dispatch
--------------------------------

This type of dynamic dispatch lets you define functions for a ``union`` type that forward the call
to the corresponding function of the currently held type.

.. code-block:: c++

  struct Dog {};
  struct Cat {};

  void makeSound(Dog*) { print("woof!"); }
  void makeSound(Cat*) { print("meow!"); }

.. code-block:: c++

  union Animal {
    Dog dog;
    Cat cat;
  };

  void makeSound(virtual Animal*);

  int main() {
    const d = Animal::dog(Dog());
    d.makeSound(); // woof!
    return 0;
  }

The compiler implements ``makeSound`` internally in the following manner:

.. code-block:: c++

  void makeSound(Animal* a) {
    switch (*a) {
      case (dog) { makeSound(&dog); }
      case (cat) { makeSound(&cat); }
    }
  }

If the 'virtual' function contains a body, it serves as the default logic for types
for which the given function is not implemented.

.. code-block:: c++

  void runAway(Cat*) {
    print("the cat climbs a tree");
  }

  void runAway(virtual Animal* animal) {
    // this code will run if 'animal' is of type 'Dog'.
    print("the animal runs away");
  }

The benefits of ``union``-based dynamic dispatch are:

* The type 'hierarchy' has value semantics and doesn't require memory allocation.
* Virtual template functions are supported.
* Multiple dispatch is possible (although it's not implemented yet).
* The 'interface' can be extended easily by defining new virtual functions somewhere else in the code.

The only downside is that the complete list of the 'subtypes' must be defined in the ``union`` definition and 
can't be extended elsewhere, for example by library clients.

Type erasure using 'concept' types
----------------------------------

A 'concept' type points to or holds a given value while forgetting its original type. It 'knows' how to 
call a predefined set of functions on it though.

.. code-block:: c++

  template <T>
  concept MakesSound {
    void makeSound(T*);
  };

  void pet(MakesSound* m) {
    m->makeSound();
  }

  MakesSound get() {
    return Cat();
  }

  int main() {
    const c = Cat();
    pet(&c);
    MakesSound something = get();
    something.makeSound();
    return 0;
  }

Here are the differences between 'concept' types and ``union``-based dispatch:

* The list of types implementing the interface is not predefined and can be extended anywhere in the code.
* The interface of the 'concept' type is fixed in the ``concept`` definition.
* The value-based concept type requires memory allocation.
