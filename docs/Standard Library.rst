
Standard Library
================

.. contents::
  :local:

io
~~

This file contains utilities for performing input and output.

.. code-block:: c++

  import "std/io.znn";
  
  int main() {
    mutable x = 5;
    printf("x = {x}\n");
    const file = file_output("file.txt");
    file.printf("52\n");
    discard move(file); // this calls the destructor, which closes the file
    const file2 = file_input("file.txt");
    x = file2.scan_int() ?? -1;
    if (const error = remove_file("file.txt"))
      printf(*error);
    return x;
  }

panic
~~~~~

vector
~~~~~~

map
~~~

set
~~~

enum_map
~~~~~~~~

enum_set
~~~~~~~~
