# zenon
## The Zenon programming language (work in progress)

Zenon is a compiled language with similiar features to C++. The aim is to remove a lot of complexity from C++, and add some features that support safer and more high-level programming.


### Features
* No null pointer
* No uninitialized variables and members
* Removes most or possibly all undefined behaviour
* Built-in variant/tagged union type
* Compiles to C++


## Example code

### Variant

``` C++
variant IntOrBool {
    bool asBool;
    int asInt;
};

int example() {
    IntOrBool var = IntOrBool::asBool(true);
    switch (var) {
        case (bool asBool) {
            if (asBool)
                return 1;
        }
        case (int asInt) {
            return asInt;
        }
    }
    return -1;
}

```

### Templates
``` C++
// Implementing a nullable a'ka optional type using a variant
template <T>
variant Nullable {
    T value;
    void null;
};

int example() {
    Nullable<int> var = Nullable<int>::value(5);
    var = Nullable<int>::null();
    switch (var) {
        case (int value) {
            return value;
        }
        default {
            return -1;
        }
    }
}
```

### Embedding C++ code
``` C++
embed #include <stdio.h>

void print(int a) embed {
  // C++ code in the body of this function 
  printf("%d\n", a);
}

int main() {
    print(5);
}

```
