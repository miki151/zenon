# zenon
## The Zenon programming language (work in progress)

Zenon is a compiled language with similiar features to C++. The aim is to remove a lot of complexity from C++, and add some features that support safer and more high-level programming.


### Features
* No null pointer
* No uninitialized variables and members
* No headers
* Built-in variant/tagged union type
* Named parameters in function calls
* Reflection (tbd)
* Compiles to C++

### Non-features
* Major paradigm divergence from C++

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

template<T>
Nullable<T> value(T v) {
    return Nullable<T>::value(v);
}

template<T>
Nullable<T> null() {
    return Nullable<T>::null();
}

int example() {
    // The template parameter of the function 'value' is inferred.
    Nullable<int> var = value(5);
    var = null<int>();
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

embed {
    #include <stdio.h>
}

void print(int a) {
    embed {
      //C++ code in this block
      printf("%d\n", a);
    }
}

int main() {
    print(5);
}

```

### Named parameters
``` C++
int sum(int a, int b) {
    return a + b;
}

int main() {
    int x = sum(3, 4);
    x = x + sum({a = 32, b = -30});
    return x;
}
```

### Working with multiple files
``` C++
// library.znn
int getNumber() {
    return 5;
}

// main.znn
import "library.znn"
import "print.znn"

int main() {
    print(getNumber());
    return 0;
}
