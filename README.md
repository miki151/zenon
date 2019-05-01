# zenon
## The Zenon programming language

Zenon is a statically typed language that compiles to C++. It offers safe and easy application development thanks to high-level features and fast compile times, while keeping C++'s performance and familiar syntax.


### Features
* No headers
* Default pointer type is non-null.
* No uninitialized variables and members.
* No member functions/methods. Any function can be called with a method syntax.
* Compiles to C++.
* Extremely easy binding with C/C++.
* Built-in variant/tagged union type
* Syntax similar to C++

## Zenon in examples

### Variant

``` C++
variant my_variant {
    bool bvalue;
    int ivalue;
};

int example() {
    const var = my_variant::bvalue(true);
    switch (var) {
        case (bool bvalue) {
            if (bvalue)
                return 1;
        }
        case (int ivalue) {
            return ivalue;
        }
    }
    return -1;
}

```

### Templates
``` C++
// Implementing a nullable aka optional type using a variant
template <T>
variant nullable {
    T value;
    void null;
};

template<T>
nullable<T> value(T v) {
    return nullable<T>::value(v);
}

template<T>
nullable<T> null() {
    return nullable<T>::null();
}

int example() {
    // 'T' is inferred from the function argument.
    mutable var = value(5);
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

embed {
    #include <vector>
    using std::vector;
}

// Zenon can't parse the C++ vector header, so we have to tell it that 'vector' exists and how to use it. 
template<T>
extern struct vector {
    int size();
    void push_back(T t);
    T operator[](int index);
};

int main() {
    auto v = vector<int>();
    for (int i = 0; i < 10; i = i + 1)
        v.push_back(i);
    print(v[3]);
    return 0;
}

```

### Named parameters
``` C++
int sum(int a, int b) {
    return a + b;
}

struct my_struct {
    int my_int;
    bool my_bool;
}

int main() {
    int x = sum(3, 4);
    x = x + sum(.a = 32, .b = -30);
    auto my1 = my_struct(.my_int = 5, .my_bool = false);
    return x;
}
```

### Working with multiple files
``` C++
// library.znn
export int get_number() {
    return 5;
}
```
``` C++
// main.znn
import "library.znn"
import "print.znn"

int main() {
    print(get_number());
    return 0;
}
```

### Concepts
``` C++
template <T>
concept comparable {
    bool operator < (T*, T*);
};

template <T> requires comparable<T>
T min(T value1, T value2) {
    // the only operations allowed on T in this context are those guaranteed by comparable<T>
    if (value1 < value2)
        return move(value1);
    else
        return move(value2);
}

int main() {
  const v = min(3, 5);
  // calling min with a type that doesn't implement "comparable" results in a compile error one the spot
  // const c = min('a', 'b'); // compile error: char doesn't implement operator < required by "comparable"
}
```
