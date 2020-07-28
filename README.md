## The Zenon programming language

Zenon is a statically typed language inspired by C++. Its mission is to offer safer and easier application development thanks to high-level features and fast compile times, while keeping C++’s performance and familiar syntax. It’s also massively simpler than C++.

Visit https://zenon-lang.org for an overview of the language.

### Compiling

The Zenon compiler is currently in the prototype phase, and if you’d like to try it out, you’ll have to compile it from source. Note that Zenon compiles only with the GCC or Clang compiler, it doesn’t work with MSVC. Download the source code from Github:

    git clone https://github.com/miki151/zenon.git

Zenon requires boost-regex. You can install it on Ubuntu with:

    apt install libboost-regex-dev

Now you can just run make to build it:

    cd zenon
    make

Let’s try out an example program.

    echo '
    import "std/io.znn";

    int main(string[] args) {
      if (!args.empty())
        printf("The first argument was: {args[0]}\n");
      return 0;
    }
    ' > test.znn
    ./zenon test.znn -o test
    ./test

Zenon automatically compiles all imported files as separate compilation units and links them, so there is no need to write Makefiles.

The default C++ compiler used by zenon is “g++”. You can change it or add extra flags with a command line flag, run ./zenon –help to see all of the options.

If you run zenon without the -o flag, it will output the generated C++ code to stdout.
