# Introduction

Gypsum is an experimental new programming language. In the future, it
will be useful for developing high-performance desktop and web
applications and games.

Gypsum is a compiled, statically-typed, object-oriented language. Once
the compiler is more fully implemented, it will be functional as
well. The compiler generates bytecode for a virtual machine,
CodeSwitch. CodeSwitch currently provides basic interpretation and
memory management. Eventually, it will have an optimizing JIT compiler
and will be able to execute bytecode for other languages.

The syntax of Gypsum is inspired by Python and Scala. It is designed
to be simple, requiring less extraneous punctuation than most
languages. Here's how "hello world" is written:

```
def main =
  print("Hello, world!\n")
```

You can find several simple programs in the `examples` directory.

# Projects

## Compiler

The Gypsum bootstrap compiler can be found in the `compiler`
directory. It is written in Python (2.7). Once the language is mostly
complete, the compiler will be rewritten in Gypsum.

To run the compiler:

```
cd compiler
./compiler program.gy -o program.csp
```

To test the compiler:

```
python -m unittest discover
```

## Virtual machine

The CodeSwitch virtual machine can be found in the `vm` directory. It
is intended to be minimal. Currently, it can load packages (.csp
files), manage memory (including a basic copying garbage collector),
and interpret bytecode (with a simple switch loop).

To build and test the VM:

```
cd vm
make -j
```

To run a package:

```
out/driver program.csp
```

## Other goodies

An experimental Emacs major mode for Gypsum is in
`tools/gypsum-mode.el`. It provides basic syntax highlighting.

To install, use `M-x package-install-file`, then add this to your `.emacs`:

```
(require 'gypsum-mode)
```

## Dependencies

* Python 2.7 is needed to run the compiler and to generate some files
  needed for CodeSwitch.
* PyYAML is needed to parse some data files needed for the Gypsum
  compiler and for CodeSwitch.
* G++ 4.8 or any other C++ compiler which supports C++11 is needed to
  build CodeSwitch. Other binutils are assumed as well.
* GNU Make 3.81 is needed to build CodeSwitch.

Gypsum and CodeSwitch were developed and tested on Ubuntu 13.10. They
will hopefully work on other platforms with minimal modification, but
nothing else is supported right now.
