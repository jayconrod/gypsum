# Gypsum examples

This directory contains some examples written in Gypsum. Here's a
suggested order to look at them:

* hello.gy - introductory "hello world" program. Shows basic input, output,
  output, functions, and variables.
* factorial.gy - computes factorials recursively and
  iteratively. Shows if- and while-expressions.
* pi.gy - approximates the value of pi. A more advanced math example.
* list.gy - creates a linked list and sums the integers in it. Shows
  how classes and objects are used.

Before you can run any examples, you need to compile the VM driver.

```
cd ../vm
make -j
```

You can compile an example like this:

```
../compiler/compiler hello.gy -o hello.csp
```

And then run it like this:

```
../vm/out/driver hello.csp
```
