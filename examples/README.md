# Gypsum examples

This directory contains some examples written in Gypsum. A few simple
examples to start with are:

* hello.gy - introductory "hello world" program. Shows basic input, output,
  output, functions, and variables.
* factorial.gy - computes factorials recursively and
  iteratively. Shows if- and while-expressions.
* pi.gy - approximates the value of pi. A more advanced math example.
* list.gy - creates a linked list and sums the integers in it. Shows
  how classes and objects are used.

To build all the examples (in the top-level Gypsum directory):

```
make build-examples -j8
```

To run an individual example with the CodeSwitch VM:

```
make build-vm -j8
out/debug/driver -P out/debug -P out/debug/examples/hello.csp
```
