# Features

## Language features

- allow calling overridden methods via "super"
- `final` attribute for classes, methods
- `static` attribute for methods, fields
- annotations
- require "get", "set" for implicit calls
- define getters, setters for fields automatically
- dynamic type parameterization
- template type parameterization
- arrays
- native functions
- packages
- standard library
- compile from multiple files
- downcasting and type testing
- pattern matching
- default parameters
- positional, keyword arguments
- proper closures (with type from standard lib)
- class closures
- inner classes
- `static` attribute for inner classes, functions
- other expressions (lambdas, pattern matching, etc)
- traits and multiple inheritance
- tail recursion
- awesome for loops
- dynamic features

## VM features
- native object access (depends on dynamic features)
- native implementations for functions
- packages

## Emacs mode
- match types properly
- match number suffixes like i32

# Refactoring

## Compiler
- Coding convention
- improved debug output for scope, types, etc.
- replace "$..." names with symbols
- fewer classes should inherit from Data
  - compile info classes
  - ir classes
- no optional parameters: prevent objects from being extended
- use separate Id classes instead of integers

## VM
- use variadic arguments for API call
- Implement optional ASLR
- Prune includes
- Remaining TODOs
- Coding convention
- Switch enums to enum class, especially flags

# Bugs

## Compiler
- delete output file when an internal error is encountered

## VM
- `read()` breaks when EOF is given. Exception is thrown in non-GC-safe place.
- pattern matching performs an implicit downcast without telling the interpreter.
  We probably need a type-check-and-branch pattern.
- no way to express nullable types for casts in bytecode.
- assertion when trying to instantiate Nothing. This should be a special case.

# TODO: file issues for everything and delete this section.
