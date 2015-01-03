# Features

## Language features

- allow calling overridden methods via "super"
- `final` attribute for classes, methods
- `static` attribute for methods, fields
- annotations
- require "get", "set" for implicit calls
- define getters, setters for fields automatically
- global variables
- dynamic type parameterization
- template type parameterization
- arrays
- native functions
- packages
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
- fix cyclic dependencies between modules
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

## Builtins
- use tags to format functions, classes, etc
- get rid of newlines in lists

# Bugs
