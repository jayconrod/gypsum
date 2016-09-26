# Features

## Language features

- allow calling overridden methods via "super"
- `static` attribute for fields
- annotations
- require "get", "set" for implicit calls
- define getters, setters for fields automatically
- variadic type parameterization
- dynamic type parameterization
- template type parameterization
- downcasting and type testing
- default parameters
- positional, keyword arguments
- variadic function parameters
- function type and trait for standard lib
- lambda expressions
- type inference for type arguments
- type inference for lambda parameters
- infer types for numeric literals
- autoboxing
- class closures
- inner classes
- `static` attribute for closures
- `static` attribute for inner classes
- trait fields
- impl traits
- tail recursion
- awesome for loops
- dynamic features
- doc annotations
- list literal
- dict literal
- mutable / non-mutable references
- lifetimes and borrowing
- structs

## VM features
- backtrace implementation for exceptions

## Standard library features
- lists
- maps
- sets
- net
- concurrency
- unit testing

## Emacs mode
- match types properly
- match number suffixes like i32

# Refactoring

## Compiler
- Coding convention
- improved debug output for scope, types, etc.
- Does NameInfo.findDefnInfoWithArgTypes need to return allArgTypes? Seems unused.

## VM
- use variadic arguments for API call
- Implement optional ASLR
- Prune includes (automatically)
- Coding convention
- Switch enums to enum class, especially flags
