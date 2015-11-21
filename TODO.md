# Features

## Language features

- allow calling overridden methods via "super"
- `final` attribute for classes, methods
- `static` attribute for fields
- annotations
- require "get", "set" for implicit calls
- define getters, setters for fields automatically
- variadic type parameterization
- dynamic type parameterization
- template type parameterization
- native functions
- standard library
- downcasting and type testing
- default parameters
- positional, keyword arguments
- variadic function parameters
- function type and trait for standard lib
- lambda expressions
- type inference for type arguments
- type inference for lambda parameters
- infer types for numeric literals
- class closures
- inner classes
- `static` attribute for closures
- `static` attribute for inner classes
- traits and multiple inheritance
- tail recursion
- awesome for loops
- dynamic features
- doc annotations
- list literal
- map literal
- number classes and autoboxing

## VM features
- native object access (depends on dynamic features)
- native implementations for functions
- backtrace implementation for exceptions

## Standard library features
- tuples
- lists
- maps
- sets
- file i/o
- net
- concurrency

## Emacs mode
- match types properly
- match number suffixes like i32

# Refactoring

## Compiler
- Coding convention
- improved debug output for scope, types, etc.
- AST classes should always be referenced through module and should not start with Ast
- Does NameInfo.findDefnInfoWithArgTypes need to return allArgTypes? Seems unused.
- Speed up and parallelize tests

## VM
- use variadic arguments for API call
- Implement optional ASLR
- Prune includes (automatically)
- Coding convention
- Switch enums to enum class, especially flags
