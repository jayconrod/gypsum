# Features

## Language features

- allow calling overridden methods via "super"
- `final` attribute for classes, methods
- `static` attribute for methods, fields
- annotations
- require "get", "set" for implicit calls
- define getters, setters for fields automatically
- variadic type parameterization
- dynamic type parameterization
- template type parameterization
- arrays
- native functions
- standard library
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
- doc annotations

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
- fewer classes should inherit from Data
  - compile info classes
  - ir classes
- no optional parameters: prevent objects from being extended
- AST classes should always be referenced through module and should not start with Ast
- Does NameInfo.findDefnInfoWithArgTypes need to return allArgTypes? Seems unused.

## VM
- use variadic arguments for API call
- Implement optional ASLR
- Prune includes
- Remaining TODOs
- Coding convention
- Switch enums to enum class, especially flags

# Bugs

## Compiler
- flag bindings depending on whether they can be looked up from outside of the scope. right now,
  we can look up local variables from inside functions with a scope prefix.
- in type analysis, we check whether to do a local lookup based on whether the scope is same
  as current. we might have a prefix for the same scope though, so a local lookup would
  be unsafe.
- in type analysis handlePossibleCall, receiver may only be implicit if there is no prefix before,
  and we have no way to know that. same in visitAstDestructurePattern.

## VM
- `read()` breaks when EOF is given. Exception is thrown in non-GC-safe place.
- pattern matching performs an implicit downcast without telling the interpreter.
  We probably need a type-check-and-branch pattern.
- assertion when trying to instantiate Nothing. This should be a special case.
- ldp, ldpc, and other instructions which may throw exceptions should be GC-safe.
  No pointer map is recorded for these, even though we might allocate an exception.

# TODO: file issues for everything and delete this section.
