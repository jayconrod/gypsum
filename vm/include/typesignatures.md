## Function type signatures

CodeSwitch uses *type signatures* to look up functions. A type
signature is a string that indicates the types of the function's
parameters. Type signatures are necessary to disambiguate between
functions that have the same name (overloaded functions).

Here are some examples of type signatures for various Gypsum
functions:

    // Type signature: ""
    def f: unit

    // Type signature: "(L)"
    def f(x: i64): unit

    // Type signature: "(Z,B,S,I,L,F,D)"
    def f(a: boolean, b: i8, c: i16, d: i32, e: i64, f: f32, g:f64): unit

    // Type signature: "(C::Object,C::String)"
    def f(obj: Object, str: String): unit

    class Foo
    // Type signature: "(C:Foo)"
    def f(foo: Foo): unit

    // Type signature: "(C:Foo?)"
    def f(foo: Foo?): unit

    import std.I32Array
    // Type signature: "(Cstd:I32Array)"
    def f(a: I32Array): unit

    import std.Option
    // Type signature "(Cstd:Option[C::String])"
    def f(opt: Option[String]): unit

    // Type signature: "[s<C::Object>C::Nothing,s<C::Object>C::Nothing](T0,T1)"
    def f[static S, static T](x: S, y: T): unit

### Type parameters and parameter types

Type signatures are divided into two parts: a *type parameters*
section delimited with brackets ('[', ']'), and a *parameter types*
section delimted with parenthesis ('(', ')').

The type parameters section describes the function's type parameters,
if it has any. If the function has no type parameters, the section
should be omitted. Descriptions for each type parameter are separated
by commas. A type parameter description consists of flags (currently
just `s` for `static`), an upper bound type preceded by a '<'
character, and a lower bound type preceded by a '>' character. For
example, if we have a function with type parameters defined like this:

    def f[static A <: Foo >: Bar, B <: Bar >: Baz]

Its signature will look like this:

    "[s<CFoo>CBar,<CBar>CBaz]"

The parameter types section describes the types of the actual
parameters of the function. If the function has no type parameters,
the section should be omitted. Note that functions without type
parameters or regular parameters will have an empty signature string
because of this. Descriptions for each parameter type are separated by
commas.

### Encoding types

Every type is encoded starting with a code character, which is a
capital letter. The codes are in the table below:

Type        | Code
----------- | ----
`unit`      | U
`boolean`   | Z
`i8`        | B
`i16`       | S
`i32`       | I
`i64`       | L
`f32`       | F
`f64`       | D
class       | C
variable    | T
existential | E

If the type is a primitive (not a class, variable, or existential)
then, the code describes the entire type. Otherwise, more information
follows the type.

When a class type is encoded, the class's name follows the 'C'
code. If the class is builtin, two colons ('::') separate the 'C' and
the name. If the class is from the same package as the function the
signature belongs to, a single colon separates the 'C' and the
name. If the class is from some other package, the package name is
written immediately after the 'C', followed by a single colon (':'),
followed by the class name.

    // Builtin class type
    // Type signature: "(C::String)"
    def f(str: String): unit

    class Foo
    // Class type from same package
    // Type signature "(C:Foo)"
    def f(foo: Foo): unit

    import std.I32Array
    // Class type from different package
    // Type signature "(Cstd:I32Array)"
    def f(a: I32Array): unit

If a class type has type arugments, they are written in brackets after
the class's name.

    import std.Option
    // Type signature: "(Cstd:Option[C::String])"
    def f(opt: Option[String]): unit

If a type is nullable, a question mark ('?') is written after
everything else.

    // Type signature "(C::String?)"
    def f(s: String?): unit

Variable types (those that refer to variables introduced by type
parameters or existential types) are encoded by a 'T' followed by a
zero-based index. So a variable type referring to the first type
parameter is `T0`, the second type paramter is `T1`, and so on. As
with class types, a nullable type is written with a question mark
after it.

    // Type signature: "[s<C::Object>C::Nothing](T0?)"
    def f[static X](x: X?): unit

Existential types comprise a set of type variables and an inner
type that may refer to those variables. The type parameters are
encoded in brackets after the 'E' code just like type parameters for
the function. The inner type is encoded like any other type. Variable
types within the inner type may refer to existential variables with
higher indices, as if those variables were declared in the function's
type parameter list after the rest of the type parameters.

    // Type signature "[s<C::Object>C::Nothing](T0,E[s<C::Object>C::Nothing]T1,E[s<C::Object>C::Nothing]T1)"
    def f[static A](a: A, b: forsome [B] B, c: forsome [C] C)

### Caveats

At this time, the CodeSwitch object doesn't support calling functions
with type parameters. Variable and existential types are not
guaranteed to work.
