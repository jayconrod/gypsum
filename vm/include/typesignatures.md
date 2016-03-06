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

    // Type signature: "(C::6Object,C::6String)"
    def f(obj: Object, str: String): unit

    class Foo
    // Type signature: "(C:3Foo)"
    def f(foo: Foo): unit

    import std.I32Array
    // Type signature: "(C3std.8I32Array)"
    def f(a: I32Array): unit

    import std.Option
    // Type signature "(C3std.6Option[C::6String])"
    def f(opt: Option[String]): unit

    // Type signature: "(T0,T1)"
    def f[static S, static T](x: S, y: T): unit
