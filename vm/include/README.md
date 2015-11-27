CodeSwitch is a virtual machine which executes compiled bytecode for
the Gypsum programming language. CodeSwitch can be compiled as a
library and embedded in programs that need to execute bytecode.

This documentation covers the public API for CodeSwitch.

## Building and linking

To build CodeSwitch run `make build-vm` from the top-level directory
of this repository. This will produce a static library at
`out/debug/codeswitch.a` (or `out/release/codeswitch.a`) in release
mode. Simply link this library into your binary and include
`codeswitch.h`.

## Starting the VM

To start the VM, just create a `VM` object. Each `VM` object
represents an instance of the CodeSwitch virtual machine with its own
set of packages and garbage collected heap. Virtual machines are
completely independent and share no global state.

    #include <codeswitch.h>

    codeswitch::VM vm;

## Loading a package

CodeSwitch code is compiled into packages. Each package contains
a set of globals, classes, functions. Packages may depend on other
packages, and when you load a package, its dependencies will be loaded
automatically (if they aren't loaded already).

    codeswitch::Package package = vm.loadPackageFromFile("foo-1.0.csp");


## Calling a package's entry function

Each package may have an "entry" function (like a "main" function in
C++). This function may be loaded directly from the package.

    codeswitch::Function function = package.entryFunction();

You can check whether the entry function is valid by casting the
returned `Function` object to `bool`.

    if (!function) {
      std::cerr << "no entry function found!" << std::endl;
    }

To call a function, use the `call` method with the appropriate return
type. For example, if you are calling a function that returns `unit`
(equivalent to `void` in C++), just use `call`.

    function.call();

To call a function that returns a 64-bit integer, use `callForI64`.

    function.callForI64();

You can pass any number of arguments to these functions. See methods
in `CallBuilder` for supported types. All arguments and return types
are type-checked, and an exception will be thrown if a type error is
found.

    int64_t x = 12, y = 34;
    int64_t z = function.callForI64(x, y);

## Catching exceptions

CodeSwitch API functions may throw `Error` objects when they detect
errors. These can be caught, and an error message can be extracted.

    try {
      function.call();
    } catch (codeswitch::Error& error) {
      std::cerr << error.message() << std::endl;
    }

.