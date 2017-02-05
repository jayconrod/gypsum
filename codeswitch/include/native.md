## Linking native code

CodeSwitch has a foreign function interface which allows it to make
calls from interpreted functions to native (C++) functions. This is
useful for implementing features that require access to the kernel or
to native libraries. For example, this might be used to implement file
I/O or a database library wrapper.

In Gypsum, a function may be declared with the `native`
attribute. Native functions must not have bodies. For example, suppose
we have a package named `foo.http`, which has a function like this:

    public native def send-http-get-request(url: String): String

The native implementation will be loaded the first time this function
is called. There are several ways to load native implementations:

1. They can be loaded from a shared library (.so, .dylib, or .dll file)
   distributed with the package file. Shared libraries may be loaded
   automatically for packages that contain native functions. They will
   always be loaded from the same directory as the package.

2. They can be loaded from the VM or a shared library linked to the
   VM. This is useful if the VM and all the packages you want to load
   are distributed together, since you can link native implementations
   statically. Native functions still need symbols that are visible to
   the dynamic loader though (declare with
   `__attribute__((visibility("default")))` in G++ or Clang).

3. They can be loaded from a table of explicitly registered functions
   with the VM using `VMOptions`. This method gives you more control,
   but it is less convenient.

These methods are configurable using `VMOptions`. For example, if you
want to use the first two methods in that order:

    VMOptions vmOptions;
    vmOptions.nativeFunctionSearchOrder = vector<NativeFunctionSearch>{
        SEARCH_LIBRARY_FUNCTIONS, SEARCH_LINKED_FUNCTIONS
    };
    VM vm(vmOptions);

These methods can also be configured on a per-package basis when
calling `VM::loadPackage` or `VM::loadPackageFromFile`.

If a function is loaded dynamically using one of the first two
methods, it's loaded using a symbol derived from the package name and
the function name. Characters in the names other than ASCII letters,
numbers, and underscores are replaced with a single underscore. Name
components are separated by two underscores. The package and function
names are separate by three underscores. The symbol must not be
mangled. So in the example above, we would declare a function like
this:

    extern "C" __attribute__((visibility("default")))
    String foo__http___send_http_get_request(VM* vm, String url) {
       ...
    }

The first argument of any native function must be a pointer to the
VM. Other arguments may be primitives (bool, int8_t, int16_t, int32_t,
int64_t, float, double) or objects (Object, String). The return type
may also be any of these. Arguments and return values are passed by
value.
