load("//:build_defs.bzl", "gy_package")
load(":build_defs.bzl", "py_gen_file", "doxygen_archive", "gy_test_name")

config_setting(
    name = "linux-x64",
    values = {
        "cpu": "k8",
    },
)

config_setting(
    name = "osx-x64",
    values = {
        "cpu": "darwin",
    },
)

config_setting(
    name = "debug",
    values = {
        "compilation_mode": "dbg",
    }
)

CODESWITCH_COPTS = [
    "-std=c++11",
    "-iquote", "codeswitch/src",
    "-iquote", "$(GENDIR)/codeswitch/src",
    "-Wno-invalid-offsetof",
]

cc_library(
    name = "codeswitch",
    srcs = glob(["src/*.h", "src/*.cpp"]) + [
        ":src/builtins.h",
        ":src/flags.h",
        ":src/opcodes.h",
        ":src/roots_builtins.cpp",
    ] + select(
        {
            ":linux-x64": glob([
                "src/posix/*",
                "src/linux/*",
                "src/x64/*",
                "src/linux-x64/*",
            ]),
            ":osx-x64": glob([
                "src/posix/*",
                "src/osx/*",
                "src/x64/*",
                "src/osx-x64/*"
            ]),
        },
        no_match_error = "unsupported platform",
    ),
    hdrs = glob(["include/*.h"]),
    includes = ["include"],
    defines = [
        "WORDSIZE=64",
        "PAGESIZE=4096",
    ] + select(
        {
            ":debug": ["DEBUG"],
            "//conditions:default": [],
        }
    ),
    copts = CODESWITCH_COPTS,
    linkopts = ["-ldl"],
    visibility = ["//visibility:public"],
)

cc_binary(
    name = "codeswitch_cmd",
    srcs = ["programs/driver.cpp"],
    deps = [":codeswitch"],
    copts = ["-std=c++11"],
    visibility = ["//visibility:public"],
)

# This only works if Doxygen is installed.
# It must be run manually.
doxygen_archive(
    name = "codeswitch_doc",
    doxyfile = "include/Doxyfile",
    srcs = glob(["include/*.h", "include/*.md"]),
    tags = ["manual"],
)

py_gen_file(
    name = "builtins",
    script = "src/gen_builtins_h.py",
    data = "//:common/builtins.yaml",
    out = "src/builtins.h",
)

py_gen_file(
    name = "flags",
    script = "src/gen_flags_h.py",
    data = "//:common/flags.yaml",
    out = "src/flags.h",
)

py_gen_file(
    name = "opcodes",
    script = "src/gen_opcodes.py",
    data = "//:common/opcodes.yaml",
    out = "src/opcodes.h",
)

py_gen_file(
    name = "roots_builtins",
    script = "src/gen_roots_builtins_cpp.py",
    data = "//:common/builtins.yaml",
    out = "src/roots_builtins.cpp",
)

GYPSUM_TESTS = [(f, gy_test_name(f)) for f in glob(["test/*.gy"])]

cc_test(
    name = "codeswitch_test",
    size = "small",
    srcs = glob(["test/*.cpp", "test/*.h", "src/*.h"], exclude=["test.h", "test.cpp"])
        + [":test/%s_resource" % t for (_, t) in GYPSUM_TESTS],
    deps = [
        ":codeswitch",
        ":codeswitch_test_lib",
    ],
    data = ["//std", "//std/io"],
    copts = CODESWITCH_COPTS + ["-iquote", "codeswitch/test"],
    args = ["-P", "std", "-P", "std/io"],
)

cc_library(
    name = "codeswitch_test_lib",
    srcs = ["test/test.cpp"],
    hdrs = ["test/test.h"],
    deps = [":codeswitch"],
    testonly = True,
)

[gy_package(
    name = "test/%s_package" % t,
    package_name = "test." + t,
    srcs = [f],
    deps = ["//std", "//std/io"],
) for f, t in GYPSUM_TESTS]
[genrule(
    name = "test/%s_resource" % t,
    srcs = [":test/%s_package" % t],
    tools = [":test/mktest"],
    outs = ["test/%s_resource.cpp" % t],
    cmd = "python $(location :test/mktest) %s $< $@" % t,
) for _, t in GYPSUM_TESTS]

py_binary(
    name = "test/mktest",
    srcs = ["test/mktest.py"],
)

cc_test(
    name = "ApiByName",
    size = "small",
    srcs = ["test/ApiByName/ApiByName.cpp"],
    deps = [
        ":codeswitch",
        ":codeswitch_test_lib",
    ],
    data = [":test.ApiByName"],
    copts = [
        "-std=c++11",
        "-iquote", "codeswitch/src",
        "-iquote", "codeswitch/test",
    ],
    args = ["codeswitch", "std"],
)

gy_package(
    name = "test.ApiByName",
    package_name = "test.ApiByName",
    srcs = ["test/ApiByName/ApiByName.gy"],
    deps = ["//std"],
)

cc_test(
    name = "ApiElements",
    size = "small",
    srcs = ["test/ApiElements/ApiElements.cpp"],
    deps = [
        ":codeswitch",
        ":codeswitch_test_lib",
    ],
    data = [":test.ApiElements"],
    copts = [
        "-std=c++11",
        "-iquote", "codeswitch/src",
        "-iquote", "codeswitch/test",
    ],
    args = ["codeswitch", "std"],
)

gy_package(
    name = "test.ApiElements",
    package_name = "test.ApiElements",
    srcs = ["test/ApiElements/ApiElements.gy"],
    deps = ["//std"],
)

cc_test(
    name = "NativeCalls",
    size = "small",
    srcs = ["test/NativeCalls/NativeCalls.cpp"],
    deps = [
        ":codeswitch",
        ":codeswitch_test_lib",
        ":NativeCalls-static",
    ],
    data = [
        ":test.NativeCalls.sharedlib",
        ":test.NativeCalls.staticlib",
        ":test.NativeCalls.registered",
    ],
    copts = [
        "-std=c++11",
        "-iquote", "codeswitch/src",
        "-iquote", "codeswitch/test",
    ],
    args = ["codeswitch", "std"],
)

gy_package(
    name = "test.NativeCalls.sharedlib",
    package_name = "test.NativeCalls.sharedlib",
    srcs = ["test/NativeCalls/sharedlib.gy"],
    deps = ["//std"],
    native_lib = ":test.NativeCalls.sharedlib-native",
    testonly = 1,
)

cc_library(
    name = "test.NativeCalls.sharedlib-native",
    srcs = ["test/NativeCalls/sharedlib.cpp"],
    deps = [":codeswitch"],
    testonly = 1,
    linkstatic = 0,
)

gy_package(
    name = "test.NativeCalls.staticlib",
    package_name = "test.NativeCalls.staticlib",
    srcs = ["test/NativeCalls/staticlib.gy"],
    deps = ["//std"],
    testonly = 1,
)

cc_library(
    name = "NativeCalls-static",
    srcs = ["test/NativeCalls/staticlib.cpp"],
    deps = [":codeswitch"],
    testonly = 1,
)

gy_package(
    name = "test.NativeCalls.registered",
    package_name = "test.NativeCalls.registered",
    srcs = ["test/NativeCalls/registered.gy"],
    deps = ["//std"],
    testonly = 1,
)
