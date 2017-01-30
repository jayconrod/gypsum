load("//:build_defs.bzl", "gy_package")
load(":build_defs.bzl", "py_gen_file", "gy_test_name")

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
    name = "codeswitch_lib",
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
                "src/linux-x64/*"
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
    name = "codeswitch",
    srcs = ["programs/driver.cpp"],
    deps = [":codeswitch_lib"],
    copts = ["-std=c++11"],
    visibility = ["//visibility:public"],
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

GYPSUM_TESTS = [(f, gy_test_name(f)) for f in glob(["test/*.gy"], exclude=["test/std-io-length.gy"])]


cc_test(
    name = "codeswitch_test",
    size = "small",
    srcs = glob(["test/*.cpp", "test/*.h", "src/*.h"])
        + [":test/%s_resource" % t for (_, t) in GYPSUM_TESTS],
    deps = [":codeswitch_lib"],
    data = ["//std"],
    copts = CODESWITCH_COPTS + ["-iquote", "codeswitch/test"],
    args = ["-P", "std", "-nofork", "-verbose", "Dict"],
)

[gy_package(
    name = "test/%s_package" % t,
    package_name = "test." + t,
    srcs = [f],
    deps = ["//std"],
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
