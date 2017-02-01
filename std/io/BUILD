load("//:build_defs.bzl", "gy_package")

package(default_visibility = ["//visibility:public"])

gy_package(
    name = "io",
    package_name = "std.io",
    version = "1",
    srcs = glob(["src/*.gy"]),
    deps = ["//std"],
    native_lib = ":std.io-1",
)

cc_library(
    name = "std.io-1",
    srcs = glob(["src/*.cpp"]),
    deps = ["//codeswitch:codeswitch_lib"],
)
