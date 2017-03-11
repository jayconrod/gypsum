load("//:build_defs.bzl", "gy_library")

package(default_visibility = ["//visibility:public"])

gy_library(
    name = "io",
    package_name = "std.io",
    package_version = "0",
    srcs = glob(["src/*.gy"]),
    deps = ["//std"],
    native_lib = ":std.io-native",
)

cc_library(
    name = "std.io-native",
    srcs = glob(["src/*.cpp"]),
    deps = ["//codeswitch:codeswitch"],
)
