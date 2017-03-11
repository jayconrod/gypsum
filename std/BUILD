load("//:build_defs.bzl", "gy_library")

package(default_visibility = ["//visibility:public"])

gy_library(
    name = "std",
    package_name = "std",
    package_version = "0",
    srcs = glob(["src/*.gy"]),
    flags = ["--no-std"],
)
