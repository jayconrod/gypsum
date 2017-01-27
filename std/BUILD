load("//:build_defs.bzl", "gy_package")

package(default_visibility = ["//visibility:public"])

gy_package(
    name = "std",
    package_name = "std",
    version = "1",
    srcs = glob(["src/*.gy"]),
    flags = ["--no-std"],
)
