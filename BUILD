# Top-level Bazel build targets

load(":build_defs.bzl", "python_dist")

python_dist(
    package_name = "gypsum",
    package_version = "0",
    setup = "setup.py",
    srcs = ["//gypsum:sources"],
    data = ["//gypsum:common"],
    tags = ["manual"],
)

exports_files(
    glob(["common/*"]),
    visibility = ["//visibility:public"],
)
