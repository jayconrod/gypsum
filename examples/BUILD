load("//:build_defs.bzl", "gy_package")

[gy_package(
    name = source_file[:-3].replace("-", ""),
    package_name = source_file[:-3].replace("-", ""),
    srcs = [source_file],
    deps = ["//std"]
) for source_file in glob(["*.gy"])]
