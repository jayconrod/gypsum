load("//:build_defs.bzl", "gy_binary")

[gy_binary(
    name = source_file[:source_file.rindex(".")],
    package_name = source_file[:-3].replace("-", ""),
    srcs = [source_file],
    deps = ["//std"]
) for source_file in glob(["*.gy"])]
