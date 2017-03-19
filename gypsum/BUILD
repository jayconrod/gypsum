filegroup(
    name = "sources",
    srcs = glob(["*.py"], exclude=["test_*.py", "utils_test.py"]),
    visibility = ["//:__subpackages__"],
)

filegroup(
    name = "common",
    srcs = [
        "builtins.yaml",
        "flags.yaml",
        "opcodes.yaml",
    ],
    visibility = ["//:__subpackages__"],
)

GYPSUM_DEPS = ["@yaml//:yaml"]

py_binary(
    name = "compiler",
    srcs = ["__main__.py"],
    deps = [":gypsum"],
    data = [":common"],
    main = "__main__.py",
    visibility = ["//visibility:public"],
)

py_library(
    name = "gypsum",
    srcs = glob(["*.py"], exclude=["test_*.py", "utils_test.py", "__main__.py"]),
    deps = GYPSUM_DEPS,
    data = [":common"],
    visibility = ["//:__subpackages__"],
)

[py_test(
    name = test_file[:-3],
    size = "small",
    srcs = [
        test_file,
        "utils_test.py",
        ":sources",
    ],
    deps = GYPSUM_DEPS,
    data = [":common"],
) for test_file in glob(["test_*.py"])]
