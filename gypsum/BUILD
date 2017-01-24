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

[py_test(
    name = test_file[:-3],
    size = "small",
    srcs = [
        test_file,
        "utils_test.py",
        ":sources",
    ],
    deps = GYPSUM_DEPS,
) for test_file in glob(["test_*.py"])]
