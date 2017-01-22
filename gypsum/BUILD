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
