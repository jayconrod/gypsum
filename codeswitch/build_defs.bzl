# Bazel rules and macros used by CodeSwitch.

def py_gen_file(name, script, data, out):
    """Generate a file using a Python script.

    Note that the script may not be used multiple times, since this macro creates a
    py_binary for it.

    Args:
        name: label for the generated file.
        script: the Python script to execute.
        data: data file provided to the script on the command line.
        out: output file.
    """
    script_name = "gen_" + name
    native.py_binary(
        name = script_name,
        srcs = [script],
        main = script,
        deps = ["@yaml//:yaml"],
    )
    native.genrule(
        name = name,
        srcs = [data],
        tools = [script_name],
        outs = [out],
        cmd = "python $(location :%s) $< $@" % script_name,
    )


def gy_test_name(file_name):
    """Computes a unit test name, given a Gypsum file name.

    For example, given "test/map-list-example.gy", this returns "MapListExample".
    """
    base_name = file_name[file_name.index("/") + 1 : file_name.rindex(".")]
    words = base_name.split("-")
    name = "".join([w.capitalize() for w in words])
    return name
