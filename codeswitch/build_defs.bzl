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


def _doxygen_archive_impl(ctx):
    doxyfile = ctx.files.doxyfile[0]
    out_file = ctx.outputs.out
    out_dir_path = out_file.short_path[:-len(".tar.gz")]
    commands = [
        "mkdir -p %s" % out_dir_path,
        "out_dir_path=$(readlink -e %s)" % out_dir_path,
        "pushd %s" % doxyfile.dirname,
        "sed -e \"s:@@OUTPUT_DIRECTORY@@:$out_dir_path/codeswitch-api/:\" <%s | doxygen -" %
            doxyfile.basename,
        "popd",
        "tar czf %s -C %s codeswitch-api" % (out_file.path, out_dir_path),
    ]
    ctx.action(
        inputs = ctx.files.srcs + [doxyfile],
        outputs = [out_file],
        command = " && ".join(commands),
    )


"""Generate a .tar.gz archive containing documentation using Doxygen.

Args:
    name: label for the generated rule. The archive will be "%{name}.tar.gz".
    doxyfile: configuration file for Doxygen
    srcs: source files the documentation will be generated from.
"""
doxygen_archive = rule(
    implementation = _doxygen_archive_impl,
    attrs = {
        "doxyfile": attr.label(mandatory = True, allow_files = True),
        "srcs": attr.label_list(mandatory = True, allow_files = True),
    },
    outputs = {
        "out": "%{name}.tar.gz",
    },
)


def gy_test_name(file_name):
    """Computes a unit test name, given a Gypsum file name.

    For example, given "test/map-list-example.gy", this returns "MapListExample".
    """
    base_name = file_name[file_name.index("/") + 1 : file_name.rindex(".")]
    words = base_name.split("-")
    name = "".join([w.capitalize() for w in words])
    return name


def _dirname(path):
    index = path.rfind("/")
    if index == -1:
        return "."
    else:
        return path[:index]
