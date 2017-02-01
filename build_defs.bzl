# Bazel rules and macros

def python_dist(package_name,
                version,
                setup,
                srcs,
                data=(),
                manifest=None,
                visibility=None):
    """Runs a set.py script to create a Python source distribution archive.

    Args:
        package_name: the name of the Python package. This is also becomes the target name.
        version: the version of the Python package.
        srcs: Python source files in the package.
        data: resource files included in the package.
        manifest: a MANIFEST.in file, if there is one.
    """

    inputs = [setup] + srcs + data
    if manifest:
        inputs += [manifest]
    out = "%s-%s.tar.gz" % (package_name, version)
    cmd = ("python $(location %s) sdist --formats gztar -d $(@D)" % setup)
    native.genrule(
        name=package_name,
        srcs=inputs,
        outs=[out],
        cmd=cmd,
        message="Packaging " + package_name)


def _gy_package_impl(ctx):
    args = [
        "--package-name", ctx.attr.package_name,
        "--package-version", ctx.attr.version,
        "--output", ctx.outputs.out.path,
    ]
    inputs = []
    for dep in ctx.attr.deps:
        for file_ in dep.files:
            args.append("--depends")
            args.append(file_.path)
            inputs.append(file_)
    for src in ctx.files.srcs:
        args.append(src.path)
        inputs.append(src)
    args.extend(ctx.attr.flags)
    ctx.action(
        inputs=inputs,
        outputs=[ctx.outputs.out],
        arguments=args,
        executable=ctx.executable._gy_compiler,
        progress_message="Compiling Gypsum package %s" % ctx.outputs.out.path)
    runfiles = ctx.runfiles(files = [ctx.outputs.out], collect_data = True)
    return struct(
        label = ctx.label,
        files = set([ctx.outputs.out]),
        runfiles = runfiles,
    )


"""Builds a CodeSwitch package from Gypsum source code.

Note that the package name and version determine the output file name.

Args:
    name: label for the target.
    package_name: the CodeSwitch package name.
    version: the CodeSwitch package version.
    srcs: a list of Gypsum sources. The order will be preserved.
    deps: a list of CodeSwitch packages that the package being compiled depends on directly.
        Indirect dependencies do not need to be listed.
    native_lib: a label for a `cc_library` containing implementations of native functions
        in this package.
    flags: additional flags to pass to the Gypsum compiler.
"""
gy_package = rule(
    implementation = _gy_package_impl,
    attrs = {
        "package_name": attr.string(default="default"),
        "version": attr.string(default="1"),
        "srcs": attr.label_list(allow_files=True),
        "deps": attr.label_list(allow_files=True),
        "native_lib": attr.label(),
        "flags": attr.string_list(),
        "_gy_compiler": attr.label(
            executable=True, cfg="host", default=Label("//gypsum:compiler")),
    },
    outputs = {"out": "%{package_name}-%{version}.csp"},
)
