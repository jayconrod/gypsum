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


def _compile_gy_package(ctx, src_files, dep_files, flags, pkg_file):
    args = [
        "--package-name", ctx.attr.package_name,
        "--package-version", ctx.attr.version,
        "--output", ctx.outputs.out.path,
    ]
    inputs = []
    for f in dep_files:
        args.append("--depends")
        args.append(f.path)
        inputs.append(f)
    for f in src_files:
        args.append(f.path)
        inputs.append(f)
    args.extend(flags)
    ctx.action(
        inputs = inputs,
        outputs = [pkg_file],
        arguments = args,
        executable = ctx.executable._gy_compiler,
        progress_message = "Compiling Gypsum package %s" % pkg_file.path
    )


def _symlink(ctx, src, dst_path):
    dst = ctx.new_file(dst_path)
    command = ("pwd && mkdir -p $(dirname '%s') && ln -s $(readlink -e '%s') '%s'"
               % (dst.path, src.path, dst.path))
    ctx.action(
        inputs = [src],
        outputs = [dst],
        command = command,
        progress_message = "Creating link"
    )
    return dst


def _gy_package_impl(ctx):
    _compile_gy_package(ctx, ctx.files.srcs, ctx.files.deps, ctx.attr.flags, ctx.outputs.out)
    out_files = [ctx.outputs.out]
    run_files = [ctx.outputs.out]
    if ctx.attr.native_lib:
        native_lib_file = [f for f in ctx.files.native_lib if f.path.endswith(".so")][0]
        native_lib_name = "lib%s-%s.so" % (ctx.attr.package_name, ctx.attr.version)
        run_files.append(_symlink(ctx, native_lib_file, native_lib_name))
    return struct(
        label = ctx.label,
        files = set(out_files),
        runfiles = ctx.runfiles(files = run_files, collect_data = True),
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
