# Bazel rules and macros common to the whole project.

def python_dist(package_name,
                package_version,
                setup,
                srcs,
                data=(),
                manifest=None,
                visibility=None):
    """Runs a setup.py script to create a Python source distribution archive.

    Args:
        package_name: the name of the Python package. This is also becomes the target name.
        package_version: the version of the Python package.
        srcs: Python source files in the package.
        data: resource files included in the package.
        manifest: a MANIFEST.in file, if there is one.
    """
    inputs = [setup] + srcs + data
    if manifest:
        inputs += [manifest]
    out = "%s-%s.tar.gz" % (package_name, package_version)
    cmd = ("python $(location %s) sdist --formats gztar -d $(@D)" % setup)
    native.genrule(
        name=package_name,
        srcs=inputs,
        outs=[out],
        cmd=cmd,
        message="Packaging " + package_name)


_shared_lib_exts = ["so", "dylib"]


_gy_attrs = {
    "package_name": attr.string(default="default"),
    "package_version": attr.string(default="0"),
    "srcs": attr.label_list(allow_files=True),
    "deps": attr.label_list(allow_files=True),
    "native_lib": attr.label(cfg="target"),
    "flags": attr.string_list(),
    "_gy_compiler": attr.label(
        executable=True, cfg="host", default=Label("//gypsum:compiler")),
}


def _gy_package_impl(ctx):
    # Build package
    pkg_file = ctx.outputs.pkg
    _compile_gy_package(ctx, ctx.files.srcs, ctx.files.deps, ctx.attr.flags, pkg_file)

    # Prepare runfiles.
    out_files = [pkg_file]
    runfile_links = {}
    if ctx.attr.native_lib:
        native_lib_file = [f for f in ctx.files.native_lib if f.extension in _shared_lib_exts][0]
        pkg_dir = _dirname(pkg_file.short_path)
        link_path = "%s/lib%s-%s.%s" % (
            pkg_dir, ctx.attr.package_name, ctx.attr.package_version, native_lib_file.extension)
        runfile_links[link_path] = native_lib_file
    runfiles = ctx.runfiles(
        files = out_files,
        symlinks = runfile_links,
        collect_data = True
    )

    return struct(
        label = ctx.label,
        files = set(out_files),
        runfiles = runfiles,
    )


"""Builds a CodeSwitch package from Gypsum source code.

Note that the package name and version determine the output file name.

Args:
    name: label for the target.
    package_name: the CodeSwitch package name.
    package_version: the CodeSwitch package version.
    srcs: a list of Gypsum sources. The order will be preserved.
    deps: a list of CodeSwitch packages that the package being compiled depends on directly.
        Indirect dependencies do not need to be listed.
    native_lib: a label for a `cc_library` containing implementations of native functions
        in this package.
    flags: additional flags to pass to the Gypsum compiler.
"""
gy_package = rule(
    implementation = _gy_package_impl,
    attrs = _gy_attrs,
    outputs = {"pkg": "%{package_name}-%{package_version}.csp"},
)


def _gy_binary_impl(ctx):
    # Build package
    pkg_file = ctx.outputs.pkg
    _compile_gy_package(ctx, ctx.files.srcs, ctx.files.deps, ctx.attr.flags, pkg_file)

    # Prepare runfiles.
    runfile_links = {}
    if ctx.attr.native_lib:
        native_lib_file = [f for f in ctx.files.native_lib if f.extension in _shared_lib_exts][0]
        pkg_dir = _dirname(pkg_file.short_path)
        link_path = "%s/%s-%s.%s" % (
            pkg_dir, ctx.attr.package_name, ctx.attr.package_version, native_lib_file.extension)
        runfile_links[link_path] = native_lib_file
    runfiles = ctx.runfiles(
        files = [ctx.file._codeswitch_cmd, pkg_file],
        symlinks = runfile_links,
        collect_data = True
    )

    # Generate runner script.
    dep_dirs = set([_dirname(d.short_path) for d in ctx.files.deps])
    command = "exec %s %s %s" % (
        ctx.file._codeswitch_cmd.short_path,
        " ".join(["-P " + d for d in dep_dirs]),
        pkg_file.short_path)
    ctx.file_action(
        output = ctx.outputs.executable,
        content = command,
        executable = True
    )

    return struct(
        label = ctx.label,
        files = set([pkg_file, ctx.outputs.executable]),
        runfiles = runfiles,
    )


"""Builds a CodeSwitch package from Gypsum source code and creates
an executable script to run it with CodeSwitch.

Args:
    name: label for the target.
    package_name: the CodeSwitch package name.
    package_version: the CodeSwitch package version.
    srcs: a list of Gypsum sources. The order will be preserved.
    deps: a list of CodeSwitch packages that the package being compiled depends on directly.
        Indirect dependencies do not need to be listed.
    native_lib: a label for a `cc_library` containing implementations of native functions
        in this package.
    flags: additional flags to pass to the Gypsum compiler.
"""
gy_binary = rule(
    implementation = _gy_binary_impl,
    executable = True,
    attrs = _gy_attrs + {
        "_codeswitch_cmd": attr.label(
            executable=True,
            allow_single_file=True,
            cfg="target",
            default=Label("//codeswitch:codeswitch_cmd")),
    },
    outputs = {
        "pkg": "%{package_name}-%{package_version}.csp",
    },
)


def _compile_gy_package(ctx, src_files, dep_files, flags, pkg_file):
    args = [
        "--package-name", ctx.attr.package_name,
        "--package-version", ctx.attr.package_version,
        "--output", ctx.outputs.pkg.path,
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


def _dirname(path):
    index = path.rfind("/")
    if index == -1:
        return "."
    else:
        return path[:index]
