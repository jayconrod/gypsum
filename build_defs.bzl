# Bazel rules and macros common to the whole project.

def python_dist(package_name,
                package_version,
                setup,
                srcs,
                data=(),
                manifest=None,
                visibility=None,
                tags=None):
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
        name = package_name,
        srcs = inputs,
        outs = [out],
        cmd = cmd,
        message = "Packaging " + package_name,
        output_to_bindir = True,
        visibility = visibility,
        tags = tags
    )


_gy_provider = provider()


_gy_attrs = {
    "package_name": attr.string(default="default"),
    "package_version": attr.string(default="0"),
    "srcs": attr.label_list(allow_files=[".gy"]),
    "deps": attr.label_list(providers=[
        "gy",
        "transitive_pkg_dirs",
    ]),
    "data": attr.label_list(allow_files=True, cfg="data"),
    "native_lib": attr.label(cfg="target", providers=["cc"]),
    "flags": attr.string_list(),
    "_gy_compiler": attr.label(
        executable=True, cfg="host", default=Label("//gypsum:compiler")),
}


def _gy_library_impl(ctx):
    """Builds a CodeSwitch package from Gypsum source code.

    Note that the package name and version determine the output file name.

    Args:
        name: label for the target.
        package_name: the CodeSwitch package name.
        package_version: the CodeSwitch package version.
        srcs: a list of Gypsum sources. The order will be preserved.
        deps: a list of CodeSwitch packages that the package being compiled depends on directly.
            Indirect dependencies do not need to be listed.
        data: a list of labels for files that must be available at run-time.
        native_lib: a label for a `cc_library` containing implementations of native functions
            in this package.
        flags: additional flags to pass to the Gypsum compiler.
    """
    _compile_gy_package(ctx)
    return _prepare_gy_providers(ctx)


gy_library = rule(
    implementation = _gy_library_impl,
    attrs = _gy_attrs,
    outputs = {"pkg": "%{package_name}-%{package_version}.csp"},
    fragments = ["cpp"],
)


def _gy_binary_impl(ctx):
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
    # Build package
    _compile_gy_package(ctx)
    providers = _prepare_gy_providers(ctx)

    # Generate runner script.
    args = [ctx.file._codeswitch_cmd.short_path]
    for pkg_dir in providers.transitive_pkg_dirs:
        args += ["--package-path", "'%s'" % pkg_dir]
    args += ["--package", "'%s'" % ctx.attr.package_name]
    command = "exec " + " ".join(args)
    ctx.file_action(
        output = ctx.outputs.executable,
        content = command,
        executable = True
    )

    return providers


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
    fragments = ["cpp"],
)


def _compile_gy_package(ctx):
    args = [
        "--package-name", ctx.attr.package_name,
        "--package-version", ctx.attr.package_version,
        "--output", ctx.outputs.pkg.path,
    ]
    inputs = []
    for f in ctx.files.deps:
        args.append("--depends")
        args.append(f.path)
        inputs.append(f)
    args += [f.path for f in ctx.files.srcs]
    inputs += ctx.files.srcs
    args += ctx.attr.flags
    ctx.action(
        inputs = inputs,
        outputs = [ctx.outputs.pkg],
        arguments = args,
        executable = ctx.executable._gy_compiler,
        progress_message = "Compiling Gypsum package %s" % ctx.outputs.pkg.path,
    )


def _prepare_gy_providers(ctx):
    files = [ctx.outputs.pkg]
    pkg_dir_name = _short_dirname(ctx.outputs.pkg)
    transitive_pkg_dirs = set([pkg_dir_name])
    symlinks = {}

    package_name = ctx.attr.package_name
    package_version = ctx.attr.package_version
    package_file = [ctx.outputs.pkg]
    native_lib_file = None
    if ctx.attr.native_lib:
        native_lib_file = [f for f in ctx.files.native_lib if f.extension == "so"][0]
        files.append(native_lib_file)
        cpu = ctx.fragments.cpp.cpu
        lib_ext = "dylib" if cpu == "darwin" else "so"
        link_path = "%s/lib%s-%s.%s" % (pkg_dir_name, package_name, package_version, lib_ext)
        if link_path != native_lib_file.short_path:
            symlinks[link_path] = native_lib_file
    gy = _gy_provider(
        package_name = package_name,
        package_version = package_version,
        package_file = package_file,
        native_lib_file = native_lib_file,
    )

    if hasattr(ctx.attr, "_codeswitch_cmd"):
        files.append(ctx.file._codeswitch_cmd)
    runfiles = ctx.runfiles(
        files = files,
        collect_data = True,
        symlinks = symlinks)

    for dep in ctx.attr.deps:
        transitive_pkg_dirs += dep.transitive_pkg_dirs
        runfiles = runfiles.merge(dep.data_runfiles)

    return struct(
        gy = gy,
        transitive_pkg_dirs = transitive_pkg_dirs,
        runfiles = runfiles,
    )


def _short_dirname(f):
    short_path = f.short_path
    i = short_path.rfind("/")
    return short_path[:i] if i != -1 else "."
