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
        message="Packaging " + package_name
    )
