def py_gen_file(name, script, data, out):
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
    base_name = file_name[file_name.index("/") + 1 : file_name.rindex(".")]
    words = base_name.split("-")
    name = "".join([w.capitalize() for w in words])
    return name
