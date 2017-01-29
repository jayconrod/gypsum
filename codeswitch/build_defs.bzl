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
