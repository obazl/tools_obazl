
def xrule_stamp_template_impl(ctx):
    """ Generate a file from a template file and workspace status data.

See https://bazelbuild.github.io/rules_nodejs/stamping.html and https://docs.bazel.build/versions/master/user-manual.html#workspace_status.
"""
    debug = True
    # if (ctx.label.name == "snark0.cm_"):
    #     debug = True

    if debug:
        print("XRULE_STAMP_TEMPLATE target: %s" % ctx.label.name)

    print("OUTPUT: %s" % ctx.attr.output)
    outfile = ctx.actions.declare_file(ctx.attr.output.name)
    print("OUTFILE: %s" % outfile)

    # args = ["--stamp-info-file=%s" % f.path for f in (ctx.info_file, ctx.version_file)]
    print("INFO_FILE: %s" % ctx.info_file.path)
    print("VERSION_FILE: %s" % ctx.version_file.path)

    subslines = []
    i = 1
    for item in ctx.attr.substitutions.items():
        subslines.append(
            "    LINE=\"${{LINE//{key}/${{{val}}}}}\"".format(
                key = item[0].replace("{", "\\{").replace("}", "\\}"),
                val = item[1],
            ),
        )
        i = i + 1

    cmd = "\n".join([
        "#!/bin/sh",
        # ] + lines + [
        "exec <{}".format(ctx.info_file.path),
        "while read -r K V LINE",  # -r "backslash does not act as an escape char"
        "do",
        "    eval ${K}=$V",
        "done",
        "exec 3>{}".format(outfile.short_path),
        "exec <{}".format(ctx.file.template.short_path),
        "while read -r LINE",
        "do",
    # ] + subslines + [
        "    echo ${LINE} 1>&3",
        "done",
    ])

    # print(cmd)

    cmd = "cat {infile}".format(infile = ctx.info_file.path)

    ctx.actions.run_shell(
        command = cmd,
        outputs = [outfile],
        mnemonic = "StampTemplate",
        progress_message = "xrule_stamp_template: {}".format(ctx.file.template.basename),
    )

########################
# xrule_stamp_template = rule(
stamped_filegroup = rule(
    implementation = xrule_stamp_template_impl,
    attrs = dict(
        stamp = attr.bool(),
        output = attr.output(),
        template = attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
        substitutions = attr.string_dict(
            doc = "Keys are fields in template file. Values are keys in workspace status file (stable-status.txt), which will be looked up to find replacement strings. See https://bazelbuild.github.io/rules_nodejs/stamping.html and https://docs.bazel.build/versions/master/user-manual.html#workspace_status."
        ),
    ),
)
