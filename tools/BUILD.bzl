load("@rules_ocaml//ocaml:providers.bzl",
     "OcamlModuleMarker",
     "OcamlSignatureProvider")

################
def _inspect_impl(ctx):
    if ctx.label.package == "sig":
        objs = ctx.attr.obj[OutputGroupInfo].cmi.to_list()
    elif ctx.label.package == "struct":
        if OcamlModuleMarker in ctx.attr.obj:
            objs = ctx.attr.obj[DefaultInfo].files.to_list()
        else:
            fail("No struct for sig target")

    out = ctx.actions.declare_file("inspect.sh")

    runfiles = ctx.runfiles(
        files = objs + [ctx.executable._tool]
    )

    cmd = " ".join([
        ctx.executable._tool.path,
        "`pwd`/{}".format(objs[0].short_path),
    ])
    # print("CMD: %s" % cmd)

    ctx.actions.write(
        output  = out,
        content = cmd,
        is_executable = True,
    )

    defaultInfo = DefaultInfo(
        executable = out,
        runfiles   = runfiles
    )
    return defaultInfo

inspect = rule(
    implementation = _inspect_impl,
    doc = "Use ocamlobjinfo to inspect object",
    executable = True,
    attrs = dict(
        obj = attr.label(
            doc = "Label of object to inspect; must be ocaml_module or ocaml_signature target.",
            mandatory = True,
            providers = [
                [OcamlModuleMarker],
                [OcamlSignatureProvider]
            ]
        ),
        _tool = attr.label(
            executable = True,
            cfg = "exec",
            allow_files = True,
            default = Label("@ocaml//bin:ocamlobjinfo"),
        )
    )
)

################################################################
def _extract_impl(ctx):
    if ctx.label.package == "sig":
        # if OcamlSignatureProvider in ctx.attr.obj:
        #    print("extracting from ocaml_signature target")
        # else:
        #    print("extracting from ocaml_module target")
        objs = ctx.attr.obj[OutputGroupInfo].cmi.to_list()
    elif ctx.label.package == "struct":
            objs = ctx.attr.obj[DefaultInfo].files.to_list()

    out = ctx.actions.declare_file("extract.sh")

    runfiles = ctx.runfiles(
        files = objs + [ctx.executable._tool]
    )

    cmd = " ".join([
        ctx.executable._tool.path,
        "`pwd`/{}".format(objs[0].short_path),
    ])
    print("CMD: %s" % cmd)

    ctx.actions.write(
        output  = out,
        content = cmd,
        is_executable = True,
    )

    defaultInfo = DefaultInfo(
        executable = out,
        runfiles   = runfiles
    )
    return defaultInfo

extract = rule(
    implementation = _extract_impl,
    doc = "Use cmitomli to extract sigfile from cmi file",
    executable = True,
    attrs = dict(
        obj = attr.label(
            doc = "Label of ocaml_module target. Interface code will be inferred from module output (*.cmo or *.cmx)",
            mandatory = True,
            providers = [
                [OcamlModuleMarker],
                [OcamlSignatureProvider]
            ]
        ),
        _tool = attr.label(
            executable = True,
            cfg = "exec",
            allow_files = True,
            default = Label("@ocaml//bin:cmitomli"),
        )
    )
)
