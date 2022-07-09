load("@rules_ocaml//ocaml:providers.bzl",
     "OcamlArchiveMarker",
     "OcamlImportMarker",
     "OcamlLibraryMarker",
     "OcamlModuleMarker",
     "OcamlProvider",
     "OcamlNsResolverProvider",
     "OcamlSignatureProvider")

load("@rules_ocaml//ppx:providers.bzl",
     "PpxCodepsProvider",
     "PpxExecutableMarker",
)

load("@rules_ocaml//ocaml/_rules:impl_ccdeps.bzl",
     "dump_compilation_context",
     "ccinfo_to_string")

load("@rules_ocaml//ocaml/_debug:colors.bzl",
     "CCRED", "CCGRN", "CCBLU", "CCMAG", "CCCYAN", "CCRESET")

#####################################################
def _inspect_out_transition_impl(settings, attr):
    # print(">>> INSPECT_OUT_TRANSITION: %s" % attr.name)
    return {
        "@rules_ocaml//ppx:stop": True,
        "@rules_ocaml//ppx/print": "text"
    }

################
inspect_out_transition = transition(
    implementation = _inspect_out_transition_impl,
    inputs = [],
    outputs = [
        "@rules_ocaml//ppx:stop",
        "@rules_ocaml//ppx/print"
    ]
)

################
def _write_codeps_file(ctx, provider, text):
    print("Provider: %s" % provider)
    INDENT = "  "

    tc = ctx.toolchains["@rules_ocaml//toolchain:type"]

    if tc.emitting == "native":
        text = text + "structs:\n"
        for struct in provider.structs.to_list():
            text = text + INDENT + struct.path + "\n"

    if tc.emitting == "native":
        text = text + "ofiles:\n"
        for ofile in provider.ofiles.to_list():
            text = text + INDENT + ofile.path + "\n"

    if tc.emitting == "native":
        text = text + "astructs:\n"
        for astruct in provider.astructs.to_list():
            text = text + INDENT + astruct.path + "\n"

    text = text + "sigs:\n"
    for sig in provider.sigs.to_list():
        text = text + INDENT + sig.path + "\n"

    if tc.emitting == "native":
        text = text + "afiles:\n"
        for afile in provider.afiles.to_list():
            text = text + INDENT + afile.path + "\n"

    text = text + "archives:\n"
    for archive in provider.archives.to_list():
        text = text + INDENT + archive.path + "\n"

    f = ctx.actions.declare_file("codeps.txt")
    ctx.actions.write(
        output  = f,
        content = text,
        is_executable = False
    )

    return f

################
def _write_providers_file(ctx, tgt, text):
    if OcamlProvider in tgt:
        provider = tgt[OcamlProvider]
        text = text + CCCYAN + "OcamlProvider:\n"
        for d in dir(provider):
            text = text + "  " + CCRED + d + CCRESET
            val = getattr(provider, d)
            text = text + "  " + str(val) + "\n"

    if PpxCodepsProvider in tgt:
        provider = tgt[PpxCodepsProvider]
        text = text + CCCYAN + "PpxCodepsProvider:\n"
        for d in dir(provider):
            text = text + "  " + CCRED + d + CCRESET
            val = getattr(provider, d)
            text = text + "  " + str(val) + "\n"

    if CcInfo in tgt:
        text = text + "{color}CcInfo in {lbl}{reset}".format(
            color = CCCYAN, reset = CCRESET, lbl=tgt.label)
        print("CCINFO: %s" % tgt[CcInfo])
        print("CCINFO: in rule %s" % tgt.label)
        dump_compilation_context(tgt[CcInfo])
        text = text + ccinfo_to_string(ctx, tgt[CcInfo])

    f = ctx.actions.declare_file("inspect.txt")
    ctx.actions.write(
        output  = f,
        content = text,
        is_executable = False
    )
    return f

################
def _write_import_providers_file(ctx, tgt, text):
    # tc = ctx.toolchains["@rules_ocaml//toolchain:type"]

    provider = tgt[OcamlProvider]
    INDENT = "  "
    text = text + "structs:\n"
    for struct in provider.structs.to_list():
        text = text + INDENT + struct.path + "\n"

    text = text + "ofiles:\n"
    for ofile in provider.ofiles.to_list():
        text = text + INDENT + ofile.path + "\n"

    text = text + "astructs:\n"
    for astruct in provider.astructs.to_list():
        text = text + INDENT + astruct.path + "\n"

    text = text + "sigs:\n"
    for sig in provider.sigs.to_list():
        text = text + INDENT + sig.path + "\n"

    text = text + "afiles:\n"
    for afile in provider.afiles.to_list():
        text = text + INDENT + afile.path + "\n"

    text = text + "archives:\n"
    for archive in provider.archives.to_list():
        text = text + INDENT + archive.path + "\n"

    if CcInfo in tgt:
        text = text + "ccdeps:\n"
        # for ccdep in tgt[CcInfo].to_list():
        text = text + INDENT + ccinfo_to_string(ctx, tgt[CcInfo]) + "\n"

    ################
    if PpxCodepsProvider in tgt:
        provider = tgt[PpxCodepsProvider]
        text = text + "PPX CODEPS\n"
        text = text + "ppx_codep structs:\n"
        for struct in provider.structs.to_list():
            print("XXXXXXXXXXXXXXXX: %s" % struct)
            text = text + INDENT + struct.path + "\n"

        text = text + "ppx_codep ofiles:\n"
        for ofile in provider.ofiles.to_list():
            text = text + INDENT + ofile.path + "\n"

        text = text + "ppx_codep astructs:\n"
        for astruct in provider.astructs.to_list():
            text = text + INDENT + astruct.path + "\n"

        text = text + "ppx_codep sigs:\n"
        for sig in provider.sigs.to_list():
            text = text + INDENT + sig.path + "\n"

        text = text + "ppx_codep afiles:\n"
        for afile in provider.afiles.to_list():
            text = text + INDENT + afile.path + "\n"

        text = text + "ppx_codep archives:\n"
        for archive in provider.archives.to_list():
            text = text + INDENT + archive.path + "\n"

        # text = text + "ppx_codep cclibs:\n"
        # for cclib in provider.cclibs.to_list():
        #     text = text + INDENT + cclib.path + "\n"


    f = ctx.actions.declare_file("inspect.txt")
    ctx.actions.write(
        output  = f,
        content = text,
        is_executable = False
    )

    return f

################################
def _inspect_impl(ctx):
    tool = None
    ppx  = False # hack
    is_import = False
    providers = False

    text = CCGRN + "Providers for " + str(ctx.attr.obj[0].label) + CCRESET + "\n"

    if ctx.label.package == "inspect":
        if ctx.label.name == "import":
            is_import = True
            print("ctx.attr.obj: %s" % ctx.attr.obj)
            tmp = ctx.attr.obj[0]
            objs = []
            if OcamlImportMarker in tmp:
                print("OcamlImportMarker: %s" % tmp)
                tmpfile = _write_import_providers_file(ctx, tmp, text)
                tool = "echo '{f}:'; cat".format(f=tmpfile.path)
            else:
                fail("Target is not ocaml_import")
        if ctx.label.name == "providers":
            providers = True
            print("ctx.attr.obj: %s" % ctx.attr.obj)
            tmp = ctx.attr.obj[0]
            tmpfile = _write_providers_file(ctx,tmp, text)
            tool = "echo '{f}:'; cat".format(f=tmpfile.path)

            # objs = []
            # if OcamlImportMarker in tmp:
            #     print("OcamlImportMarker: %s" % tmp)
            #     tmpfile = _write_import_providers_file(ctx, tmp)
            #     tool = "echo '{f}:'; cat".format(f=tmpfile.path)
            # else:
        elif ctx.label.name == "codeps":
            ## get provider, dump to file, show file
            tmp = ctx.attr.obj[0]
            if PpxCodepsProvider in tmp:
                provider = tmp[PpxCodepsProvider]
                tmpfile = _write_codeps_file(ctx, provider, text)
                tool = "echo '{f}:'; cat".format(f=tmpfile.path)
                ppx = True
            else:
                fail("Target does not carry ppx codeps")
        elif ctx.label.name == "ppx":
            print("DefaultInfo: %s" % ctx.attr.obj[0][DefaultInfo])
            objs = ctx.attr.obj[0][DefaultInfo].files.to_list()
            tool = "echo '{f}:'; cat".format(f=objs[0].path)
            print("TOOL %s" % tool)
        elif ctx.label.name == "sig":
            tool = ctx.executable._tool.path
            objs = ctx.attr.obj[OutputGroupInfo].cmi.to_list()
        elif ctx.label.name == "struct":
            tool = ctx.executable._tool.path
            if OcamlModuleMarker in ctx.attr.obj:
                objs = ctx.attr.obj[DefaultInfo].files.to_list()
            else:
                fail("No struct for inspect target")
        elif ctx.label.name == "src":
            if OcamlLibraryMarker in ctx.attr.obj:
                fail("No srcfile in ocaml_library target: %s" % ctx.attr.obj)
            elif OcamlNsResolverProvider in ctx.attr.obj:
                objs = [ctx.attr.obj[OcamlNsResolverProvider].resolver_file]
            else:
                if OcamlProvider in ctx.attr.obj:
                    objs = [ctx.attr.obj[OcamlProvider].srcs.to_list()[0]]
                else:
                    fail("No srcfile in inspect target: %s" % ctx.attr.obj)
            tool = "echo '{f}:'; cat".format(f=objs[0].path)

    elif ctx.label.package == "sig":
        tool = ctx.executable._tool.path
        objs = ctx.attr.obj[OutputGroupInfo].cmi.to_list()
    elif ctx.label.package == "struct":
        tool = ctx.executable._tool.path
        if OcamlModuleMarker in ctx.attr.obj:
            objs = ctx.attr.obj[DefaultInfo].files.to_list()
        else:
            fail("No struct for sig target")

    if ppx or is_import or providers:
        out = ctx.actions.declare_file("inspect.sh")
        runfiles = ctx.runfiles(
            files = [tmpfile]
        )
        cmd = " ".join([
            "{tool} `pwd`/{obj}".format(
                tool = tool, obj = tmpfile.short_path),
        ])
    else:
        out = ctx.actions.declare_file("inspect.sh")
        runfiles = ctx.runfiles(
            files = objs # + tool
        )
        cmd = " ".join([
            "{tool} `pwd`/{obj}".format(
                tool = tool, obj = objs[0].short_path),
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

###############
inspect = rule(
    implementation = _inspect_impl,
    doc = "Use ocamlobjinfo to inspect object. Pass label of object to inspect like this: bazel run @obazl//sig:inspect --@obazl//ocamlobj=//pkg:tgt",
    executable = True,
    attrs = dict(
        obj = attr.label(
            doc = "Label of object to inspect; must be ocaml_module or ocaml_signature target.",
            mandatory = True,
            providers = [
                [OcamlArchiveMarker],
                [OcamlLibraryMarker],
                [OcamlImportMarker],
                [OcamlModuleMarker],
                [OcamlNsResolverProvider],
                [OcamlSignatureProvider],
                [PpxExecutableMarker],
                [CcInfo]
            ],
            ## transition fn: enable ppx_print:text for inspect:ppx
            cfg = inspect_out_transition
        ),
        _allowlist_function_transition = attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist"
        ),

        _ocamlobjinfo = attr.label(
            executable = True,
            cfg = "exec",
            allow_files = True,
            default = Label("@ocaml//bin:ocamlobjinfo"),
        ),
        _tool = attr.label(
            executable = True,
            cfg = "exec",
            allow_files = True,
            default = Label("@ocaml//bin:ocamlobjinfo"),
        )
    ),
    toolchains = ["@rules_ocaml//toolchain:type"],
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
                [OcamlSignatureProvider],
            ]
        ),
        _tool = attr.label(
            executable = True,
            cfg = "exec",
            allow_files = True,
            default = Label("@ocaml//bin:cmitomli"),
        ),
        _cmitomli = attr.label(
            executable = True,
            cfg = "exec",
            allow_files = True,
            default = Label("@ocaml//bin:cmitomli"),
        )
    )
)
