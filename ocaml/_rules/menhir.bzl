load("@bazel_skylib//lib:paths.bzl", "paths")
load("@rules_ocaml//ocaml:providers.bzl",
     "OcamlProvider",
     "OcamlNsResolverProvider"
     )

########  RULE:  MENHIR  ########

# This is a multi-action rule. Steps:

# 1. 'menhir parser.mly --infer-write-query parser.mock.ml'
# 2. 'ocamlc -I path/to/dep -i parser.mock.ml > parser.inferred.mli'
# 3. 'menhir parser.mly --infer-read-reply parser.inferred.mli'

# Output is parser.ml, parser.mli

# 'tool' attribute: label of a menhir executable

######################
def _menhir_impl(ctx):

    debug = True
    if debug:
        print("MENHIR TARGET: %s" % ctx.label.name)
        print("menhir tool: %s" % ctx.file.tool)
        print("menhir outs: %s" % ctx.outputs.outs)
        print("menhir ns resolver: %s" % ctx.attr._ns_resolver)

    nsrp = ctx.attr._ns_resolver[OcamlNsResolverProvider]

    if hasattr(nsrp, "cmi"):
        if debug: print("has _ns_resolver")
        nsop = ctx.attr._ns_resolver[OcamlProvider]
        if debug:
            # print("nsrp: %s" % nsrp)
            print("nsrp.cmi: %s" % nsrp.cmi)
            print("nsrp.cmi.struct: %s" % nsrp.struct)
            print("nsrp.cmi.ofile: %s" % nsrp.ofile)
            # print("nsop: %s" % nsop)

    tc = ctx.toolchains["@rules_ocaml//toolchain/type:std"]

    pfx = "__obazl/" ## for intermediate files

    mly = ctx.files.grammars

    if len(ctx.files.grammars) > 1:
        base_grammar = ctx.attr.base
    else:
        (base_grammar, extension) = paths.split_extension(
            ctx.files.grammars[0].basename)

    # mly = ctx.actions.declare_file(pfx + ctx.file.grammars.basename)
    # ctx.actions.symlink(output=mly, target_file=ctx.file.grammars)

    mock_ml_name = paths.replace_extension(base_grammar, ".mock.ml")
    mock_ml = ctx.actions.declare_file(pfx + mock_ml_name)

    inferred_mli_name = paths.replace_extension(base_grammar, ".inferred.mli")
    inferred_mli = ctx.actions.declare_file(pfx + inferred_mli_name)

    ################################################################
    # Step 1. 'menhir parser.mly --infer-write-query parser.mock.ml'
    args = ctx.actions.args()
    args.add_all([g.path for g in ctx.files.grammars])

    if len(ctx.files.grammars) > 1:
        if ctx.attr.base:
            args.add("--base", ctx.attr.base)
        else:
            fail("Attribute 'base' must be used if multiple src files listed")

    if ctx.attr.token:
        args.add("--external-tokens",
                 ctx.attr.token[OcamlProvider].submodule)

    if ctx.attr.tokens_unused:
        args.add_all(ctx.attr.tokens_unused,
                     before_each="--unused-token", uniquify = True)

    if ctx.attr.opts:
        args.add_all(ctx.attr.opts)

    if ctx.attr.flags:
        args.add_all(ctx.attr.flags)

    args.add("--infer-write-query", mock_ml.path)

    action_inputs  = ctx.files.grammars
    action_outputs = [mock_ml]

    ctx.actions.run(
        executable = ctx.file.tool.path,
        arguments = [args],
        inputs  = action_inputs,
        outputs = action_outputs,
        tools = [ctx.file.tool],
        mnemonic = "MenhirMock",
        progress_message = "menhir: generating mock",
    )

    ################################################################
    ## 2. 'ocamlc -I path/to/dep -i parser.mock.ml > parser.inferred.mli'
    args = ctx.actions.args()

    # for d in ctx.attr.deps:
    #     print("MENHIR DEP: %s" % d[DefaultInfo])

    action_inputs = [mock_ml] + ctx.files.deps
    action_outputs = [inferred_mli]

    dep_dirs = [d.dirname for d in ctx.files.deps]
    if ctx.attr.token:
        action_inputs.extend(ctx.files.token)
        dep_dirs.extend([d.dirname for d in ctx.files.token])

    # if OcamlNsResolverProvider in ctx.attr._ns_resolver:
    if hasattr(nsrp, "cmi"):
        # print("NSRP: %s" % nsrp)
        nsrp = ctx.attr._ns_resolver[OcamlNsResolverProvider]
        dep_dirs.append(nsrp.cmi.dirname)
        action_inputs.append(nsrp.cmi)
        action_inputs.append(nsrp.struct)
        action_inputs.append(nsrp.ofile)
        args.add("-open", nsrp.module_name)

    args.add_all(dep_dirs, before_each="-I", uniquify = True)
    args.add("-i", mock_ml.path)
    args.add("-no-alias-deps")

    # dune args:
    # -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -w -39 -g -I .LibMwe.objs/byte -open LibMwe -short-paths -i ...

    ctx.actions.run_shell(
        arguments = [args],
        inputs  = action_inputs,
        outputs = action_outputs,
        tools   = [tc.compiler],
        command = tc.compiler.path + " $@ > " + inferred_mli.path,
        mnemonic = "ExtractMli",
        progress_message = "menhir: generating inferred.mli",
    )

    ################################################################
    ## 3. emit parser.ml, parser.mli
    ## 'menhir parser.mly --infer-read-reply parser.inferred.mli'

    action_inputs  = ctx.files.grammars + [inferred_mli]
    action_outputs = ctx.outputs.outs

    args = ctx.actions.args()

    # args.add(mly.path)
    args.add_all([g.path for g in ctx.files.grammars])

    if ctx.attr.base:
        args.add("--base", ctx.attr.base)

    dep_dirs = [d.dirname for d in ctx.files.deps]
    if ctx.attr.token:
        action_inputs.extend(ctx.files.token)
        dep_dirs.extend([d.dirname for d in ctx.files.token])

    # args.add_all(dep_dirs, before_each="-I", uniquify = True)

    if ctx.attr.token:
        args.add("--external-tokens",
                 ctx.attr.token[OcamlProvider].submodule)
    if ctx.attr.tokens_unused:
        args.add_all(ctx.attr.tokens_unused,
                     before_each="--unused-token", uniquify = True)
    if ctx.attr.opts:
        args.add_all(ctx.attr.opts)
    if ctx.attr.flags:
        args.add_all(ctx.attr.flags)
    args.add("--infer-read-reply", inferred_mli.path)

    cmd = "\n".join([
        ctx.file.tool.path + " $@;",
        "    cp {src} {dst};".format(
            src= ctx.outputs.outs[0].short_path,
            dst=ctx.outputs.outs[0].dirname),
        "    cp {src} {dst};".format(
            src = ctx.outputs.outs[1].short_path,
            dst=ctx.outputs.outs[1].dirname),
    ])

    # cmd = " ".join([
    #     ctx.file.tool.path,
    #     mly.path,
    #     "--table", "--explain",
    #     extern,
    #     "--base", ctx.attr.base,
    #     "--infer-read-reply", inferred_mli.path + ";",
    #     "cp {src} {dst};".format(
    #         src= ctx.outputs.outs[0].short_path,
    #         dst=ctx.outputs.outs[0].dirname),
    #     "cp {src} {dst};".format(
    #         src = ctx.outputs.outs[1].short_path,
    #         dst=ctx.outputs.outs[1].dirname),
    # ])

    ## WARNING: menhir writes its output to ${PWD} (?), and does not
    ## seem to support any kind of -outdir option to redirect output.
    ## Since our outfiles are relative to the pkg dir, we must copy
    ## the output to that directory.
    ctx.actions.run_shell(
        arguments = [args],
        inputs  = action_inputs,
        outputs = action_outputs,
        tools = [ctx.file.tool],
        command = cmd,
        mnemonic = "EmitParser",
        progress_message = "menhir: generating parser",
    )

    return [
        DefaultInfo(files = depset(
            direct = ctx.outputs.outs
        ))
    ]

#################
menhir = rule(
    implementation = _menhir_impl,
    doc = """Runs menhir.""",
    attrs = dict(
        grammars = attr.label_list(
            doc = "Menhir source files.",
            allow_files = [".mly"]
        ),
        deps = attr.label_list(
            doc = "List of OCaml dependencies.",
            # providers = [[OcamlArchiveMarker],
            #              [OcamlImportMarker],
            #              [OcamlLibraryMarker],
            #              [OcamlModuleMarker],
            #              [OcamlNsResolverProvider],
            #              [OcamlSignatureProvider]]
        ),
        outs = attr.output_list(
            doc = """Output filenames, .ml and .mli.""",
            mandatory = True
        ),
        tool = attr.label(
            doc = "Bazel label of a menhir executable.",
            mandatory = True,
            allow_single_file = True,
            executable = True,
            cfg = "exec"
        ),
        base = attr.string(
            doc = "Equivalent to --base arg",
        ),
        cmly = attr.bool(
            doc = "Produce .cmly file",
            default = False
        ),
        explain = attr.bool(
            doc = "Produce basename.conflicts file",
            default = False
        ),
        # options
        flags = attr.string_list(
            doc = "Boolean flag options"
        ),
        opts = attr.string_list(
            doc = "Options of form --option val"
        ),
        tokens_unused = attr.string_list(
            doc = "--unused-tokens list"
        ),
        token = attr.label(
            doc = "--external-token value.",
        ),
        compile_errors = attr.label(
            doc = "--compile-errors switch"
        ),
        ## top-down namespacing support
        ns = attr.label(
        ),
        _ns_resolver = attr.label(
            doc = "NS resolver module for bottom-up namespacing",
            # allow_single_file = True,
            providers = [OcamlNsResolverProvider],
            default = "@rules_ocaml//cfg/ns:resolver",
        ),
    ),
    executable = False,
    toolchains = ["@rules_ocaml//toolchain/type:std"],
)
