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
            print("nsrp: %s" % nsrp)
            print("nsrp.cmi: %s" % nsrp.cmi)
            print("nsrp.cmi.struct: %s" % nsrp.struct)
            print("nsrp.cmi.ofile: %s" % nsrp.ofile)
            print("nsop: %s" % nsop)

    tc = ctx.toolchains["@rules_ocaml//toolchain/type:std"]

    pfx = "__obazl/" ## for intermediate files

    mly = ctx.file.src
    # mly = ctx.actions.declare_file(pfx + ctx.file.src.basename)
    # ctx.actions.symlink(output=mly, target_file=ctx.file.src)

    mock_ml_name = paths.replace_extension(ctx.file.src.basename, ".mock.ml")
    mock_ml = ctx.actions.declare_file(pfx + mock_ml_name)

    inferred_mli_name = paths.replace_extension(ctx.file.src.basename,
                                                ".inferred.mli")
    inferred_mli = ctx.actions.declare_file(pfx + inferred_mli_name)

    # Step 1. 'menhir parser.mly --infer-write-query parser.mock.ml'
    args = ctx.actions.args()
    args.add(mly.path) # ctx.file.src.path)
    args.add("--base", "parser")
    args.add("--infer-write-query", mock_ml.path)

    ctx.actions.run(
        executable = ctx.file.tool.path,
        arguments = [args],
        inputs  = [mly],  ## ctx.file.src],
        outputs = [mock_ml],
        tools = [ctx.file.tool],
        mnemonic = "MenhirMock",
        progress_message = "menhir: generating mock",
    )

    ################################################################
    ## 2. 'ocamlc -I path/to/dep -i parser.mock.ml > parser.inferred.mli'
    args = ctx.actions.args()

    dep_dirs = [d.dirname for d in ctx.files.deps]
    action_inputs = [mock_ml] + ctx.files.deps
    # if OcamlNsResolverProvider in ctx.attr._ns_resolver:
    if hasattr(nsrp, "cmi"):
        nsrp = ctx.attr._ns_resolver[OcamlNsResolverProvider]
        dep_dirs.append(nsrp.cmi.dirname)
        action_inputs.append(nsrp.cmi)
        action_inputs.append(nsrp.struct)
        action_inputs.append(nsrp.ofile)
        args.add("-open", "LibMwe")

    args.add_all(dep_dirs, before_each="-I", uniquify = True)
    args.add("-i", mock_ml.path)
    args.add("-no-alias-deps")

    # dune args:
    # -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -w -39 -g -I .LibMwe.objs/byte -open LibMwe -short-paths -i ...

    ctx.actions.run_shell(
        arguments = [args],
        inputs  = action_inputs,
        outputs = [inferred_mli],
        tools   = [tc.compiler],
        command = tc.compiler.path + " $@ > " + inferred_mli.path,
        mnemonic = "ExtractMli",
        progress_message = "menhir: generating inferred.mli",
    )

    ################################################################
    ## 3. 'menhir parser.mly --infer-read-reply parser.inferred.mli'

    ## WARNING: menhir writes its output to ${PWD}, and does not seem to
    ## support any kind of -outdir option to redirect output. Since our
    ## outfiles are relative to the pkg dir, we must copy the output to
    ## that directory.
    ctx.actions.run_shell(
        inputs  = [mly, inferred_mli], # ctx.file.src
        # outputs = [mly_ml, mly_mli],
        outputs = ctx.outputs.outs,
        tools = [ctx.file.tool],
        command = " ".join([
            ctx.file.tool.path,
            mly.path,
            "--base", "parser",
            "--infer-read-reply", inferred_mli.path + ";",
            "cp parser.ml {dst};".format(dst=ctx.outputs.outs[0].dirname),
            "cp parser.mli {dst};".format(dst=ctx.outputs.outs[0].dirname),
        ]),
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
        src = attr.label(
            doc = "A single .mly menhir source file label",
            allow_single_file = [".mly"]
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
        opts = attr.string_list(
            doc = "Options"
        ),
        tool = attr.label(
            doc = "Bazel label of a menhir executable.",
            mandatory = True,
            allow_single_file = True,
            executable = True,
            cfg = "exec"
        ),
        ## top-down namespacing support
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
