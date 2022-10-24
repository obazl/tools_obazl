load("@bazel_skylib//lib:paths.bzl", "paths")

load("@rules_ocaml//ocaml:providers.bzl",
     "OcamlModuleMarker",
     "OcamlProvider",
     "OcamlVmRuntimeProvider")

load("@rules_ocaml//ocaml/_rules:impl_ccdeps.bzl",
     "extract_cclibs", "dump_CcInfo")

load("@rules_ocaml//ocaml/_functions:deps.bzl", "aggregate_deps")

# load(":impl_binary.bzl", "impl_binary")

# load("//ocaml/_transitions:in_transitions.bzl", "executable_in_transition")

# load(":options.bzl", "options", "options_binary")

load("@rules_ocaml//ocaml/_debug:colors.bzl", "CCRED", "CCYEL", "CCRESET")

def fdirname(f):
    return f.dirname

###############################
def _ppx_expect_test(ctx):

    debug = False

    # 1. generate and compile 'main' runner module:

    # 2. compile the test runner

    ## NOTES: we do not need any of the complicated machinery used for
    ## generic tests/executables: namespaces, ppxes, etc.

    tc = ctx.toolchains["@rules_ocaml//toolchain/type:std"]

    if tc.target == "vm":
        struct_extensions = ["cma", "cmo"]
    else:
        struct_extensions = ["cmxa", "cmx"]


    ## testing

    depsets = struct(
        deps = struct(
            sigs = [],
            structs = [],
            ofiles  = [],
            archives = [],
            # archives = struct(
            #     cma  = [],
            #     cmxa = []
            # ),
            afiles = [],
            astructs = [], # archived cmx structs, for linking
            cmts = [],
            paths  = [],
            jsoo_runtimes = [], # runtime.js files
        ),
        codeps = struct(
            sigs = [],
            structs = [],
            ofiles = [],
            archives = [],
            afiles = [],
            astructs = [],
            cmts = [],
            paths = [],
            jsoo_runtimes = [],
        ),
        ccinfos = []
    )

    for dep in ctx.attr.deps:
        depsets = aggregate_deps(ctx, dep, depsets)

    # archives_depset = depset(transitive = depsets.deps.archives)
    # print("archives_depset: %s" % archives_depset)
    # sorted_archives = []
    # for a in archives_depset.to_list():
    #     if a.extension not in ["cma", "cmxa"]:
    #         print("{c}arch: {a}{r}".format(
    #             c=CCRED, a=a.basename, r=CCRESET))
    #     else:
    #         sorted_archives.append(a.basename)
    # for a in sorted(sorted_archives):
    #     print("arch: %s" % a)

    # ccInfo = cc_common.merge_cc_infos(
    #     direct_cc_infos = ccinfos_primary,
    #     cc_infos = depsets.ccinfos
    # )
    # print("ccinfo merged: %s" % ccInfo)
    # dump_CcInfo(ctx, ccInfo)

    # jsoo_runtimes_depset = depset(
    #     direct = depsets.deps.jsoo_runtimes,
    #     transitive = depsets.codeps.jsoo_runtimes
    # )
    # print("jsoo_runtimes_depset: %s" % jsoo_runtimes_depset)
    # for a in sorted(jsoo_runtimes_depset.to_list()):
    #     print("jsoo: %s" % a)

    # fail("mmmm")

    ################################################################
    ## Write main module src files (ml plus empty mli)

    #TODO: unique name in case one pkg has multiple test runners
    test_runner_module = "Ppx_expect_test_runner_main"

    test_runner_sigfile = ctx.actions.declare_file(
        test_runner_module + ".mli")
    ctx.actions.write(
        output = test_runner_sigfile,
        content = "(* empty *)"
    )

    test_runner_structfile = ctx.actions.declare_file(
        test_runner_module + ".ml")
    ctx.actions.write(
        output = test_runner_structfile,
        content = "\n".join([
            "(* GENERATED FILE - DO NOT EDIT *)",
            "let () = Ppx_inline_test_lib.Runtime.exit ();;"
        ])
    )

    ################################################################
    ## Compile main module
    cc_deps_primary         = []
    cc_deps_secondary       = []

    # merge ctx.attr.deps and ctx.attr._deps
    hidden_deps = []
    #FIXME: use archives_secondary, sigs_secondary, etc.
    for dep in ctx.attr.deps:
        if OcamlProvider in dep:
            hidden_deps.append(dep[OcamlProvider].sigs)
            hidden_deps.append(dep[OcamlProvider].archives)
            if tc.target == "sys":
                hidden_deps.append(dep[OcamlProvider].afiles)
                hidden_deps.append(dep[OcamlProvider].astructs)
                hidden_deps.append(dep[OcamlProvider].ofiles)
        if CcInfo in dep:
            # print("CcInfo dep: %s" % dep)
            cc_deps_primary.append(dep[CcInfo])

    for dep in ctx.attr._deps:
        if OcamlProvider in dep:
            hidden_deps.append(dep[OcamlProvider].sigs)
            hidden_deps.append(dep[OcamlProvider].archives)
            if tc.target == "sys":
                hidden_deps.append(dep[OcamlProvider].afiles)
                hidden_deps.append(dep[OcamlProvider].astructs)
                hidden_deps.append(dep[OcamlProvider].ofiles)
        if CcInfo in dep:
            # print("CcInfo dep: %s" % dep)
            cc_deps_primary.append(dep[CcInfo])


    hidden_depset = depset(
        transitive = hidden_deps
    )

    action_inputs = depset(
        direct = [test_runner_structfile, test_runner_sigfile] ,
        transitive = hidden_deps
    )

    out_cmi_fname = test_runner_module + ".cmi"
    out_cmi = ctx.actions.declare_file(out_cmi_fname)
    action_outputs = [out_cmi]

    out_ofile = None
    if tc.target == "vm":
        out_struct_fname = test_runner_module + ".cmo"
    else:
        out_struct_fname = test_runner_module + ".cmx"
        out_ofile_fname = test_runner_module + ".o"
        out_ofile = ctx.actions.declare_file(out_ofile_fname)
        action_outputs.append(out_ofile)

    out_struct = ctx.actions.declare_file(out_struct_fname)
    action_outputs.append(out_struct)

    ################################
    args = ctx.actions.args()
    args.add("-I", test_runner_sigfile.dirname)

    # args.add_all(hidden_depset.to_list(),
    #              map_each = fdirname,
    #              before_each="-I",
    #              uniquify=True)

    for d in hidden_depset.to_list():
        if tc.target == "vm":
            if d.extension == "cma":
                args.add("-I", d.dirname)
                # args.add(d)
        else:
            if d.extension == "cmxa":
                args.add("-I", d.dirname)

    # print("xxxx: %s" % ctx.attr._deps)
    # for d in hidden_depset.to_list():
    #     args.add("-I", d.dirname)
    # fail("mmmmmmmmmmmmmmmm")
    # args.add("-o", out_struct)
    args.add("-c")
    args.add(test_runner_sigfile.path)
    # args.add("-impl")
    args.add(test_runner_structfile.path)

    ctx.actions.run(
        # env = env,
        executable = tc.compiler,
        arguments = [args],
        inputs = action_inputs,
        outputs = action_outputs,
        tools = [tc.compiler],
        mnemonic = "CompilePpxExpectTestRunner",
        progress_message = "{mode} compiling: {impl}".format(
            # to {ws}//{pkg}:{tgt}".format(
            mode = tc.host + ">" + tc.target,
            impl = out_struct.basename
            # rule=ctx.attr._rule,
            # ws  = "@" + ctx.label.workspace_name if ctx.label.workspace_name else "", ## ctx.workspace_name,
            # pkg = ctx.label.package,
            # tgt=ctx.label.name,
        )
    )

    # ### testing
    # out_exe = ctx.actions.declare_file("foo.exe")
    # # action_outputs.append(out_exe)
    # ctx.actions.write(
    #     output = out_exe,
    #     content = "\n".join([
    #         "(* GENERATED FILE - DO NOT EDIT *)",
    #         "let () = Ppx_inline_test_lib.Runtime.exit ();;"
    #     ])
    # )

    ################################################################
    # now link into executable
    ################################

    #FIXME: call impl_binary?

    out_exe = ctx.actions.declare_file(test_runner_module + ".exe")
    exe_outputs = [out_exe]

    args = ctx.actions.args()
    # args.add("-I", test_runner_sigfile.dirname)

    args.add_all([
        "-keep-locs",
        "-short-paths",
        "-strict-formats",
        "-strict-sequence",
        "-linkall"
    ])

    ## FIXME: separate ordinary ccdeps from ppx_codep ccdeps?
    ccInfo = cc_common.merge_cc_infos(
        cc_infos = cc_deps_primary + cc_deps_secondary)
    # dump_CcInfo(ctx, ccInfo)
    # fail("aaaa")

    # + codep_cc_deps_primary + codep_cc_deps_secondary)
    # if debug_cc: print("Merged CcInfo: %s" % ccInfo)

    # codeps_ccInfo = cc_common.merge_cc_infos(
    #     cc_infos = codep_cc_deps_primary + codep_cc_deps_secondary)

    ## extract cc_deps from merged CcInfo provider:
    [
        static_cc_deps, dynamic_cc_deps
    ] = extract_cclibs(ctx, ccInfo) ##  tc.linkmode, args, ccInfo)

    # if debug_cc:
    print("static_cc_deps:  %s" % static_cc_deps)
    print("dynamic_cc_deps: %s" % dynamic_cc_deps)

    ## we put -lfoo before -Lpath/to/foo, to avoid iterating twice
    cclib_linkpaths = []
    cc_runfiles = []

    ## NB: -cclib -lfoo is just for -custom linking!
    ## for std (non-custom) linking use -dllib

    runfiles_root = out_exe.path + ".runfiles"
    # print("runfiles_root: %s" % runfiles_root)
    ws_name = ctx.workspace_name
    # print("ws name: %s" % ws_name)

    if tc.target == "vm":

        # vmlibs =  lib/stublibs/dll*.so, set by toolchain
        # only needed for bytecode mode, else we get errors like:
        # Error: I/O error: dllbase_internalhash_types_stubs.so: No such
        # file or directory

        # may also get e.g.
        # Fatal error: cannot load shared library dllbase_internalhash_types_stubs
        # Reason: dlopen(dllbase_internalhash_types_stubs.so, 0x000A): tried: 'dllbase_internalhash_types_stubs.so' (no such file) ... etc.

        vmlibs = tc.vmlibs
        includes = []

        ## WARNING: both -dllpath and -I are required!
        # args.add("-ccopt", "-L" + tc.vmlibs[0].dirname)
        args.add("-dllpath", tc.vmlibs[0].dirname)
        args.add("-I", tc.vmlibs[0].dirname)

        # if debug:
        #     print("{c}vm_runtime:{r} {rt}".format(
        #         c=CCGRN,r=CCRESET, rt = ctx.attr.vm_runtime))
        #     print("vm_runtime[OcamlVmRuntimeProvider: %s" %
        #           ctx.attr.vm_runtime[OcamlVmRuntimeProvider])

        # if "ppx" in ctx.attr.tags or ctx.attr._rule == "ppx_executable":
            ## Currently we default to a custom runtime.
            ## See section 20.1.3 "Statically linking C code with OCaml code"
            ## https://v2.ocaml.org/manual/intfc.html#ss:staticlink-c-code
            ## and https://ocaml.org/manual/runtime.html

        # args.add("-custom")

        for cclib in dynamic_cc_deps:
            args.add("-dllpath", cclib.dirname)
            cc_runfiles.append(cclib)
            # args.add("-ccopt", "-L" + cclib.dirname)
            # args.add("-cclib", "-l" + cclib.basename)

        if ctx.attr.vm_runtime[OcamlVmRuntimeProvider].kind == "dynamic":
            for cclib in dynamic_cc_deps:
                # print("cclib.short_path: %s" % cclib.short_path)
                # print("cclib.dirname: %s" % cclib.dirname)

                linkpath = "%s/%s/%s" % (
                    runfiles_root, ws_name, cclib.short_path)

                # this is for build-time:
                includes.append(cclib.dirname)
                # and this is for run-time:
                includes.append(paths.dirname(linkpath))
                args.add("-dllpath", cclib.dirname)
                args.add("-dllpath", paths.dirname(cclib.short_path))
                # as is this:
                cc_runfiles.append(cclib)

                bn = cclib.basename[3:]
                bn = bn[:-3]
                # args.add("-dllib", "-l" + bn)

                # args.add("-cclib", "-l" + bn)
                # cclib_linkpaths.append("-L" + cclib.dirname)
                # cclib_linkpaths.append("-L" + paths.dirname(cclib.short_path))
                # includes.append(paths.dirname(linkpath))
                # includes.append(paths.dirname(cclib.short_path))
                # cc_runfiles.append(cclib)
                # fail("xxxxxxxxxxxxxxxx")

        elif ctx.attr.vm_runtime[OcamlVmRuntimeProvider].kind == "static":
            ## should not be any .so files???
            sincludes = []
            for dep in static_cc_deps:
                print("STATIC DEP: %s" % dep)
                args.add("-custom")
                args.add("-ccopt", dep.path)
                includes.append(dep.dirname)
                sincludes.append("-L" + dep.dirname)

                # args.add_all(sincludes, before_each="-ccopt", uniquify=True)
                # includes.append(cclib.dirname)
                # args.add(cclib.short_path)
    else: # tc.target == sys
        vmlibs = [] ## we never need vmlibs for native code
        ## this accomodates ml libs with cc deps
        ## e.g. 'base' depends on libbase_stubs.a
        for cclib in static_cc_deps:
            # print("STATIC DEP: %s" % dep)
            cclib_linkpaths.append("-L" + cclib.dirname)
            bn = cclib.basename[3:]
            bn = bn[:-2]
            args.add("-cclib", "-l" + bn) # cclib.basename)

    args.add_all(cclib_linkpaths, before_each="-ccopt", uniquify=True)

    deps = []
    for dep in ctx.attr.deps:
        deps.append(dep[OcamlProvider].archives)
        deps.append(dep[OcamlProvider].afiles)

    exe_inputs = depset(
        direct = action_outputs # from previous action
        # + ctx.attr.deps[OcamlProvider].archives,
        + static_cc_deps
        + dynamic_cc_deps,
        transitive = hidden_deps # from previous action
        + deps
    )

    for d in hidden_depset.to_list():
        if tc.target == "vm":
            if d.extension == "cma":
                args.add(d)
        else:
            if d.extension == "cmxa":
                args.add(d)

    args.add(out_struct) ## input

    args.add("-o", out_exe)

    ctx.actions.run(
        # env = env,
        executable = tc.compiler,
        arguments = [args],
        inputs = exe_inputs,
        outputs = exe_outputs,
        tools = [tc.compiler],
        mnemonic = "LinkPpxExpectTestRunner",
        progress_message = "{mode} linking: {impl}".format(
            # to {ws}//{pkg}:{tgt}".format(
            mode = tc.host + ">" + tc.target,
            impl = out_exe.basename
            # rule=ctx.attr._rule,
            # ws  = "@" + ctx.label.workspace_name if ctx.label.workspace_name else "", ## ctx.workspace_name,
            # pkg = ctx.label.package,
            # tgt=ctx.label.name,
        )
    )

    print("cc_runfiles: %s" % cc_runfiles)
    # fail("xxxx")
    #### RUNFILE DEPS ####
    rfiles = tc.vmlibs + cc_runfiles # + ctx.files.data
    # if ctx.attr.strip_data_prefixes:
    #     myrunfiles = ctx.runfiles(
    #         files = rfiles,
    #         symlinks = {dfile.basename : dfile for dfile in ctx.files.data}
    #     )
    # else:
    myrunfiles = ctx.runfiles(
        files = rfiles
    )

    defaultInfo = DefaultInfo(
        executable = out_exe,
        runfiles = myrunfiles
        # files = depset(
        #     direct = action_outputs
        # )
    )

    return defaultInfo

################################
# rule_options = options("ocaml")
# rule_options.update(options_binary())

##################
ppx_expect_test = rule(
    implementation = _ppx_expect_test,

    doc = """Test rule for running inline ppx_expect tests.
    """,

    attrs = dict(
        # rule_options,

        # test_id = attr.string(mandatory = True),

        deps = attr.label_list(
            doc = "Modules under test, instrumented using ppx_expect.",
            providers = [
                [OcamlProvider],
                [OcamlModuleMarker],
            ]
        ),

        _deps = attr.label_list(
            doc = "Modules under test, instrumented using ppx_expect.",
            default = [
                # "@opam_ppx_inline_test//lib/runtime-lib",
                "@opam_ppx_expect//lib/evaluator",
                # "@opam_ppx_expect//lib/config",
            ],
            providers = [
                [OcamlProvider],
                [OcamlModuleMarker],
            ]
        ),
        vm_runtime = attr.label(
            doc = "@ocaml_rules//cfg/runtime:dynamic (default), @ocaml_rules//cfg/runtime:static, or a custom ocaml_vm_runtime target label",
            default = "@rules_ocaml//cfg/runtime"
        ),

        _rule = attr.string( default = "ppx_expect_test" ),


        ## https://bazel.build/docs/integrating-with-rules-cc
        ## hidden attr required to make find_cpp_toolchain work:
        # _cc_toolchain = attr.label(
        #     default = Label("@bazel_tools//tools/cpp:current_cc_toolchain")
        # ),
    ),
    test = True,
    fragments = ["platform", "cpp"],
    host_fragments = ["platform",  "cpp"],
    toolchains = ["@rules_ocaml//toolchain/type:std",
                  "@rules_ocaml//toolchain/type:profile",
                  "@bazel_tools//tools/cpp:toolchain_type"]
)
