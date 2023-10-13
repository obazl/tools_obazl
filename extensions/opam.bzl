def _opam_repo_impl(repo_ctx):
    # print("TOOL root %s" % repo_ctx.attr.tool.workspace_root)

    # (seg1, sep, seg2) = repo_ctx.name.partition("~")
    # (version, sep, seg3) = seg2.partition("~")
    # (ext, sep, repo_name) = seg3.partition("~")

    #### download opam
    ## FIXME: install.sh only installs to /usr/local/bin
    ## We need to choose and download a binary directly
    ## from https://github.com/ocaml/opam/releases
    ## OR, download only and figure out how to chmod u+x
    res = repo_ctx.download(
        ["https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh"],
        output = "opam_install.sh",
        executable = True,
        # sha256 = "",
        # integrity = ""
    )

    res = repo_ctx.execute(
        ["./opam_install.sh", "--download-only"],
    )
    # print("rc: %s" % res.return_code)
    # print("stdout: %s" % res.stdout)
    # print("stderr: %s" % res.stderr)

    #HACK: we need to chmod u+x the downloaded file,
    # but we do not know the file name.
    # This is the only way I could find to do this.
    res = repo_ctx.execute(["ls"])
    files = res.stdout.splitlines()
    opambin = ""
    for f in files:
        if f.startswith("opam-"):
            print("FILE: %s" % f)
            opambin = f
            res = repo_ctx.execute(
                ["chmod", "-v", "u+x", opambin]
            )
            print("rc: %s" % res.return_code)
            print("stdout: %s" % res.stdout)
            print("stderr: %s" % res.stderr)

    repo_ctx.symlink(opambin, "opam")

    ## now run 'opam switch create ...' etc.
    # res = repo_ctx.read("./root/config")
    # print("rc: %s" % res.return_code)
    # print("stdout: %s" % res.stdout)
    # print("stderr: %s" % res.stderr)
    if True: # res.return_code != 0:
        repo_ctx.report_progress("Initializing opam root")
        res = repo_ctx.execute(
            ["./" + opambin, "init",
             "--root=./root",
             "--bare",
             "--no-setup", # don't update shell stuff
             "--no-opamrc",
             "--no" # answer no to q about modifying shell rcfiles
             ],
            # environment = {"OPAMROOT": "/tmp"}
        )
        print("rc: %s" % res.return_code)
        print("stdout: %s" % res.stdout)
        print("stderr: %s" % res.stderr)
    print("using existing opam root")

    repo_ctx.report_progress("Creating opam switch")
    # res = repo_ctx.execute(
    #     ["./" + opambin, "switch", "create", "4.14.0",
    #      "--root=./root"],
    #     # environment = {"OPAMROOT": "/tmp"}
    # )
    # print("rc: %s" % res.return_code)
    # print("stdout: %s" % res.stdout)
    # print("stderr: %s" % res.stderr)

    repo_ctx.file(
        "WORKSPACE.bazel",
        content = "# do not remove"
    )

    repo_ctx.file(
        "MODULE.bazel",
        content = """
module(
    name = "{repo_name}",
    version = "1.0.0",
    compatibility_level = 1,
)
""".format(repo_name = repo_ctx.name
           # repo_name
           )
    )

#     repo_ctx.file(
#         "var/BUILD.bzl",
#         content = """
# def _impl(ctx):
#     args = ctx.actions.args()
#     args.add("var")
#     args.add("--root=./root")
#     ctx.run(
#         executable = ctx.file._opam,
#         arguments = args,
#         outputs = outs
#     )
#     ## print to stdout???

# run_opam = rule(
#     implementation = _impl,
#     attrs = dict(
#         _opam = attr.label(default = "//bin:opam",
#             allow_single_file = True,
#             executable = True
#         )
# )
#         """
#     )

    repo_ctx.file(
        "var/BUILD.bazel",
        content = """
sh_binary(
    name = "var",
    srcs = ["//:var.sh"],
    data = ["//:opam"]
)
        """
    )

    repo_ctx.file(
        "var.sh",
        content = """
#!/bin/sh
set +x
./opam var --root=./root
        """,
        executable = True
    )

#     repo_ctx.file(
#         "var/BUILD.bazel",
#         content = """
# genrule(
#     name = "var",
#     cmd  = "
# )
#         """
#     )

    repo_ctx.file(
        "bin/BUILD.bazel",
        content = """
alias(
    name = "opam",
    actual = "//:opam",
    visibility = ["//visibility:public"]
)
        """
)

    repo_ctx.file(
        "BUILD.bazel",
        content = """
exports_files(["opam", "var.sh"])
# alias(
#     name = "opam",
#     actual = "{opambin}",
#     visibility = ["//visibility:public"]
# )

# load("@obazl_tools_cc//:MACROS.bzl", "repo_paths")

# PROD_REPOS = [
# ]

# repo_paths(
#     name = "repo_paths",
#     repos = PROD_REPOS
# )

# repo_paths(
#     name = "test_repo_paths",
#     repos = PROD_REPOS + [
#     ]
# )

cc_library(
        name = "test",
        srcs = ["foo.c"],
        # toolchains = [":repo_paths"],
        # copts = ["-I$(@)/FOO"]
)
""".format(opambin = opambin),
        executable = False,
    )

    repo_ctx.file(
        "foo.c",
        content = "int x;",
        executable = False,
    )

###############################
_opam_repo = repository_rule(
    implementation = _opam_repo_impl,
    attrs = {
        "tool": attr.label(
            allow_single_file = True,
            # default = "//new:coswitch",
            executable = True,
            cfg = "exec"
        )
        # "generating_repository": attr.string(default = "maven"),
        # "target_name": attr.string(),
    },
)

################

## TAG CLASSES
_install = tag_class(
    attrs = {
        "root": attr.string(),
        "opam_version": attr.string(),
        "switch": attr.string(),
        "compiler_version": attr.string(),
        "packages": attr.string_dict(),
        "_tool": attr.label(
            allow_single_file = True,
            default = "@obazl//new:coswitch",
            executable = True,
            cfg = "exec"
        )
    }
)

_register_toolchains = tag_class(
    attrs = {}
)

## EXTENSION IMPL
def _opam_impl(module_ctx):
    # print("OPAM EXTENSION")
    # print("TOOLBIN %s" % module_ctx.modules[0].install._tool)

    # collect artifacts from across the dependency graph
    artifacts = []

    register_toolchains = False
    print("Module ct: %s" % len(module_ctx.modules))
    for mod in module_ctx.modules:
        # register_toolchains may be called by multiple modules
        # first one wins
        if mod.tags.register_toolchains:
            register_toolchains = True

    if register_toolchains:
        print("REGISTERING TOOLCHAINS")
native.register_toolchains("@ocaml//toolchain/selectors/local:vmvm")
native.register_toolchains("@ocaml//toolchain/selectors/local:vmsys")
native.register_toolchains("@ocaml//toolchain/selectors/local:sysvm")
native.register_toolchains("@ocaml//toolchain/selectors/local:syssys")
native.register_toolchains("@ocaml//toolchain/selectors/local:_vm")
native.register_toolchains("@ocaml//toolchain/selectors/local:__")
native.register_toolchains("@ocaml//toolchain/profiles:sys-dev")
native.register_toolchains("@ocaml//toolchain/profiles:sys-dbg")
native.register_toolchains("@ocaml//toolchain/profiles:sys-opt")
native.register_toolchains("@ocaml//toolchain/profiles:vm-dev")
native.register_toolchains("@ocaml//toolchain/profiles:vm-dbg")
native.register_toolchains("@ocaml//toolchain/profiles:vm-opt")
native.register_toolchains("@ocaml//toolchain/profiles:default-dev")
native.register_toolchains("@ocaml//toolchain/profiles:default-dbg")
native.register_toolchains("@ocaml//toolchain/profiles:default-opt")

        # for install in mod.tags.install:
    #         print("root: %s" % install.root)
    #         print("opam_version: %s" % install.opam_version)
    #         print("switch: %s" % install.switch)
    #         print("compiler_version: %s" % install.compiler_version)
    #         print("packages: %s" % install.packages)

    #         print("TOOL: %s" % install._tool)
    #         # module_ctx.path(
    #         for pkg in install.packages:
    #             print("INSTALLING: %s" % pkg)
    #             # _opam_package(name = pkg,
    #             #               tool = install._tool)

        # artifacts += install.artifacts
        # artifacts += [_to_artifact(artifact) for artifact in mod.tags.artifact]

    # call out to the coursier CLI tool to resolve dependencies
    # output = module_ctx.execute(["coursier", "resolve", artifacts])
    # # output: exec_result, flds: return_code, stdout, stderr
    # repo_attrs = _process_coursier_output(output)

    # call repo rules to generate repos
    # for attrs in repo_attrs:
    #   http_file(**attrs)
    # _generate_hub_repo(name = "maven", repo_attrs)

    _opam_repo(name = "opam")

########################
opam = module_extension(
  implementation = _opam_impl,
  tag_classes = {
      "install": _install,
      "register_toolchains": _register_toolchains
  },
)
