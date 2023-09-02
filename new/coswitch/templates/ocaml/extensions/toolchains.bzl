# def _toolchains_repo_impl(repo_ctx):
    # print("TOOL root %s" % repo_ctx.attr.tool.workspace_root)

    # (seg1, sep, seg2) = repo_ctx.name.partition("~")
    # (version, sep, seg3) = seg2.partition("~")
    # (ext, sep, repo_name) = seg3.partition("~")

###############################
# _toolchains_repo = repository_rule(
#     implementation = _opam_repo_impl,
#     attrs = {
#         "tool": attr.label(
#             allow_single_file = True,
#             # default = "//new:coswitch",
#             executable = True,
#             cfg = "exec"
#         )
#         # "generating_repository": attr.string(default = "maven"),
#         # "target_name": attr.string(),
#     },
# )

################

## TAG CLASSES
_register = tag_class(
    attrs = {}
)

## EXTENSION IMPL
def _toolchains_impl(module_ctx):
    print("OCAML TOOLCHAINS EXTENSION")

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

    # _opam_repo(name = "opam")

########################
opam = module_extension(
  implementation = _opam_impl,
  tag_classes = {
      "install": _install,
      "register_toolchains": _register_toolchains
  },
)
