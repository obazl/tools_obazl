# generated file - DO NOT EDIT

load("@rules_ocaml//toolchain:BUILD.bzl", "toolchain_selector")

exports_files(glob(["*.bazel"]))

##########
toolchain_selector(
    name           = "__", # *>*
    toolchain      = "@ocaml//toolchain/adapters/local:syssys",
    visibility     = ["//visibility:public"],
)

##########
toolchain_selector(
    name           = "_vm", # *>vm
    toolchain      = "@ocaml//toolchain/adapters/local:sysvm",
    target_host_constraints  = ["@ocaml//platforms/target:vm?"],
    visibility     = ["//visibility:public"],
)

##########
toolchain_selector(
    name           = "syssys",
    toolchain      = "@ocaml//toolchain/adapters/local:syssys",
    build_host_constraints  = ["@ocaml//platforms/build:sys?"],
    target_host_constraints  = ["@ocaml//platforms/target:sys?"],
    visibility     = ["//visibility:public"],
)

##########
toolchain_selector(
    name                    = "sysvm",
    toolchain               = "@ocaml//toolchain/adapters/local:sysvm",
    build_host_constraints  = ["@ocaml//platforms/build:sys?"],
    target_host_constraints  = ["@ocaml//platforms/target:vm?"],
    visibility              = ["//visibility:public"],
)

##########
toolchain_selector(
    name                    = "vmsys",
    toolchain               = "@ocaml//toolchain/adapters/local:vmsys",
    build_host_constraints    = ["@ocaml//platforms/build:vm?"],
    target_host_constraints  = ["@ocaml//platforms/target:sys?"],
    visibility              = ["//visibility:public"],
)

##########
toolchain_selector(
    name                    = "vmvm",
    toolchain               = "@ocaml//toolchain/adapters/local:vmvm",
    build_host_constraints  = ["@ocaml//platforms/build:vm?"],
    target_host_constraints  = ["@ocaml//platforms/target:vm?"],
    visibility              = ["//visibility:public"],
)
