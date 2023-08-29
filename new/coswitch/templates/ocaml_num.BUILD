# generated file - DO NOT EDIT

load("@rules_ocaml//build:rules.bzl", "ocaml_import")

ocaml_import(
    name       = "core",
    version    = "[distributed with OCaml]",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "nums.cma",
        "//conditions:default":         "nums.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma        = "nums.cma",
    # cmxa       = "nums.cmxa",
    # cmo        = glob(["*.cmo"]),
    # cmx        = glob(["*.cmx"]),
    ofiles     = glob(["*.o"]),
    cmts       = glob(["*.cmt"]),
    cmtis      = glob(["*.cmti"]),
    srcs       = glob(["*.ml", "*.mli"]),
    all        = glob(["*.*"]),
    visibility = ["//visibility:public"],
)

ocaml_import(
    name       = "plugin",
    version    = "[distributed with OCaml]",
    plugin     =  select({
        "@ocaml//platforms/target:vm?": "nums.cma",
        "//conditions:default":         "nums.cmxs",
    }),
    # cmxs       = "nums.cmxs",
    # cma        = "nums.cma",
    visibility = ["//visibility:public"],
);
