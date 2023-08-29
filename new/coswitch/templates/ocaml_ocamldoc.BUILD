# generated file - DO NOT EDIT

load("@rules_ocaml//build:rules.bzl", "ocaml_import")

ocaml_import(
    name       = "ocamldoc",
    version    = "[distributed with OCaml]",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "odoc_info.cma",
        "//conditions:default":         "odoc_info.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma        = "odoc_info.cma",
    # cmxa       = "odoc_info.cmxa",
    # cmo        = glob(["*.cmo"]),
    # cmx        = glob(["*.cmx"]),
    ofiles     = glob(["*.o"]),
    cmts       = glob(["*.cmt"]),
    cmtis      = glob(["*.cmti"]),
    srcs       = glob(["*.ml", "*.mli"]),
    all        = glob(["*.*"]),
    visibility = ["//visibility:public"],
)
