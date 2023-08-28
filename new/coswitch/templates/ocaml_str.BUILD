# generated file - DO NOT EDIT

load("@opam//build:rules.bzl", "opam_import")

opam_import(
    name       = "str",
    version    = """[distributed with Ocaml]""",
    doc        = """Regular expressions and string processing""",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "str.cma",
        "//conditions:default":         "str.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma        = "str.cma",
    # cmxa       = "str.cmxa",
    # cmo        = glob(["*.cmo"]),
    # cmx        = glob(["*.cmx"]),
    ofiles     = glob(["*.o"]),
    cmts       = glob(["*.cmt"]),
    cmtis      = glob(["*.cmti"]),
    srcs       = glob(["*.ml", "*.mli"]),
    all        = glob(["str.*"]),
    visibility = ["//visibility:public"]
)

opam_import(
    name       = "plugin",
    plugin     =  select({
        "@ocaml//platforms/target:vm?": "str.cma",
        "//conditions:default":         "str.cmxs",
    }),
    # cmxs       = "str.cmxs",
    # cma        = "str.cma",
    visibility = ["//visibility:public"]
)
