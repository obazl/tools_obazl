# generated file - DO NOT EDIT

load("@rules_ocaml//build:rules.bzl", "ocaml_import")

ocaml_import(
    name       = "common",
    doc        = """Common compiler routines""",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "ocamlcommon.cma",
        "//conditions:default":         "ocamlcommon.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma        = "ocamlcommon.cma",
    # cmxa       = "ocamlcommon.cmxa",
    # cmo        = glob(["*.cmo"]),
    # cmx        = glob(["*.cmx"]),
    ofiles     = glob(["*.o"]),
    cmts       = glob(["*.cmt"]),
    cmtis      = glob(["*.cmti"]),
    srcs       = glob(["*.ml", "*.mli"]),
    all        = glob(["*"]),
    visibility = ["//visibility:public"]
)

ocaml_import(
    name       = "bytecomp",
    doc        = """Common compiler routines""",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "ocamlbytecomp.cma",
        "//conditions:default":         "ocamlbytecomp.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma        = "ocamlbytecomp.cma",
    # cmxa       = "ocamlbytecomp.cmxa",
    # cmi        = glob(["*.cmi"]),
    # cmo        = glob(["*.cmo"]),
    # cmx        = glob(["*.cmx"]),
    ofiles     = glob(["*.o"]),
    cmts        = glob(["*.cmt"]),
    cmtis       = glob(["*.cmti"]),
    srcs       = glob(["*.ml", "*.mli"]),
    all        = glob(["*"]),
    visibility = ["//visibility:public"]
)

ocaml_import(
    name       = "optcomp",
    doc        = """optcomp compiler routines""",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "ocamloptcomp.cma",
        "//conditions:default":         "ocamloptcomp.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma        = "ocamloptcomp.cma",
    # cmxa       = "ocamloptcomp.cmxa",
    # cmi        = glob(["*.cmi"]),
    # cmo        = glob(["*.cmo"]),
    # cmx        = glob(["*.cmx"]),
    ofiles     = glob(["*.o"]),
    cmts        = glob(["*.cmt"]),
    cmtis       = glob(["*.cmti"]),
    srcs       = glob(["*.ml", "*.mli"]),
    all        = glob(["*"]),
    visibility = ["//visibility:public"]
)

ocaml_import(
    name = "toplevel",
    doc = """Toplevel interactions""",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "ocamltoplevel.cma",
        "//conditions:default":         "ocamltoplevel.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma  = "ocamltoplevel.cma",
    # cmxa = "ocamltoplevel.cmxa",
    # cmi  = glob(["*.cmi"]),
    # cmo  = glob(["*.cmo"]),
    # cmx  = glob(["*.cmx"]),
    ofiles = glob(["*.o"]),
    cmts        = glob(["*.cmt"]),
    cmtis       = glob(["*.cmti"]),
    srcs = glob(["*.ml", "*.mli"]),
    all = glob(["*.cmx", "*.cmi"]),
    deps = [":bytecomp"],
    visibility = ["//visibility:public"]
)

# exports_files([
#     "ocamlcommon.cma",
#     "ocamlcommon.cmxa",
#     "ocamlcommon.a",

#     "ocamlbytecomp.cma",
#     "ocamlbytecomp.cmxa",
#     "ocamlbytecomp.a",

#     "ocamloptcomp.cma",
#     "ocamloptcomp.cmxa",
#     "ocamloptcomp.a",

#     "ocamlmiddleend.cma",
#     "ocamlmiddleend.cmxa",
#     "ocamlmiddleend.a",

#     "ocamltoplevel.cma",
#     "ocamltoplevel.cmxa",
#     "ocamltoplevel.a",
# ])

# filegroup(
#     name = "all",
#     srcs = glob([
#         "*.cmx", "*.o", "*.cmo",
#         "*.cmt", "*.cmti",
#         "*.mli", "*.cmi",
#     ]),
#     visibility = ["@ocaml//compiler-libs:__subpackages__"]
# )
