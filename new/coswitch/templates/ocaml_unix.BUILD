# generated file - DO NOT EDIT

load("@opam//build:rules.bzl", "opam_import")

opam_import(
    name       = "unix",
    version    = "[distributed with OCaml]",
    sigs       = glob(["*.cmi"]),
    archive    =  select({
        "@ocaml//platforms/target:vm?": "unix.cma",
        "//conditions:default":         "unix.cmxa",
    }),
    afiles     = glob(["*.a"]),
    astructs   = glob(["*.cmx"]),
    # cma        = "unix.cma",
    # cmxa       = "unix.cmxa",
    # cmo        = glob(["*.cmo"]),
    # cmx        = glob(["*.cmx"]),
    ofiles     = glob(["*.o"]),
    cmts       = glob(["*.cmt"]),
    cmtis      = glob(["*.cmti"]),
    srcs       = glob(["*.ml", "*.mli"]),
    all        = glob(["unix*.*"]),
    visibility = ["//visibility:public"],
)

opam_import(
    name       = "plugin",
    version    = "[distributed with OCaml]",
    cmxs       = "unix.cmxs",
    # cma        = "unix.cma",
    visibility = ["//visibility:public"],
);
