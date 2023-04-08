##########
def script(name = "obazl", main = None, **kwargs):
    if main:
        args = ["-m", main]
    else:
        args = []

    native.cc_binary(
        name  = name,
        args  = args,
        data = [
            "//scm:srcs",
            "//scm/bazel:srcs"
            # these are already in runfiles, from dep @mibl//src:mibl
            # "@mibl//scm:srcs",
            # "@mibl//scm/dune:srcs",
            # "@mibl//scm/meta:srcs",
            # "@mibl//scm/opam:srcs",
    ],
        linkstatic = True,
        srcs  = ["obazl.c", "obazl.h"],
        defines = select({
            "//bzl/host:debug": ["DEBUG_TRACE"],
            "//conditions:default":   []
        }) + select({
            "//bzl/host:linux": ["_XOPEN_SOURCE=500"], # strdup
            "//conditions:default":   []
        }) + [
            "WITH_C_LOADER"
        ],
        copts = select({
            "//bzl/host:macos": ["-std=c11"],
            "//bzl/host:linux": ["-std=gnu11"],
            "//conditions:default": ["-std=c11"],
        }) + [
            "-Wall",
            "-Werror",
            "-Wpedantic",
            "-Wno-unused-function",

            "-I$(GENDIR)/obazl",
            "-I$(GENDIR)/external/obazl/obazl",

            "-I$(GENDIR)/external/mibl/src/hdrs", # libmibl.h
            "-Iexternal/mibl/vendored/gopt",
            "-Iexternal/mibl/vendored/libinih",
            "-Iexternal/mibl/vendored/logc",
            "-Iexternal/mibl/vendored/uthash",
            "-Iexternal/libs7/src", # loaded by @mibl
        ],
        linkopts = select({
            "//bzl/host:macos": [],
            "//bzl/host:linux": ["-ldl", "-lm"],
            "//conditions:default": {}
        }),
        deps = [
            "@mibl//src:mibl",
            "@mibl//vendored/gopt",
            "@mibl//vendored/libinih:inih",
            "@mibl//vendored/logc",
            "@mibl//vendored/uthash",
        ],
        visibility = ["//visibility:public"]
    )


