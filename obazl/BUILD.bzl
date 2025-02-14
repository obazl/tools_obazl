# load("//:BUILD.bzl",
#      "GOPT_VERSION",
#      "INIH_VERSION",
#      "LIBS7_VERSION",
#      "MIBL_VERSION",
#      "UTHASH_VERSION")

load("//cfg/cc:CONFIG.bzl",
     SRCS = "BASE_SRCS",
     DEPS = "BASE_DEPS",
     "BASE_INCLUDE_PATHS",
     "BASE_COPTS",
     LINKOPTS = "BASE_LINKOPTS",
     DEFINES = "BASE_DEFINES")

COPTS = BASE_COPTS + BASE_INCLUDE_PATHS

##########
def script(name = "obazl", main = None, **kwargs):
    if main:
        args = ["-m", main]
    else:
        args = []

    native.cc_binary(
        name  = name,
        args  = args,
        linkstatic = True,
        srcs  = SRCS + ["obazl.c", "obazl.h"],
        deps = DEPS + [
            "@gopt//lib:gopt",
            "@inih//:inih",
            "@uthash//lib:uthash",
            "@mibl//lib:mibl",
            # "@mibl//vendored/gopt",
            # "@mibl//vendored/libinih:inih",
            # "@mibl//vendored/logc",
            # "@mibl//vendored/uthash",
        ],
        copts = COPTS + [
            "-I$(GENDIR)/obazl",
            "-I$(GENDIR)/external/{}/obazl".format(
                native.repository_name()[1:]
            ),

            # "-Iexternal/libs7~{}/src".format(LIBS7_VERSION),
            # "-I$(GENDIR)/external/mibl~{}/src/hdrs".format(MIBL_VERSION),

            # "-Iexternal/gopt~{}/src".format(GOPT_VERSION),

            # "-Iexternal/inih~{}/src".format(INIH_VERSION),
            # "-Iexternal/mibl/vendored/libinih",

            # "-Iexternal/uthash~{}/src".format(UTHASH_VERSION),
            # "-Iexternal/mibl/vendored/uthash",

            # "-Iexternal/mibl/vendored/logc",
        ],
        # select({
        #     "//bzl/host:macos": ["-std=c11"],
        #     "//bzl/host:linux": ["-std=gnu11"],
        #     "//conditions:default": ["-std=c11"],
        # }) + [
        #     "-Wall",
        #     "-Werror",
        #     "-Wpedantic",
        #     "-Wno-unused-function",

        #     "-I$(GENDIR)/obazl",
        #     "-I$(GENDIR)/external/obazl/obazl",

        #     "-I$(GENDIR)/external/mibl/src/hdrs", # libmibl.h
        #     "-Iexternal/libs7/src", # loaded by @mibl

        #     # "-Iexternal/mibl/vendored/gopt",
        #     # "-Iexternal/mibl/vendored/libinih",
        #     # "-Iexternal/mibl/vendored/logc",
        #     # "-Iexternal/mibl/vendored/uthash",
        # ],
        linkopts = select({
            "//bzl/host:macos": [],
            "//bzl/host:linux": ["-ldl", "-lm"],
            "//conditions:default": {}
        }),
        defines = [
            "LOCAL_REPO=\\\"{}\\\"".format(
                native.repository_name()[1:])
        ],
        local_defines = select({
            "//bzl/host:debug": ["DEBUG_TRACE"],
            "//conditions:default":   []
        }) + select({
            "//bzl/host:linux": ["_XOPEN_SOURCE=500"], # strdup
            "//conditions:default":   []
        }) + [
            "WITH_C_LOADER"
        ],
        data = [
            "//scm:srcs",
            "//scm/bazel:srcs",
     # these are already in runfiles, from dep @mibl//lib:mibl
            # "@mibl//scm:srcs",
            # "@mibl//scm/dune:srcs",
            # "@mibl//scm/meta:srcs",
            # "@mibl//scm/opam:srcs",
        ],
        visibility = ["//visibility:public"]
    )


