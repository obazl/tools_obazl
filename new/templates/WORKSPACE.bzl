load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")

OBAZL_BRANCH = "alpha"

all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""

def fetch_repos():

    maybe(
        http_archive,
        name = "bazel_skylib",
        urls = [
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.1.1/bazel-skylib-1.1.1.tar.gz",
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.1.1/bazel-skylib-1.1.1.tar.gz",
        ],
        sha256 = "c6966ec828da198c5d9adbaa94c05e3a1c7f21bd012a0b29ba8ddbccb2c93b0d",
    )

    maybe(
        git_repository,
        name = "rules_ocaml",
        remote = "https://github.com/obazl/rules_ocaml",
        branch = OBAZL_BRANCH
    )

    maybe(
        git_repository,
        name = "opam",
        remote = "https://github.com/obazl/tools_opam",
        branch = OBAZL_BRANCH
    )

    maybe(
        git_repository,
        name = "obazl",
        remote = "https://github.com/obazl/tools_obazl",
        branch = OBAZL_BRANCH
    )

    maybe(
        git_repository,
        name = "rules_jsoo",
        remote = "https://github.com/obazl/rules_jsoo",
        branch = OBAZL_BRANCH
    )
