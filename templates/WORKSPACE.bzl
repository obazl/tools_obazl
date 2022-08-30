load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")

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
        http_archive,
        name = "rules_ocaml",
        url = "https://github.com/obazl/rules_ocaml/archive/2c981ba4e8b7b590d04f8fedf328d824a407be1b.zip",
        sha256 = "1f4fe9e432c9d361f80e99c64a8687c9f1738c37c18800cfc552ae8d7b7c7b08",
        strip_prefix = "rules_ocaml-2c981ba4e8b7b590d04f8fedf328d824a407be1b"
    )

    maybe(
        http_archive,
        name = "opam",  ## do not change name
        url = "https://github.com/obazl/tools_opam/archive/refs/tags/v2.0.0-alpha-1.tar.gz",
        sha256 = "c0772de1319b6c2eae0144dab1047d91b9fb4e7bfc43beebe771c5e7071fc8c0",
        strip_prefix = "tools_opam-2.0.0-alpha-1"
    )

    maybe(
        http_archive,
        name = "obazl",  ## do not change name
        url = "https://github.com/obazl/tools_obazl/archive/352c4a1b39f19c8f377d0dafda9e6233cf3aee52.zip",
        strip_prefix = "tools_obazl-352c4a1b39f19c8f377d0dafda9e6233cf3aee52",
        sha256 = "936feae1df3d46eb7e297a252c1003eebf3780fd64e0fa3f17974b9e0a347a94"
    )

    maybe(
        git_repository,
        name = "libs7",
        remote = "https://github.com/obazl/libs7",
        branch = "dev"
    )

    maybe(
        git_repository,
        name = "mibl",
        remote = "https://github.com/obazl/mibl",
        branch = "dev"
    )
