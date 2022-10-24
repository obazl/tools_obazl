"""Public definitions for OCaml rules.

All public OCaml rules imported and re-exported in this file.

Definitions outside this file are private unless otherwise noted, and
may change without notice.
"""

load("//ocaml/_rules:menhir.bzl",
     _menhir = "menhir")

load("//ocaml/_rules:ppxlib_executable.bzl",
     _ppxlib_executable = "ppxlib_executable")

load("//ocaml/_rules:ppx_expect_test.bzl",
     _ppx_expect_test = "ppx_expect_test")

menhir = _menhir
ppxlib_executable = _ppxlib_executable
ppx_expect_test = _ppx_expect_test
