"""Public definitions for OCaml rules.

All public OCaml rules imported and re-exported in this file.

Definitions outside this file are private unless otherwise noted, and
may change without notice.
"""

load("//ocaml/_rules:menhir.bzl", _menhir = "menhir")

menhir = _menhir

