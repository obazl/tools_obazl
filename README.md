# tools_obazl

A collection of Bazel tools, mainly for OCaml development using obazl_rules_ocaml.

WORKSPACE.bazel:

```
git_repository(
    name = "tools_bazel",
    remote = "https://github.com/obazl/tools_bazel",
    branch = "main",
)
```

Tools:

* `tokenize` tokenizes a string. Default separator is whitespace.
  * `load( "@tools_bazel//tools/functions:strings.bzl", "tokenize")`
  * `tokenize("foo   bar") => ["foo", "bar"]`.
  * `tokenize("foo_bar", sep="_") => ["foo", "bar"]`.
```

