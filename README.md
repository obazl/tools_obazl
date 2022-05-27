# tools_obazl

WARNING: currently under heavy development, do not use.

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

* `get_xdg(repo_ctx)` returns multival: home, XDG_CONFIG_HOME, XDG_CACHE_HOME, XDG_DATA_HOME


```

