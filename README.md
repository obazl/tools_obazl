# OBazl Tools

A (small) collection of language-independent Bazel tools.

WORKSPACE.bazel:

```
git_repository(
    name = "obazl_tools_bazel",
    remote = "https://github.com/obazl/tools_bazel",
    branch = "main",
)
```

Tools:

* `tokenize` tokenizes a string. Default separator is whitespace.
  * `load( "@obazl_tools_bazel//tools/functions:strings.bzl", "tokenize")`
  * `tokenize("foo   bar") => ["foo", "bar"]`.
  * `tokenize("foo_bar", sep="_") => ["foo", "bar"]`.
```

