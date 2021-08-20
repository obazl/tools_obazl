# notes

step 1: fetch all remotes: $ bazel sync

$ bazel query 'kind(local_repository, //external:*)'

$ bazel query 'kind(http_archive, //external:*)'

$ bazel query 'kind(.*, //external:*)'

$ ls $(bazel info output_base)/external/

## bazelizing remote repos

1. add `coq_repository` or `ocaml_repository` rules to
WORKSPACE.bazel. Omit the `strip_prefix` and `sha256` attributes.

2. $ bazel fetch @the_repo//...

3. `ls $(bazel info output_base)/external/the_repo` - this will show
what is needed for `strip_prefix`. add it to the *_repository rule.

4. step 2 will also show the sha256 for the downloaded file; add the attribute
