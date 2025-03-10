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

## providers

bazel run @obazl//inspect:providers --@obazl//tgt=//interop/ffi/case110:Main

output:

----
Providers for //interop/ffi/case110:Main
OCamlDepsProvider:
  afiles  depset([], order = "postorder")
  archives  depset([], order = "postorder")
  astructs  depset([], order = "postorder")
  cc_libs  [<generated file interop/ffi/case110/cclibs/libalpha.a>]
  cmi  <generated file interop/ffi/case110/__obazl/Main.cmi>
  ofiles  depset([<generated file interop/ffi/case110/cclibs/__obazl/Alpha.o>, <generated file interop/ffi/case110/__obazl/Main.o>], order = "postorder")
  paths  depset(["bazel-out/darwin-fastbuild-ST-0e351842cdbd/bin/interop/ffi/case110/cclibs/__obazl", "bazel-out/darwin-fastbuild-ST-0e351842cdbd/bin/interop/ffi/case110/__obazl"], order = "postorder")
  sigs  depset([<generated file interop/ffi/case110/cclibs/__obazl/Alpha.cmi>, <generated file interop/ffi/case110/__obazl/Main.cmi>], order = "postorder")
  srcs  depset([<source file interop/ffi/case110/main.ml>])
  structs  depset([<generated file interop/ffi/case110/cclibs/__obazl/Alpha.cmx>, <generated file interop/ffi/case110/__obazl/Main.cmx>], order = "postorder")
  to_json  <built-in method to_json of struct value>
  to_proto  <built-in method to_proto of struct value>
  xmo  True
CcInfo in //interop/ffi/case110:Main  lib[0][0].alwayslink == None
  lib[0][0].dynamic_library == None
  lib[0][0].interface_library == None
  lib[0][0].lto_bitcode_files == None
  lib[0][0].must_keep_debug: <built-in method must_keep_debug of LibraryToLink value>
  lib[0][0].objects: [<generated file interop/ffi/case110/cclibs/_objs/alpha/alpha_adapter.o>, <generated file interop/ffi/case110/cclibs/_objs/alpha/alpha.o>]
  lib[0][0].pic_lto_bitcode_files == None
  lib[0][0].pic_objects == None
  lib[0][0].pic_shared_non_lto_backends: <built-in method pic_shared_non_lto_backends of LibraryToLink value>
  lib[0][0].pic_static_library == None
  lib[0][0].resolved_symlink_dynamic_library == None
  lib[0][0].resolved_symlink_interface_library == None
  lib[0][0].shared_non_lto_backends: <built-in method shared_non_lto_backends of LibraryToLink value>
  lib[0][0].static_library: <generated file interop/ffi/case110/cclibs/libalpha.a>
----

or:

$ bazel cquery //interop/ffi/case110:Main --output=starlark --starlark:expr="[p for p in providers(target)]"

output:

["@rules_ocaml//ocaml/_providers:ocaml.bzl%OCamlModuleProvider", "@rules_ocaml//ocaml/_providers:ocaml.bzl%OCamlDepsProvider", "CcInfo", "FileProvider", "FilesToRunProvider", "OutputGroupInfo"]

