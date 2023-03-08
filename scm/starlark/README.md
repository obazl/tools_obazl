# dune conversion

## ocamlc_flags, ocamlopt_flags, etc.

In OBazl, build parameters that depend on the compiler like this must
depend on the selected toolchain. Only way I can see to do this is to
define attributes and have the rule select based on tc.target.

