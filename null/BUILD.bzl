load("@rules_ocaml//build:providers.bzl",
     "OCamlModuleProvider",
     "OCamlSignatureProvider")

def _ocaml_null_module_impl(ctx):
  return DefaultInfo() # OCamlModuleProvider()

ocaml_null_module = rule(
  implementation = _ocaml_null_module_impl,
)
