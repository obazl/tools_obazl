load("@rules_ocaml//ocaml:providers.bzl",
     "OcamlModuleMarker",
     "OcamlSignatureProvider")

def _ocaml_null_module_impl(ctx):
  return DefaultInfo() # OcamlModuleMarker()

ocaml_null_module = rule(
  implementation = _ocaml_null_module_impl,
)
