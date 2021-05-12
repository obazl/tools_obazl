#include <string.h>

#include "dune_constants.h"

#if EXPORT_INTERFACE
enum stanza_type_e {
    STANZA_EXECUTABLE,
    STANZA_LIBRARY,
    STANZA_RULE
};
#endif
EXPORT const char *stanza_name[256] = {
    [STANZA_EXECUTABLE] = "executable",
    [STANZA_LIBRARY]    = "library",
    [STANZA_RULE]       = "rule",
};

/* **************************************************************** */
#if EXPORT_INTERFACE
enum field_type_e {
    FIELD_ALIAS,
    FIELD_ALLOW_OVERLAPPING_DEPENDENCIES,
    FIELD_C_LIBRARY_FLAGS,
    FIELD_DEPS,
    FIELD_EMBED_IN_PLUGIN_LIBRARIES,
    FIELD_ENABLED_IF,
    FIELD_FALLBACK,
    FIELD_FILES,
    FIELD_FLAGS,
    FIELD_FORBIDDEN_LIBRARIES,
    FIELD_FOREIGN_ARCHIVES,
    FIELD_FOREIGN_STUBS,
    FIELD_INLINE_TESTS,
    FIELD_INSTALL_C_HEADERS,
    FIELD_JAVASCRIPT_FILES,
    FIELD_JS_OF_OCAML,
    FIELD_KIND,
    FIELD_LIBRARIES,
    FIELD_LIBRARY_FLAGS,
    FIELD_LINK_DEPS,
    FIELD_LINK_FLAGS,
    FIELD_LOCKS,
    FIELD_MODE,
    FIELD_MODES,
    FIELD_MODULES,
    FIELD_MODULES_WITHOUT_IMPLEMENTATION,
    FIELD_NAME,
    FIELD_NAMES,
    FIELD_NEW_PUBLIC_NAME,
    FIELD_NO_DYNLINK,
    FIELD_OCAMLC_FLAGS,
    FIELD_OCAMLOPT_FLAGS,
    FIELD_OLD_PUBLIC_NAME,
    FIELD_OPTIONAL,
    FIELD_PACKAGE,
    FIELD_PPX_RUNTIME_LIBRARIES,
    FIELD_PREPROCESS,
    FIELD_PREPROCESSOR_DEPS,
    FIELD_PRIVATE_MODULES,
    FIELD_PROMOTE,
    FIELD_PUBLIC_NAME,
    FIELD_PUBLIC_NAMES,
    FIELD_ROOT_MODULE,
    FIELD_SYNOPSIS,
    FIELD_VIRTUAL_DEPS,
    FIELD_WRAPPED,
};
#endif
EXPORT const char *field_name[256] = {
    [FIELD_ALIAS]                          = "alias",
    [FIELD_ALLOW_OVERLAPPING_DEPENDENCIES] = "allow_overlapping_dependencies",
    [FIELD_C_LIBRARY_FLAGS]                = "c_library_flags",
    [FIELD_DEPS]                           = "deps",
    [FIELD_EMBED_IN_PLUGIN_LIBRARIES]      = "embed_in_plugin_libraries",
    [FIELD_ENABLED_IF]                     = "enabled_if",
    [FIELD_FALLBACK]                       = "fallback",
    [FIELD_FILES]                          = "files",
    [FIELD_FLAGS]                          = "flags",
    [FIELD_FORBIDDEN_LIBRARIES]            = "forbidden_libraries",
    [FIELD_FOREIGN_ARCHIVES]               = "foreign_archives",
    [FIELD_FOREIGN_STUBS]                  = "foreign_stubs",
    [FIELD_INLINE_TESTS]                   = "inline_tests",
    [FIELD_INSTALL_C_HEADERS]              = "install_c_headers",
    [FIELD_JAVASCRIPT_FILES]               = "javascript_files",
    [FIELD_JS_OF_OCAML]                    = "js_of_ocaml",
    [FIELD_KIND]                           = "kind",
    [FIELD_LIBRARIES]                      = "libraries",
    [FIELD_LIBRARY_FLAGS]                  = "library_flags",
    [FIELD_LINK_DEPS]                      = "link_deps",
    [FIELD_LINK_FLAGS]                     = "link_flags",
    [FIELD_LOCKS]                          = "locks",
    [FIELD_MODE]                           =  "mode",
    [FIELD_MODES]                          = "modes",
    [FIELD_MODULES]                        = "modules",
    [FIELD_MODULES_WITHOUT_IMPLEMENTATION] = "modules_without_implementation",
    [FIELD_NAME]                           = "name",
    [FIELD_NAMES]                          = "names",
    [FIELD_NEW_PUBLIC_NAME]                = "new_public_name",
    [FIELD_NO_DYNLINK]                     = "no_dynlink",
    [FIELD_OCAMLC_FLAGS]                   = "ocamlc_flags",
    [FIELD_OCAMLOPT_FLAGS]                 = "ocamlopt_flags",
    [FIELD_OLD_PUBLIC_NAME]                = "old_public_name",
    [FIELD_OPTIONAL]                       = "optional",
    [FIELD_PACKAGE]                        = "package",
    [FIELD_PPX_RUNTIME_LIBRARIES]          = "ppx_runtime_libraries",
    [FIELD_PREPROCESS]                     = "preprocess",
    [FIELD_PREPROCESSOR_DEPS]              = "preprocessor_deps",
    [FIELD_PRIVATE_MODULES]                = "private_modules",
    [FIELD_PROMOTE]                        = "promote",
    [FIELD_PUBLIC_NAME]                    = "public_name",
    [FIELD_PUBLIC_NAMES]                   = "public_names",
    [FIELD_ROOT_MODULE]                    = "root_module",
    [FIELD_SYNOPSIS]                       = "synopsis",
    [FIELD_VIRTUAL_DEPS]                   = "virtual_deps",
    [FIELD_WRAPPED]                        = "wrapped"
};

/* EXPORT int field_name_to_id(const char *name) */
/* { */
/*     if (strncmp(name, "name", 4) == 0) return FIELD_NAME; */
/*     if (strncmp(name, "public_name", 4) == 0) return FIELD_PUBLIC_NAME; */
/*     if (strncmp(name, "libraries", 4) == 0) return FIELD_LIBRARIES; */
/*     if (strncmp(name, "modules", 4) == 0) return FIELD_MODULES; */
/*     if (strncmp(name, "preprocess", 4) == 0) return FIELD_PREPROCESS; */
/*     return -1; */
/* } */

