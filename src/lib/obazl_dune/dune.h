// from https://github.com/ocaml/dune/blob/main/editor-integration/emacs/dune.el

// not complete. missing: (select ...from...), re_export, pps, staged_pps, future_syntax

/*
(defconst dune-fields-regex
  (eval-when-compile
    (regexp-opt
     '("name" "public_name" "synopsis" "modules" "libraries" "wrapped"
       "preprocess" "preprocessor_deps" "optional" "c_names" "cxx_names"
       "foreign_stubs" "foreign_archives" "install_c_headers" "modes"
       "no_dynlink" "kind" "ppx_runtime_libraries" "virtual_deps" "js_of_ocaml"
       "flags" "ocamlc_flags" "ocamlopt_flags" "library_flags" "c_flags"
       "cxx_flags" "c_library_flags" "self_build_stubs_archive" "inline_tests"
       "modules_without_implementation" "private_modules"
       ;; + special_builtin_support
       "special_builtin_support" "build_info" "data_module" "api_version"
       ;; +stdlib
       "stdlib" "modules_before_stdlib" "exit_module" "internal_modules"
       ;; + virtual libraries
       "virtual_modules" "implements" "variant" "default_implementation"
       "allow_overlapping_dependencies"
       ;; + for "executable" and "executables":
       "package" "link_flags" "link_deps" "names" "public_names" "variants"
       "forbidden_libraries"
       ;; + for "foreign_library" and "foreign_stubs":
       "archive_name" "language" "names" "flags" "include_dirs" "extra_deps"
       ;; + for "rule":
       "targets" "action" "deps" "mode" "fallback" "locks"
       ;; + for "menhir":
       "merge_into"
       ;; + for "cinaps":
       "files"
       ;; + for "alias"
       "enabled_if"
       ;; + for env
       "binaries"
       ;; + for "install"
       "section" "files")
     'symbols))
  "Field names allowed in dune files.")
*/
