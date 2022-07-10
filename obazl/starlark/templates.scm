(format #t "loading starlark/templates.scm\n")

(define (starlark-emit-buildfile-hdr outp obazl-rules)
  (format #t "starlark-emit-buildfile-hdr: ~A\n" obazl-rules)

  ;; if write_file, copy_file, etc, emit:
  ;; load("@bazel_skylib//lib:paths.bzl", "write_file") ;; etc.

  (format outp "load(\"@rules_ocaml//build:rules.bzl\",\n")

  ;; 'library' with wrapped false:
  (if (member :archive obazl-rules)
          (format outp "     \"ocaml_archive\",\n"))

  ;; 'library' with wrapped true:
  (if (member :ns-archive obazl-rules)
          (format outp "     \"ocaml_ns_archive\",\n"))


  ;; obazl-style libraries not supported by dune; 'library' stanza
  ;; always means archive.
  ;; (if (member :library obazl-rules)
  ;;         (format outp "     \"ocaml_library\",\n"))
  ;; (if (member :ns-library obazl-rules)
  ;;         (format outp "     \"ocaml_ns_library\",\n")



  ;; (if (or (assoc-in '(:stanzas :executable) (cdr obazl-rules))
  ;;         (assoc-in '(:stanzas :executables) (cdr obazl-rules)))
  ;;     (format outp "     \"ocaml_executable\",\n"))

  ;; (if (assoc-in '(:stanzas :ocamllex) (cdr obazl-rules))
  ;;     (format outp "     \"ocaml_lex\",\n"))

  (if (member :module obazl-rules)
      (format outp "     \"ocaml_module\",\n"))

  (if (member :sig obazl-rules)
      (format outp "     \"ocaml_signature\",\n"))

  ;; (if (pkg-has-archive? obazl-rules)
  ;;     (if (pkg-namespaced? obazl-rules)
  ;;         (format outp "     \"ocaml_ns_archive\",\n")))

  ;; ;; (if (assoc-in '(:stanzas :signature) (cdr obazl-rules))
  ;; (if (pkg-has-signature? obazl-rules)
  ;;     (format outp "     \"ocaml_signature\",\n"))

  (format outp ")\n")

  (newline outp)
  ;; (format outp "#############################\n")

  '())

(format #t "loaded starlark/templates.scm\n")
