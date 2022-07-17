(format #t "loading starlark/templates.scm\n")

(define (starlark-emit-buildfile-hdr outp obazl-rules)
  (format #t "starlark-emit-buildfile-hdr: ~A\n" obazl-rules)

  (if (member :skylib-write-file obazl-rules)
      (begin
        (format outp "load(\"@bazel_skylib//rules:write_file.bzl\", \"write_file\")\n")
        (format outp "\n")))

  (if (find-if (lambda (rule)
                 (member rule '(:archive
                                :library
                                :ns-archive
                                :ns-library
                                :executable)))
               obazl-rules)
      (begin
        (format #t "writing buildfile header\n")
        ;; if write_file, copy_file, etc, emit:
        ;; load("@bazel_skylib//lib:paths.bzl", "write_file") ;; etc.

        (format outp "load(\"@rules_ocaml//build:rules.bzl\",\n")

        (if (member :executable obazl-rules)
            (format outp "     \"ocaml_binary\",\n"))

        ;; 'library' with wrapped false:
        (if (member :archive obazl-rules)
            (format outp "     \"ocaml_archive\",\n"))
        (if (member :library obazl-rules)
            (format outp "     \"ocaml_library\",\n"))

        ;; 'library' with wrapped true:
        (if (member :ns-archive obazl-rules)
            (format outp "     \"ocaml_ns_archive\",\n"))
        (if (member :ns-library obazl-rules)
            (format outp "     \"ocaml_ns_library\",\n"))

        (if (not *ns-topdown*)
            (format outp "     \"ocaml_ns_resolver\",\n"))


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

        (format outp "package(default_visibility = [\"//visibility:public\"])\n")

        (newline outp)

        ;; (format outp "#############################\n")
        )))

(format #t "loaded starlark/templates.scm\n")
