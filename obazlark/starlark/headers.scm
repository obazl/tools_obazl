(format #t "loading starlark/headers.scm\n")

(define (pkg->obazl-rules pkg)
  (format #t "~A~%" (ublue "-pkg->obazl-rules"))
  (let* ((stanzas (assoc-val :dune pkg))
         ;; (_ (format #t "stanzas: ~A\n" stanzas))
         (-rules (fold (lambda (stanza accum)
                         (format #t "  ~A: ~A\n" (blue "stanza") stanza)
                         (let* ((stanza-alist (cdr stanza))
                                (_ (format #t "~A: ~A~%" (yellow "stanza-alist") stanza-alist))
                                (dune-rule (car stanza))
                                (_ (format #t "~A: ~A~%" (yellow "dune-rule") dune-rule))
                                (rule (case dune-rule
                                        ((:rule)
                                         (let* ((actions (assoc :actions stanza-alist))
                                                (cmd-list (assoc-in* '(:actions :cmd) stanza-alist))
                                                (_ (format #t "~A: ~A~%" (green "cmd-list") cmd-list))
                                                (cmd-ct (length cmd-list)))
                                           (if (> cmd-ct 1)
                                               (fold (lambda (cmd accum)
                                                       (format #t "~A: ~A~%" (red "cmd") cmd)
                                                       (let ((tool (car (assoc-val :tool (cdr cmd)))))
                                                         (case tool
                                                           ((:write-file)
                                                            (if (member :write-file accum)
                                                                accum (cons :write-file accum)))
                                                           (else accum))))
                                                     '() cmd-list)
                                               (list dune-rule))))
                                        ((:ocamllex) (cons :ocamllex accum))
                                        ((:ocamlyacc) (cons :ocamlyacc accum))
                                        ((:write-file) (cons :write-file accum))
                                        (else
                                         (list dune-rule))))
                                (accum (if rule (append rule accum) accum))
                                ;; (accum (if (assoc :namespaced s-alist)
                                ;;            (cons :namespaced accum)
                                ;;            accum))
                                (accum (if (alist? stanza-alist)
                                           (if (assoc :ppx stanza-alist)
                                               (cons :ppx accum)
                                               accum)
                                           accum))
                                )
                           accum))
                       '() stanzas)))
    (let* ((rules (if (assoc :modules pkg) (cons :module -rules) -rules))
           (rules (if (assoc :structures pkg) (cons :module rules) rules))
           (rules (if (assoc :signatures pkg) (cons :sig rules) rules))
           (_ (format #t "~A: ~A~%" (red "obazlrules") rules))
           ;; dedup
           (rules (fold (lambda (x accum)
                          (if (member x accum) accum
                              (cons x accum)))
                        '() rules))
           )
      (format #t "~A: ~A~%" (red "obazlrules") rules)
      rules)))

(define (starlark-emit-buildfile-hdr outp obazl-rules)
  (format #t "~A: ~A\n" (blue "starlark-emit-buildfile-hdr") obazl-rules)

  (if (member :write-file obazl-rules)
      (begin
        (format outp "load(\"@bazel_skylib//rules:write_file.bzl\", \"write_file\")\n")
        (format outp "\n")))

  (if (find-if (lambda (rule)
                 (member rule '(:archive
                                :library
                                :module
                                :ns-archive
                                :ns-library
                                :ocamllex
                                :ocamlyacc
                                :sig
                                :executable
                                :test)))
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

        (if (member :ocamllex obazl-rules)
            (format outp "     \"ocaml_lex\",\n"))

        (if (member :module obazl-rules)
            (format outp "     \"ocaml_module\",\n"))

        (if (member :sig obazl-rules)
            ;; (if *build-dyads*
            (format outp "     \"ocaml_signature\",\n")) ;;)

        ;; (if (pkg-has-archive? obazl-rules)
        ;;     (if (pkg-namespaced? obazl-rules)
        ;;         (format outp "     \"ocaml_ns_archive\",\n")))

        ;; ;; (if (assoc-in '(:stanzas :signature) (cdr obazl-rules))
        ;; (if (pkg-has-signature? obazl-rules)
        ;;     (format outp "     \"ocaml_signature\",\n"))

        (if (member :test obazl-rules)
            (format outp "     \"ocaml_test\",\n"))

        (if (member :ocamlyacc obazl-rules)
            (format outp "     \"ocaml_yacc\",\n"))

        (if (member :ppx obazl-rules)
            (format outp "     \"ppx_executable\",\n"))


        (format outp ")\n")

        (newline outp)

        ))

  (format outp "package(default_visibility = [\"//visibility:public\"])\n")
  (newline outp)
  )
