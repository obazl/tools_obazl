(define (starlark-emit-ocamllex outp stanza)
  (format #t "~A: ~A~%" (ublue "starlark-emit-ocamllex") stanza)

  (for-each (lambda (ocamllex)
              (format #t "~A: ~A~%" (uwhite "ocamllex") ocamllex)
              (let* ((principal-name (principal-name (cdr ocamllex)))
                     (module-name (car ocamllex))
                     (target-name (string-copy
                                   (format #f "~A" module-name))))
                (string-set! target-name 0
                             (char-upcase (string-ref target-name 0)))
                (format #t "emitting ocamllex: ~A\n" ocamllex)

                ;; ocaml_module target emitted by aggregate emitter
                ;; (format outp "#############\n")
                ;; (format outp "ocaml_module(\n")
                ;; (format outp "    name     = \"~A\",\n" target-name)
                ;; (format outp "    struct   = \"~A\",\n"
                ;;         (string-append module-name ".ml"))
                ;; (format outp ")")
                ;; (newline outp)

                (format outp "##########\n")
                (format outp "ocamllex(\n")
                (format outp "    name  = \"lex_~S\",\n" module-name)
                (format outp "    src   = \"~A\",\n" (cdr ocamllex))
                        ;; (string-append module-name ".mll"))
                (format outp "    out   = \"~A.ml\",\n" principal-name)
                (format outp ")")
                (newline outp)))
            (cdr stanza)))

(define (starlark-emit-ocamlyacc outp stanza)
  (format #t "~A: ~A~%" (ublue "starlark-emit-ocamlyacc") stanza)

  (for-each (lambda (ocamlyacc)
              (let* ((principal-name (principal-name (cdr ocamlyacc)))
                     (module-name (car ocamlyacc))
                     (target-name (string-copy
                                   (format #f "~A" module-name))))
                (string-set! target-name 0
                             (char-upcase (string-ref target-name 0)))
                (format #t "emitting ocamlyacc: ~A\n" ocamlyacc)

                (format outp "##########\n")
                (format outp "ocamlyacc(\n")
                (format outp "    name  = \"yacc_~S\",\n" module-name)
                (format outp "    src   = \"~A\",\n" (cdr ocamlyacc))
                (format outp "    outs  = [\"~A.ml\", \"~A.mli\"]~%" principal-name principal-name)
                (format outp ")")
                (newline outp)))
            (cdr stanza)))

(define (starlark-emit-menhir outp stanza)
  (format #t "~A: ~A~%" (ublue "starlark-emit-menhir") stanza)
  (format outp "#######  emitting menhir ####")
  (newline outp)

  (for-each (lambda (ocamlyacc)
              (let* ((principal-name (principal-name (cdr ocamlyacc)))
                     (module-name (car ocamlyacc))
                     (target-name (string-copy
                                   (format #f "~A" module-name))))
                (string-set! target-name 0
                             (char-upcase (string-ref target-name 0)))
                (format #t "emitting ocamlyacc: ~A\n" ocamlyacc)

                (format outp "##########\n")
                (format outp "menhir(\n")
                (format outp "    name  = \"menhir_~S\",\n" module-name)
                (format outp "    src   = \"~A\",\n" (cdr ocamlyacc))
                (format outp "    outs  = [\"~A.ml\", \"~A.mli\"]~%" principal-name principal-name)
                (format outp ")")
                (newline outp)))
            (cdr stanza)))

(define (starlark-emit-file-generators outp pkg)
  (format #t "~A: ~A~%" (ublue "starlark-emit-file-generators") pkg)
  (let* ((stanzas (assoc-val :dune pkg))
         (hdr-flag #t))
    (for-each (lambda (stanza)
                (case (car stanza)
                  ((:ocamllex :ocamlyacc :menhir) ;; etc
                   (if hdr-flag
                       (begin
                         (format outp "########################\n")
                         (format outp "####  File Generators  ####\n")
                         (newline outp)
                         (set! hdr-flag #f)))))

                (case (car stanza)
                  ((:ocamllex)
                   (starlark-emit-ocamllex outp stanza))
                  ((:ocamlyacc)
                   (starlark-emit-ocamlyacc outp stanza))
                  ((:menhir)
                   (starlark-emit-menhir outp stanza))
                  ;; etc.
                  (else)
                   ))
              stanzas)))

