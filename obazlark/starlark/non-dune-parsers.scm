(define (emit-non-dune-ocamllex outp lexer)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A~%" (ublue "emit-non-dune-ocamllex") lexer))

  (let* ((principal-name (principal-name (cdr lexer)))
         (module-name (car lexer))
         (target-name (string-copy
                       (format #f "~A" module-name))))
    (string-set! target-name 0
                 (char-upcase (string-ref target-name 0)))
    (if (or *debug-emit* *debugging*)
        (format #t "emitting ocamllex: ~A\n" lexer))

    ;; ocaml_module target emitted by aggregate emitter
    ;; (format outp "#############\n")
    ;; (format outp "ocaml_module(\n")
    ;; (format outp "    name     = \"~A\",\n" target-name)
    ;; (format outp "    struct   = \"~A\",\n"
    ;;         (string-append module-name ".ml"))
    ;; (format outp ")")
    ;; (newline outp)

    ;; (format outp "##########\n")
    (format outp "ocamllex(\n")
    (format outp "    name  = \"lex_~S\",\n" module-name)
    (format outp "    src   = \"~A\",\n" (cdr lexer))
    ;; (string-append module-name ".mll"))
    (format outp "    out   = \"~A.ml\",\n" principal-name)
    (format outp ")~%")
    (newline outp)))

(define (emit-non-dune-ocamlyacc outp yacc)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A~%" (ublue "emit-non-dune-ocamlyacc") yacc))

  (let* ((principal-name (principal-name (cdr yacc)))
         (module-name (car yacc))
         (target-name (string-copy
                       (format #f "~A" module-name))))
    (string-set! target-name 0
                 (char-upcase (string-ref target-name 0)))
    (if (or *debug-emit* *debugging*)
        (format #t "emitting yacc target: ~A\n" yacc))

    (if *menhir*
        ;;FIXME: use emit-non-dune-menhir below
        (emit-non-dune-menhir outp yacc)
        ;; (begin

        ;;   (format outp "menhir(\n")
        ;;   (format outp "    name  = \"menhir_Parser\",\n")
        ;;   (format outp "    tool  = \"@ocaml//bin:menhir\",\n")
        ;;   (format outp "    grammars = [\"~A\"],\n" (cdr yacc))
        ;;   (format outp "    deps  = [],\n")
        ;;   (format outp "    outs  = [\"~A.ml\", \"~A.mli\"]~%" principal-name principal-name)
        ;;   (format outp "    ]\n")
        ;;   (newline outp))
        (begin
          ;; (format outp "##########\n")
          (format outp "ocamlyacc(\n")
          (format outp "    name  = \"yacc_~S\",\n" module-name)
          (format outp "    src   = \"~A\",\n" (cdr yacc))
          (format outp "    outs  = [\"~A.ml\", \"~A.mli\"]~%" principal-name principal-name)
          (format outp ")~%")
          (newline outp)))))

(define (emit-non-dune-menhir outp yacc)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A~%" (ublue "emit-non-dune-menhir") yacc))
  (let ((principal-name (principal-name (cdr yacc)))
        (modname (car yacc)))
    (if (or *debug-emit* *debugging*)
        (format #t "emitting menhir target: ~A\n" yacc))
    ;; (format outp "#######\n")
    (format outp "menhir(\n")
    (format outp "    name     = \"menhir_~S\",\n" modname)
    (format outp "    outs     = [\"~A.ml\", \"~A.mli\"],~%" principal-name principal-name)
    (format outp "    opts     = [],~%")
    (format outp "    flags    = [],~%")
    (format outp "    grammars = [\"~A\"],\n" (cdr yacc))
    (format outp "    deps     = []~%")
    (format outp "    tool     = \"@ocaml//bin:menhir\",~%")
    (format outp "    ## tokens_unused = [\"FIXME\"],~%")
    (format outp "    ## token = \":FIXME\"~%")
    (format outp ")~%")
    (newline outp)))

(define (emit-non-dune-file-generators outp pkg)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A~%" (ublue "emit-non-dune-file-generators") pkg))
  (let* ((lexers (if-let ((lexers (assoc-in '(:lex :static) pkg)))
                         (cdr lexers) '()))
         (yaccs (if-let ((yaccs (assoc-in '(:yacc :static) pkg)))
                        (cdr yaccs) '()))
         (hdr-flag #t))

    (if (or (truthy? lexers) (truthy? yaccs))
        (begin
          (format outp "################################################################\n")

          (if (truthy? lexers)
              (begin
                (for-each (lambda (lexer)
                            (format #t "LEXER: ~A~%" lexer)
                            (emit-non-dune-ocamllex outp lexer))
                          lexers)))

          (if (truthy? yaccs)
              (begin
                (for-each (lambda (yacc)
                            (format #t "YACC: ~A~%" yacc)
                            (emit-non-dune-ocamlyacc outp yacc))
                          yaccs)))))))
