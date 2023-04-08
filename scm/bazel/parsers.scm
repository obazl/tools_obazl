(define (bazel-emit-ocamllex outp stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "bazel-emit-ocamllex") stanza))

  (for-each (lambda (ocamllex)
              (if *mibl-debug-s7*
                  (format #t "~A: ~A~%" (uwhite "ocamllex") ocamllex))
              (let* ((principal-name (principal-name (cdr ocamllex)))
                     (module-name (car ocamllex))
                     (target-name (string-copy
                                   (format #f "~A" module-name))))
                (string-set! target-name 0
                             (char-upcase (string-ref target-name 0)))
                (if *mibl-debug-s7*
                    (format #t "emitting ocamllex: ~A\n" ocamllex))

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

(define (bazel-emit-ocamlyacc outp stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "bazel-emit-ocamlyacc") stanza))

  (for-each (lambda (ocamlyacc)
              (let* ((principal-name (principal-name (cdr ocamlyacc)))
                     (module-name (car ocamlyacc))
                     (target-name (string-copy
                                   (format #f "~A" module-name))))
                (string-set! target-name 0
                             (char-upcase (string-ref target-name 0)))
                (if *mibl-debug-s7*
                    (format #t "emitting ocamlyacc: ~A\n" ocamlyacc))

                (format outp "##########\n")
                (format outp "ocamlyacc(\n")
                (format outp "    name  = \"yacc_~S\",\n" module-name)
                (format outp "    src   = \"~A\",\n" (cdr ocamlyacc))
                (format outp "    outs  = [\"~A.ml\", \"~A.mli\"]~%" principal-name principal-name)
                (format outp ")")
                (newline outp)))
            (cdr stanza)))

(define (bazel-emit-menhir outp stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "bazel-emit-menhir") stanza))
  (newline outp)

  (for-each (lambda (parser)
              (let ((principal-name (principal-name parser)))
                (if *mibl-debug-s7*
                    (format #t "emitting menhir parser: ~A\n" parser))
                (format outp "#######\n")
                (format outp "menhir(\n")
                (format outp "    name     = \"menhir_~S\",\n" parser)
                (format outp "    grammars = [\"~A.mly\"],\n" parser)
                (if-let ((deps (assoc-val :deps (cdr stanza))))
                        (begin
                          (format outp "    deps = [~%")
                          (format outp "~{        \"~A\"~^,~%~}~%" deps)
                          (format outp "    ],~%")))
                (format outp "    outs     = [\"~A.ml\", \"~A.mli\"],~%" principal-name principal-name)
                (format outp "    tool     = \"@ocaml//bin:menhir\",~%")
                (if-let ((unused (assoc-val :unused-tokens (cdr stanza))))
                        (begin
                          (format outp "    tokens_unused = [~%")
                          (format outp "~{        \"~A\"~^,~%~}~%" unused)
                          (format outp "    ],~%")))
                (if-let ((token (assoc-val :external-tokens (cdr stanza))))
                        (begin
                          (format outp "    token = \":~A\",~%" (car token))))
                (if-let ((options (assoc-val :options (cdr stanza))))
                        (begin
                          (format outp "    opts = [~%")
                          (format outp "~{        \"~A\", \"~A\"~^,~%~}~%"
                                  (map (lambda (x)
                                         (values (car x) (cdr x)))
                                       options))
                          (format outp "    ],~%")))
                (if-let ((flags (assoc-val :flags (cdr stanza))))
                        (begin
                          (format outp "    flags = [~%")
                          (format outp "~{        \"~A\"~^,~%~}~%" flags)
                          (format outp "    ]~%")))
                (format outp ")")
                (newline outp)))
            (assoc-val :grammars (cdr stanza))))

(define (bazel-emit-file-generators outp pkg)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "bazel-emit-file-generators") pkg))
  (let* ((stanzas (assoc-val :mibl pkg))
         (hdr-flag #t))
    (for-each (lambda (stanza)
                (case (car stanza)
                  ((:lex :yacc :menhir) ;; etc
                   (if hdr-flag
                       (begin
                         (format outp "########################\n")
                         (format outp "####  File Generators  ####\n")
                         (newline outp)
                         (set! hdr-flag #f)))))

                (case (car stanza)
                  ((:lex)
                   (bazel-emit-ocamllex outp stanza))
                  ((:yacc)
                   (bazel-emit-ocamlyacc outp stanza))
                  ((:menhir)
                   (bazel-emit-menhir outp stanza))
                  ;; etc.
                  (else)
                   ))
              stanzas)))

