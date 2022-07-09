(define (starlark-emit-file-targets outp fs-path stanzas dune-pkg)
  ;; (format #t "starlark-emit-file-targets: ~A\n" fs-path)

  ;; we emit targets for both static and generated source files; in
  ;; addition, we may have :indirect submodule deps (example:
  ;; src/lib_protocol_environment/sigs)

  (let* ((srcfiles (if-let ((srcs (assoc-in '(:srcfiles :ocaml :generated)
                                           dune-pkg)))
                       (cadr srcs)
                      '()))
         (srcfiles (if-let ((srcs (assoc-in '(:srcfiles :ocaml :static)
                                            dune-pkg)))
                           (sort! (concatenate (cadr srcs) srcfiles) string<?)
                           srcfiles))
        )
    (if srcfiles
        (begin
          (format outp "#############################\n")
          (format outp "####  Singleton Targets  ####\n")
          (newline outp)))

    (for-each (lambda (srcfile)
                ;; (format #t "SRCFILE: ~A/~A\n" fs-path srcfile)
                ;; (format #t ".") (flush-output-port)
                (let-values (((typ mname)
                              (if (string-suffix? ".ml" srcfile)
                                  (values :ml
                                          ;;(string-drop-right srcfile 3)
                                          (file-name->module-name srcfile))
                                  (if (string-suffix? ".mli" srcfile)
                                      (values :mli
                                              ;; (string-drop-right srcfile 4)
                                              (file-name->module-name srcfile)
                                              )
                                      (error 'bad-filename
                                             (string-append
                                              "extension should be .ml or .mli: "
                                              srcfile))))))
                  (let* ((aggregator-stanza
                          (module-name->aggregator mname stanzas))
                         ;; (_ (format #t "AGG for ~A:~A: ~A\n"
                         ;;            fs-path mname aggregator-stanza))

                         (ppx-alist ;; = ppx alist
                          (module->ppx-alist fs-path mname stanzas))
                         (_ (format #t "PPX-ALIST: ~A\n" ppx-alist))

                         (opts
                          (if aggregator-stanza
                              (if-let ((opts
                                        (assoc ':opts aggregator-stanza)))
                                      (cadr opts)
                                      #f)))

                         (namespace
                          (if aggregator-stanza
                              (if-let ((pub (assoc-in '(:name :public)
                                                            aggregator-stanza)))
                                      (cadr pub) #f)))

                         (ns-module
                          (if aggregator-stanza
                              (normalize-module-name
                               (cadr (assoc-in '(:name :private)
                                              aggregator-stanza)))
                              #f))
                         )
                    (format #t "namespace: ~A\n"
                            namespace)
                    (if (string-suffix? ".ml" srcfile)
                        (begin
                          (format outp "ocaml_module(\n")
                          (format outp "    name     = \"~A\",\n" mname)
                          (format outp "    struct   = \"~A\",\n" srcfile)
                          (let ((mlisrc (string-append
                                         ;; fs-path "/"
                                         srcfile "i")))
                            ;; (format #t "mlisrc: ~A\n" mlisrc)
                            ;; (format #t "srcfiles: ~A\n" srcfiles)
                            (if (member mlisrc srcfiles)
                                (format outp "    sig      = \":~A\",\n"
                                        (string-append
                                         (symbol->string mname) "_cmi"))))

                          (if ppx-alist
                              (begin
                                (format outp
                                        "    ppx      = \":~A\",\n"
                                        (cadr (assoc :name ppx-alist)))
                                (if (not
                                     (equal? :all (cadr (assoc :scope
                                                               ppx-alist))))
                                    (format outp
                                            "    ppx_args = [~{~S, ~}],\n"
                                            (cadr (assoc :args ppx-alist))))))
                          )

                        ;;else
                        (if (string-suffix? ".mli" srcfile)
                              (begin
                                (format outp "ocaml_signature(\n")
                                (format outp "    name     = \"~A_cmi\",\n" mname)
                                (format outp "    src      = \"~A\",\n" srcfile)

                          (format #t "YYYYYYYYYYYYYYYYy\n")
                                (if ppx-alist
                                  (begin
                                    (format outp
                                            "    ppx      = \":~A\",\n"
                                            (cadr (assoc :name ppx-alist)))
                                    (if (not
                                         (equal? :all (cadr (assoc :scope
                                                                   ppx-alist))))
                                        (format outp
                                            "    ppx_args = [~{~S, ~}],\n"
                                            (cadr (assoc :args ppx-alist))))))
                              )
                            (error 'bad-filename
                                   (string-append
                                    "extension should be .ml or .mli: "
                                    srcfile))))

                    ;; now opts and deps
                    ;; (starlark-emit-build-opts outp mname stanzas dune-pkg)
                    (if namespace
                        (if opts
                            (format outp "    opts     = ~A,\n"
                                    (name->opts-sym namespace))))
                        ;; (if-let ((exe-deps
                        ;;           (module->executable-deps stanzas mname)))
                        ;;         (begin
                        ;;           (format outp "    opts   = [\n")
                        ;;           (for-each (lambda (dep)
                        ;;                       (format outp
                        ;;                               "        \"~A\",\n" dep))
                        ;;                     exe-deps)
                        ;;           (format outp "    ],\n"))

                        ;;         ;; else: neither submodule nor exec dep
                        ;;         ;; => no opts?
                        ;;         ))

                    (begin
                      ;; (format #t "    processing deps\n")
                      (let* ((deps (if-let ((deps (filedeps-tbl
                                                   (string-append
                                                    fs-path "/" srcfile))))
                                           (cadr (assoc :deps deps))
                                           #f))
                             (dep-labels (if deps
                                             (deps->labels deps
                                                           namespace
                                                           ns-module
                                                           mname stanzas
                                                           srcfiles)
                                             '()))
                             )
                        (if (equal? mname 'Rand)
                            (begin
                              (format #t "RAND:\n")
                              (format #t "  namespace: ~A\n" namespace)
                              (format #t "  deps: ~A\n" deps)
                              (format #t "  dep-labels: ~A\n" dep-labels)
                              ))
                        (if namespace
                            ;; (if deps ;; (not (null? dep-labels))
                                (format outp "    deps     = ~A + [\n"
                                        (name->deps-sym namespace))
                                (format outp "    deps     = [\n"))
                        ;; )


                        (for-each (lambda (dep-label)
                                    (if (cadr dep-label)
                                        (format outp "       \"~A\",\n"
                                                ;; ## ~A\n"
                                                (cadr dep-label)
                                                ;; (car dep-label)
                                                )
                                        ;; (format outp "       ## ~A\n"
                                        ;;         (car dep-label))
                                        ))
                                  dep-labels))
                      (format outp "    ]\n"))
                    (format outp ")\n\n"))))
              srcfiles)
    ))

    ;; (let ((lib-stanzas (assoc+ :library stanzas)))
    ;;   (if (not (null? lib-stanzas))
    ;;       (for-each (lambda (stanza)
    ;;                   (newline outp)
    ;;                   (format outp "lib stanza: ~A\n"
    ;;                           (assoc 'name (cdr stanza)))
    ;;                   (let ((modules (assoc 'modules (cdr stanza)))
    ;;                         (flags (if-let
    ;;                                 ((flags (assoc 'flags (cdr stanza))))
    ;;                                 (cadr flags)
    ;;                                 '()))
    ;;                         (ocamlopt-flags
    ;;                          (if-let
    ;;                           ((flags (assoc 'ocamlopt_flags (cdr stanza))))
    ;;                           (cadr ocamlopt-flags)
    ;;                           '())))
    ;;                     (format outp "modules: ~A\n" modules)
    ;;                     (format outp "flags: ~A\n" flags)
    ;;                     (format outp "ocamlopt_flags: ~A\n" ocamlopt-flags)
    ;;                     ))
    ;;                 ;;(emit-lib-args fs-path stanza srcfiles out-port))
    ;;                 lib-stanzas))
    ;;   )
    ;; ))
