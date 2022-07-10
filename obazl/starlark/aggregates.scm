(format #t "loading starlark/aggregates.scm\n")

(define (aggregate-stanza? stanza)
  ;; (format #t "aggregate-stanza? ~A\n" (car stanza))
  ;; dune treats executables as aggregators, since they have a 'modules'
  ;; field
  (if (equal? (car stanza) :library)
      #t
      (if (equal? (car stanza) :executable)
          #t
          #f)))

;; if pubname and privname differ, use explicit 'ns' attribute
(define (use-ns-attr? modname pubname)
  (if (equal? modname pubname)
      #f
      (let ((s1 (if (symbol? modname) (symbol->string modname) modname))
            (s2 (if (symbol? pubname) (symbol->string pubname) pubname)))
        (not (and (string=? (substring s1 1) (substring s2 1))
                  (char=? (char-upcase (string-ref s1 0))
                          (char-upcase (string-ref s2 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (starlark-emit-aggregate-target outp stanza) ;; typ fs-path stanza)
  (format #t "~A: ~A\n" (blue "STARLARK-EMIT-AGGREGATE-TARGET") stanza)
  (let* ((kind (car stanza))
         (stanza-alist (cdr stanza))
         (privname (car (assoc-val :privname stanza-alist)))
         (modname (normalize-module-name privname))
         (pubname (if-let ((pubname (assoc-val :pubname stanza-alist)))
                          (car pubname)
                          privname))
         (_ (format #t "name: ~A, modname: ~A\n" pubname modname))
         (libname (string-upcase (stringify privname)))

         (opts (if-let ((opts (assoc :opts (cdr stanza))))
                          (cdr opts) '()))
         ;; skip :opens, '-open' not relevant for aggregates
         (flags (if-let ((flags (assoc-val :flags opts)))
                        (list (apply string-append
                                     (map stringify flags)))
                        '()))
         ;; skip :standard, handled by hidden attributes
         ;; (standard-opts? (if (assoc :standard opts) #t #f))

         (submodules (if-let ((submods (assoc :submodules stanza-alist)))
                             (cdr submods) '()))
         (deps (assoc :deps stanza-alist)))

    (begin
      (format #t "DEPS: ~A\n" deps)
      (format #t "SUBMs: ~A\n" submodules))

    ;; (format outp "######## ~A ########\n" pubname)

    (case kind
      ((:archive)
       (begin
         (format outp "#################\n")
         (format outp "ocaml_ns_archive(\n")
         (format outp "    name       = \"~A\",\n" pubname)
         (format outp "    submodules = [~{\"~A\"~^, ~}],\n" submodules)

         (format outp "    opts       = [~{\"~A\"~^, ~}],\n" flags)
         ;; (format outp "    opts       = ~A_OPTS,\n" libname)

         ;; (format outp "    submodules = [\n")
         ;;       (for-each (lambda (submod)
         ;;                   (format outp "        \":~A\",\n"
         ;;                           (symbol->string
         ;;                            (normalize-module-name submod))
         ;;                           ))
         ;;                 submods)
         ;;       (format outp "    ],\n")
         (format outp ")\n\n")))

      ((:library)
       (begin
         (format outp "##############\n")
         (format outp "ocaml_library(\n")
         (format outp "    name    = \"~A\",\n" pubname)
         (format outp "    submodules = [~{\"~A\"~^, ~}],\n" submodules)
         (format outp ")\n\n")))

      ;; ((:archive)
      ;;  (begin
      ;;    (format outp "##############\n")
      ;;    (format outp "ocaml_archive(\n")
      ;;    (format outp "    name    = \"~A\",\n" pubname)
      ;;    (format outp "    visibility = [\"//visibility:public\"],\n")

      ;;    ;; "null libs" contain no submodules, e.g. tezos:src/tooling
      ;;    (if submodules
      ;;        (let* ((subms (hash-table-keys (cadr submodules)))
      ;;               (submods (sort! subms modules<?)))
      ;;          (format outp "    modules = [\n")
      ;;          (for-each (lambda (submod) ;; (<modname> . kind)
      ;;                      (format outp "        \":~A\",\n"
      ;;                              (symbol->string
      ;;                               (normalize-module-name submod))
      ;;                              ))
      ;;                    submods)
      ;;          (format outp "    ],\n")))
      ;;    (format outp ")\n\n")))

      ;; ;; (format outp "#############################\n")))
      ;; ((:ns-archive)
      ;;  (begin
      ;;    ;; (format outp "#################\n")
      ;;    (format outp "ocaml_ns_archive(\n")
      ;;    (format outp "    name       = \"~A\",\n" pubname)

      ;;    (if (use-ns-attr? modname pubname)
      ;;        (format outp "    ns         = \"~A\",\n" modname))
      ;;    (format outp "    visibility = [\"//visibility:public\"],\n")

      ;;    ;; "null libs" contain no submodules, e.g. tezos:src/tooling
      ;;    (if submodules
      ;;        (let* ((subms (hash-table-keys (cadr submodules)))
      ;;               (submods (sort! subms modules<?)))
      ;;          (format outp "    submodules = [\n")
      ;;          (for-each (lambda (submod)
      ;;                      (format outp "        \":~A\",\n"
      ;;                              (symbol->string
      ;;                               (normalize-module-name submod))
      ;;                              ))
      ;;                    submods)
      ;;          (format outp "    ],\n")))
      ;;    (format outp ")\n\n")))
      ;; (format outp "#############################\n")))
      (else
       (format outp "UNCAUGHT kind: ~A\n\n" kind)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (starlark-emit-aggregate-targets outp pkg) ;; fs-path stanzas)
  (format #t "~A: ~A\n" (blue "STARLARK-EMIT-AGGREGATE-TARGETS") pkg)

  ;; only emit header if aggregators
  (let* ((stanzas (assoc-val :dune pkg))
         ;; FIXME: exclude null libs, e.g. tezos:src/tooling
         (aggs (filter (lambda (stanza)
                         (case (car stanza)
                           ((:archive) #t)
                           ((:library) #t)
                           (else #f)))
                       stanzas)))
    (format #t "AGGREGATES: ~A\n" aggs)
    (if aggs
        (begin
          (format outp "#############################\n")
          (format outp "####  Aggregate Targets  ####\n")
          (format outp "\n")

          (for-each (lambda (stanza)
                      (format #t "stanza: ~A\n" stanza)
                      (case (car stanza)
                        ((:archive) ;; implies dune library, wrapped
                           (starlark-emit-aggregate-target outp stanza))
                        ((:library) ;; implies dune library, unwrapped
                           (starlark-emit-aggregate-target outp stanza))
                        (else (format outp "UNCAUGHT stanza: ~A\n"
                                      stanza))))
                    aggs)))))

                          ;; outp
                          ;; (if (library-namespaced? stanza) :ns-archive :archive)
                          ;; fs-path
                          ;; stanza)

                       ;; (if (library-wrapped? stanza)
                       ;;     (starlark-emit-aggregate-target outp 'ns-archive
                       ;;                                     (cadr stanza))
                       ;;     ;; unwrapped dune libs do not get an aggregate rule,
                       ;;     ;; but they do have flags and deps that apply to their
                       ;;     ;; modules. so we need to emit those syms:
                       ;; CORRECTION: unwrapped libs are handled normally(?)
                       ;;     (starlark-emit-stanza-deps-and-flags outp 'ns-archive
                       ;;                                          (cadr stanza)))

(format #t "loaded starlark/aggregates.scm\n")
