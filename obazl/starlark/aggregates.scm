(format #t "loading starlark/aggregates.scm\n")

;;FIXME: get this from cmdline or ini file
(define *ns-topdown* #t)

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

(define (-emit-topdown-aggregate outp kind pubname submodules flags)
  (begin
    (format #t "EMITTING TOPDOWN NS AGGREGATE: ~A\n" kind)
    (format outp "#################\n")
    (if (eq? kind :ns-archive)
        (format outp "ocaml_ns_archive(\n")
        (format outp "ocaml_ns_library(\n"))
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

(define (-emit-bottomup-aggregate outp kind ns pubname submodules flags)
  (format #t "EMITTING BOTTOMUP NS AGGREGATE: ~A\n" kind)
  (format outp "#################\n")
  (if (eq? kind :ns-archive)
      (format outp "ocaml_archive(\n")
      (format outp "ocaml_library(\n"))
  (format outp "    name       = \"~A\",\n" pubname)
  (format outp "    manifest   = [~{\"~A\"~^, ~}],\n" submodules)
  (format outp "    opts       = [~{\"~A\"~^, ~}],\n" flags)
  ;; (format outp "    opts       = ~A_OPTS,\n" libname)
  (format outp ")\n\n")

  (format outp "#################\n")
  (format outp "ocaml_ns_resolver(\n")
  (format outp "    name       = \"ns.~A\",\n" pubname)
  (format outp "    ns         = \"~A\",\n" ns)
  (format outp "    submodules = [~{\"~A\"~^, ~}],\n" submodules)
  (format outp ")\n\n")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (starlark-emit-aggregate-target outp stanza) ;; typ fs-path stanza)
  (format #t "~A: ~A\n" (blue "STARLARK-EMIT-AGGREGATE-TARGET") stanza)
  (let* ((kind (car stanza))
         (stanza-alist (cdr stanza))
         (privname (assoc-val :privname stanza-alist))
         (modname (normalize-module-name privname))
         (pubname (if-let ((pubname (assoc-val :pubname stanza-alist)))
                          pubname
                          privname))
         (_ (format #t "name: ~A, modname: ~A\n" pubname modname))
         (ns (assoc-val :ns (cdr stanza)))
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

         (submodules (if-let ((submods (assoc-in '(:manifest :modules)
                                                 stanza-alist)))
                             (cdr submods) '()))
         (deps (assoc :deps stanza-alist)))

    (begin
      (format #t "kind: ~A\n" kind)
      (format #t "DEPS: ~A\n" deps)
      (format #t "SUBMs: ~A\n" submodules))

    ;; (format outp "######## ~A ########\n" pubname)

    (case kind
      ((:ns-archive :ns-library)
       (if *ns-topdown*
           (-emit-topdown-aggregate outp kind pubname submodules flags)
           ;; aggregated bottomup, needs archive or lib w/o ns
           (-emit-bottomup-aggregate outp kind ns pubname submodules flags)))

      ((:archive :library)
       (begin
         (format #t "EMITTING NON-NS AGGREGATE: ~A\n" kind)
         (format outp "##############\n")
         (if (eq? kind :library)
             (format outp "ocaml_library(\n")
             (format outp "ocaml_archive(\n"))
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
                         (member
                          (car stanza)
                          '(:archive :library :ns-archive :ns-library)))
                       stanzas)))
    (format #t "AGGREGATES: ~A\n" aggs)
    (if (not (null? aggs))
        (begin
          (format outp "#############################\n")
          (format outp "####  Aggregate Targets  ####\n")
          (format outp "\n")))

    (for-each (lambda (stanza)
                (format #t "stanza: ~A\n" stanza)
                (case (car stanza)
                  ((:ns-archive :ns-library) ;; dune library, wrapped
                   (starlark-emit-aggregate-target outp stanza))
                  ((:archive :library) ;; dune library, unwrapped
                   (starlark-emit-aggregate-target outp stanza))
                  (else (format outp "UNCAUGHT stanza: ~A\n"
                                stanza))))
              aggs)))

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
