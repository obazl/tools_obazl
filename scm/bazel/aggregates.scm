(if *mibl-debug-s7*
    (format #t "loading bazel/aggregates.scm\n"))

;;FIXME: get this from cmdline or ini file
(define *mibl-ns-topdown* #t)

(define (aggregate-stanza? stanza)
  ;; (format #t "aggregate-stanza? ~A\n" (car stanza))
  ;; dune treats executables as aggregators, since they have a 'modules'
  ;; field
  (if (equal? (car stanza) :library)
      #t
      (if (equal? (car stanza) :executable)
          #t
          #f)))

;; if findlib-name and privname differ, use explicit 'ns' attribute
(define (use-ns-attr? modname findlib-name)
  (if (equal? modname findlib-name)
      #f
      (let ((s1 (if (symbol? modname) (symbol->string modname) modname))
            (s2 (if (symbol? findlib-name) (symbol->string findlib-name) findlib-name)))
        (not (and (string=? (substring s1 1) (substring s2 1))
                  (char=? (char-upcase (string-ref s1 0))
                          (char-upcase (string-ref s2 0))))))))

(define (-emit-topdown-aggregate outp pkg-path kind ns privname submodules link-opts cc-deps)
  (let* ((singleton (and (= (length submodules) 1)
                         (equal? (normalize-module-name ns)
                                 (submodules 0))))
         (is-test (and (string-contains pkg-path "test")
                       (string-contains (format #f "~A" privname) "test")))
         (tgt-name (if is-test (format #f "~A_test" privname) privname)))

    (if *mibl-debug-s7*
        (begin
          (format #t "EMITTING TOPDOWN NS AGGREGATE: ~A\n" kind)
          (format #t " link-opts: ~A\n" link-opts)))
    (if is-test
        (format outp "fail(\"FIXME: verify build_test target\")\n"))

    (if (eq? kind :ns-archive)
        (if singleton
            (begin
              ;; (format outp "##############~%")
              ;; (format outp "ocaml_ns_archive(  #0\n")
              (format outp "ocaml_library(\n"))
            (if is-test
              (format outp "build_test(\n")
              (format outp "ocaml_ns_archive(\n")))
        (if (and (= (length submodules) 1)
                 (equal? ns (submodules 0)))
            ;; (format outp "ocaml_library(  ##\n")
            (format outp "ocaml_ns_library(\n")
            (if *mibl-wrapped-libs-to-ns-archives*
                (format outp "ocaml_ns_archive(\n")
                (format outp "ocaml_ns_library(\n"))))
    (format outp "    name       = \"~A\",\n" tgt-name)
    (format outp "    visibility = [\"//visibility:public\"],\n")
    ;; (if (or (> (length submodules) 1)
    ;;         (not (equal? (normalize-module-name ns) (submodules 0))))
    (if ns
        (if (and (not singleton) (not is-test))
            (format outp "    ns_name    = \"~A\",\n" ns)))
    (if is-test
        (format outp "    targets  = [\n")
        (format outp "    manifest = [\n"))
    (format outp "~{        \":~A\"~^,\n~}\n" submodules)
    (format outp "    ],")
    (newline outp)

    (if cc-deps
        (begin
          (if *mibl-debug-s7*
              (format #t "~A: ~A~%" (ugreen "cc-deps") cc-deps))
          (format outp "    cc_deps    = [\"__lib~A__\"],"
                  (car (cdadr cc-deps)))
          (newline outp)))

    (if (truthy? link-opts)
        (if (number? link-opts)
            (format outp "    opts       = ~A_~A,\n" privname link-opts)
            (format outp "    opts       = [~{\"~A\"~^, ~}],\n" link-opts)))

    ;; (format outp "    manifest = [\n")
    ;;       (for-each (lambda (submod)
    ;;                   (format outp "        \":~A\",\n"
    ;;                           (symbol->string
    ;;                            (normalize-module-name submod))
    ;;                           ))
    ;;                 submods)
    ;;       (format outp "    ],\n")
    (format outp ")")
    (newline outp)
    (newline outp)))

(define (-emit-bottomup-aggregate outp kind ns findlib-name submodules flags cc-deps)
  (if *mibl-debug-s7*
      (format #t "EMITTING BOTTOMUP NS AGGREGATE: ~A\n" kind))
  (format outp "#################\n")
  (if (eq? kind :ns-archive)
      (format outp "ocaml_archive( #4\n")
      (format outp "ocaml_library( #5\n"))
  (format outp "    name       = \"~A\",\n" findlib-name)
  (format outp "    manifest   = [~%")
  (format outp "~{        \":~A\"~^,~%~}~%" submodules)
  (format outp "    ],~%")
  (format outp "    opts       = [~{\"~A\"~^, ~}],\n" flags)
  ;; (format outp "    opts       = ~A_OPTS,\n" libname)
  (if cc-deps
      (begin
        (if *mibl-debug-s7*
            (format #t "~A: ~A~%" (ugreen "cc-deps") cc-deps))
        (format outp "    cc_deps    = [\"__lib~A__\"]," (cdadr cc-deps))
        (newline outp)))

  (format outp ")\n\n")

  (format outp "#################\n")
  (format outp "ocaml_ns_resolver(\n")
  (format outp "    name       = \"ns.~A\",\n" findlib-name)
  (format outp "    ns_name    = \"~A\",\n" ns)
  (format outp "    manifest = [~{\"~A\"~^, ~}],\n" submodules)
  (format outp ")\n\n")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bazel-emit-aggregate-target outp pkg-path stanza) ;; typ fs-path stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (ublue "bazel-emit-aggregate-target") stanza))
  (let* ((kind (car stanza))
         (stanza-alist (cdr stanza))
         (ns (assoc-val :ns stanza-alist))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (blue "ns") ns)))
         (privname (assoc-val :privname stanza-alist))
         (findlib-name (if ns ns
                      (if-let ((findlib-name (assoc-val :findlib-name stanza-alist)))
                              findlib-name
                              privname)))
         (tgtname (if privname privname findlib-name))
         (modname (normalize-module-name (if privname privname findlib-name)))
         (_ (if *mibl-debug-s7*
                (format #t "~A: ~A, modname: ~A\n" (uwhite "name") findlib-name modname)))
         (libname (string-upcase (stringify modname)))

         (link-opts (if-let ((opts (assoc-val :link-opts (cdr stanza))))
                       opts '()))
         (_ (if *mibl-debug-s7*
                (format #t "~A: ~A\n" (uwhite "link-opts") link-opts)))
         ;; skip :opens, '-open' not relevant for aggregates
         ;; (flags (if-let ((flags (assoc-val :flags opts)))
         ;;                (list (apply string-append
         ;;                             (map stringify flags)))
         ;;                '()))
         ;; (_ (if *mibl-debug-s7*(format #t "~A: ~A\n" (uwhite "flags") flags)))
         ;; skip :standard, handled by hidden attributes
         ;; (standard-opts? (if (assoc :standard opts) #t #f))

         (submodules (if-let ((submods (assoc-in '(:manifest :modules)
                                                 stanza-alist)))
                             (cdr submods) '()))
         (submodules (sort! submodules sym<?))
         (deps (assoc :deps stanza-alist))
         (cc-deps (assoc :cc-stubs stanza-alist)))

    (if *mibl-debug-s7*
        (begin
          (format #t "kind: ~A\n" kind)
          (format #t "DEPS: ~A\n" deps)
          (format #t "SUBMs: ~A\n" submodules)))

    ;; (format outp "######## ~A ########\n" findlib-name)

    (case kind
      ((:ns-archive :ns-library)
       (if *mibl-debug-s7*
           (format #t "~A: ~A~%" (ured "NS") ns))
       (if *mibl-ns-topdown*
           (-emit-topdown-aggregate outp pkg-path kind ns tgtname submodules link-opts cc-deps)
           ;; aggregated bottomup, needs archive or lib w/o ns
           (-emit-bottomup-aggregate outp kind ns tgtname submodules link-opts cc-deps)))

      ((:archive :library)
       (begin
         (if *mibl-debug-s7*
             (format #t "EMITTING NON-NS AGGREGATE: ~A\n" kind))
         (format outp "##############\n")
         (if (eq? kind :library)
             (format outp "ocaml_library( #6\n")
             (format outp "ocaml_archive( #7\n"))
         (format outp "    name     = \"~A\",\n" tgtname)
         ;; (format outp "    manifest = [~{\":~A\"~^, ~}],\n" submodules)
         (format outp "    manifest   = [~%")
         (format outp "~{        \":~A\"~^,~%~}~%" submodules)
         (format outp "    ],~%")
         (if cc-deps
             (begin
               (if *mibl-debug-s7*
                   (format #t "~A: ~A~%" (ugreen "cc-deps") cc-deps))
               (format outp "    cc_deps    = [\"__lib~A__\"]," (cdadr cc-deps))
               (newline outp)))
         (format outp ")\n\n")))

      ;; ((:archive)
      ;;  (begin
      ;;    (format outp "##############\n")
      ;;    (format outp "ocaml_archive(\n")
      ;;    (format outp "    name    = \"~A\",\n" findlib-name)
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
      ;;    (format outp "    name       = \"~A\",\n" findlib-name)

      ;;    (if (use-ns-attr? modname findlib-name)
      ;;        (format outp "    ns_name    = \"~A\",\n" modname))
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
(define (bazel-emit-aggregate-targets outp pkg) ;; fs-path stanzas)
  (if *mibl-debug-s7*
      (format #t "~A: ~A\n" (bgblue "bazel-emit-aggregate-targets") pkg))

  ;; only emit header if aggregators
  (let* ((stanzas (assoc-val :mibl pkg))
         (pkg-path (assoc-val :pkg-path pkg))
         ;; FIXME: exclude null libs, e.g. tezos:src/tooling
         (aggs (filter (lambda (stanza)
                         (member
                          (car stanza)
                          '(:archive :library :ns-archive :ns-library)))
                       stanzas)))
    (if *mibl-debug-s7*
        (format #t "AGGREGATES: ~A\n" aggs))
    (if (not (null? aggs))
        (format outp "############################# Aggregates #############################\n"))

    (for-each (lambda (stanza)
                (if *mibl-debug-s7*
                    (format #t "stanza: ~A\n" stanza))
                (case (car stanza)
                  ((:ns-archive :ns-library) ;; dune library, wrapped
                   (bazel-emit-aggregate-target outp pkg-path stanza))
                  ((:archive :library) ;; dune library, unwrapped
                   (bazel-emit-aggregate-target outp pkg-path stanza))
                  (else (format outp "UNCAUGHT stanza: ~A\n"
                                stanza))))
              aggs)))

                          ;; outp
                          ;; (if (library-namespaced? stanza) :ns-archive :archive)
                          ;; fs-path
                          ;; stanza)

                       ;; (if (library-wrapped? stanza)
                       ;;     (bazel-emit-aggregate-target outp 'ns-archive
                       ;;                                     (cadr stanza))
                       ;;     ;; unwrapped dune libs do not get an aggregate rule,
                       ;;     ;; but they do have flags and deps that apply to their
                       ;;     ;; modules. so we need to emit those syms:
                       ;; CORRECTION: unwrapped libs are handled normally(?)
                       ;;     (bazel-emit-stanza-deps-and-flags outp 'ns-archive
                       ;;                                          (cadr stanza)))

;; (format #t "loaded bazel/aggregates.scm\n")