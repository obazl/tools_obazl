(define (opam-dep->opam-label dep)
  ;;FIXME: lookup module name in opam dune-package
  (let* ((m (cdr dep))
         (pkg (format #f "~A" m)))
    (if (char-upper-case? (pkg 0))
        (vector (car dep) (cdr dep))
        (format #f "@~A//lib/~A" m m))))

;; OBSOLETE: deps are already resolved, by bazel-resolve-deps!
(define (local-deps->bazel ml-deps mli-deps)
  (let ((ml-locals (map (lambda (dep)
                          (let ((m (cdr dep)))
                            (case (car dep)
                              ((:here) (format #f ":~A" m))
                              ((:local) (let ((pkg (assoc-val :pkg (cdr dep)))
                                              (tgt (assoc-val :tgt (cdr dep))))
                                          (format #f "//~A:~A" pkg tgt)))
                              ((:builtin) (format #f "@ocaml//~A" m))
                              ((:opam?) (opam-dep->opam-label dep))
                              ((:unresolved) (error 'Unresolved-local-ml-dep "Unresolved local ml dep"))
                              (else (error 'Unexpected-local-dep "Unexpected local ml dep")))))
                        ml-deps))
        (mli-locals (map (lambda (dep)
                           (let ((m (cdr dep)))
                             (case (car dep)
                               ((:here) (format #f ":~A" m))
                               ((:local) (let ((pkg (assoc-val :pkg (cdr dep)))
                                               (tgt (assoc-val :tgt (cdr dep))))
                                           (format #f "//~A:~A" pkg tgt)))
                               ((:builtin) (format #f "@ocaml//~A" m))
                               ((:opam?) (opam-dep->opam-label dep))
                               ((:unresolved) (error 'Unresolved-local-ml-dep "Unresolved local ml dep"))
                               (else (error 'Unexpected-local-dep "Unexpected local mli dep")))))
                         mli-deps)))
    (mibl-trace "ml-locals"  ml-locals)
    (mibl-trace "mli-locals" mli-locals)
    ;; (if (truthy? ml-deps) (error 'x "X"))
    (values ml-locals mli-locals)
    ;; (values (sort! ml-locals string<?) (sort! mli-locals string<?))
    ))

(define -module->local-deps-labels
  (let ((+documentation+ "Lookup module (normalized name) in pkg (:modules, :structures) and return depslist.")
        (+signature+ '(module->local-deps module pkg)))
    (lambda (module pkg)
      (mibl-trace-entry "module->local-deps-labels" module)
      (let* ((module-tlbl (module-name->tagged-label module pkg)))
        (mibl-trace "Module-tlbl" module-tlbl)
        (if (alist? (cdr module-tlbl))
            (let* ((ml-deps (if-let ((deps (assoc-val :ml (cdr module-tlbl))))
                                    (if (list? deps)
                                        (cdr deps)
                                        '())
                                    (if-let ((deps (assoc-val :ml_ (cdr module-tlbl))))
                                            (cdr deps)
                                            '())))
                   (mibl-trace-let "Ml-deps" ml-deps)
                   (mli-deps (if-let ((deps (assoc-val :mli (cdr module-tlbl))))
                                     (if (list? deps)
                                         (cdr deps)
                                         '())
                                     (if-let ((deps (assoc-val :mli_ (cdr module-tlbl))))
                                             (cdr deps)
                                             '())))
                   (mibl-trace-let "MLI-deps" mli-deps)
                   )
              (mibl-trace "mli-deps 2" mli-deps)
              (values ml-deps mli-deps))
            (begin
              (mibl-trace "xxxxxxxxxxxxxxxx" "")
              ;; else must be a struct, (Foo foo.ml Dep1 ...)
              (if (list? (cdr module-tlbl))
                  (let ((ml-deps (cddr module-tlbl)))
                    (values ml-deps '()))
                  (values '() '()))))))))

(define (module->bazel-local-deps module pkg)
  (mibl-trace-entry "module->bazel-local-deps" module)
  (let-values (((ml-deps mli-deps) (-module->local-deps-labels module pkg)))
    (mibl-trace "ml-deps" ml-deps)
    (mibl-trace "mli-deps" mli-deps)
    ;;(local-deps->bazel ml-deps mli-deps)
    (values ml-deps mli-deps)
    ))

;;FIXME: 2 routines, emit-deps, emit-exec-module-deps?
(define (emit-deps outp
                    this-is-main ;;FIXME: remove?
                    prologue
                    deps-tag ;; index to :shared-deps, or ??
                    ;; stanza
                    agg-deps local-deps selectors testsuite)
  (mibl-trace-entry "emit-deps" deps-tag)
  (mibl-trace "agg-deps" agg-deps)
  (mibl-trace "local-deps" local-deps)
  (mibl-trace "deps-tag" deps-tag)
  (mibl-trace "this-is-main" this-is-main)
  (mibl-trace "prologue" prologue)

  (if (or (number? deps-tag)
          (truthy? local-deps)
          (truthy? agg-deps)
          (and this-is-main (truthy? prologue))
          selectors)
      (format outp "    deps          = ")
      )

  (if (and this-is-main (truthy? prologue))
      (begin
        (format outp "01: [\":lib~A\"]" prologue))
      ;; NB: omit trailing comma (and newline) in case select follows
      (if (null? local-deps)
          (if (null? agg-deps)
              (if (number? deps-tag)
                  (format outp "DEPS_~A" (if testsuite testsuite deps-tag))
                  ;; (error 'FIXME
                  ;;        (format
                  ;;         #f "found non-numeric deps-tag ~A but no deps"
                  ;;         deps-tag))
                  ;; else emit nothing
                  )
              ;; agg-deps e.g. ((:resolved @ocaml//compiler-libs/common))
              (if (number? deps-tag)
                  (error 'FIXME
                         (format
                          #f "found both numeric deps-tag ~A and agg-deps ~A"
                          deps-tag agg-deps))
                  (if this-is-main ;; prologue
                      (if prologue
                          (format outp "03: [\":~A_execlib\"]" prologue)
                          (format outp "04: DEPS_~A" deps-tag))
                      (format outp "DEPS_~A"
                              (if testsuite testsuite deps-tag)))))
          ;; have local-deps (note: trailing comma+newline added below)
          (if (null? agg-deps)
              (if (number? deps-tag)
                  (if this-is-main ;; prologue
                      (format outp "06: [\":~A_execlib\"]1" prologue)
                      (begin
                        (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                        (format outp "~{        ~S~^,~%~}\n" local-deps)
                        (format outp "    ]")))
                  ;; else
                  (begin
                    ;; (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                    (format outp "08: [\n")
                    (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
                    (format outp "    ]")))
              (begin
                (format outp "DEPS_~A + [\n" (if testsuite testsuite deps-tag))
                (format outp "~{        \"~A\"~^,~%~}\n" local-deps)
                (format outp "    ]")))))

  ;; (if (not (null? local-deps))
  ;;     (if (not (null? agg-deps))
  ;;         (begin
  ;;           (if (equal? :executable (car stanza))
  ;;               (format outp " ~A_EXE_DEPS + [\n" (if testsuite testsuite deps-tag))
  ;;               (format outp "~A_DEPS + [\n" (if testsuite testsuite deps-tag)))
  ;;           (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
  ;;           (format outp "    ]"))
  ;;         (begin
  ;;           (format outp "[~%")
  ;;           (format outp "~{        \":~A\"~^,~%~}\n" local-deps)
  ;;           (format outp "    ]")))
  ;;     ;; else no local-deps
  ;;              ;;     (if (not (null? agg-deps))
  ;;              ;;           (if (equal? :executable (car stanza))
  ;;              ;;               (format outp "    deps          = ~A_EXE_DEPS,~%" (if testsuite testsuite deps-tag))
  ;;              ;;               (format outp "A    deps          = ~A_DEPS,~%" (if testsuite testsuite deps-tag)))))
  ;;     (if (not (null? agg-deps))
  ;;         (format outp "~A_DEPS" (if testsuite testsuite deps-tag))))

  (mibl-trace "selectors" selectors *mibl-debug-emit*)
  (if selectors
      (begin
        (if (or *mibl-debug-emit* *mibl-debug-s7*)
            (format #t "~A: ~A~%" (uwhite "emitting selectors") selectors))
        (if (or (truthy? local-deps)
                (truthy? agg-deps)
                (truthy? deps-tag))
            (format outp " + "))
        (format outp " select({~%")
        ;; (format outp "~{        \"//bzl/import:~A?\": \"~A\",~^~%~}~%"
        ;;         src-selectors)
        (format outp "~{        \"//bzl/import:~A?\": [\"~A\"],~^~%~}~%"
                (flatten selectors))
        (format outp "        \"//conditions:default\": []~%")
        (format outp "    }),  ## ~%"))

      ;; else no selectors, finish with comma
      (if (or (number? deps-tag)
              (truthy? local-deps)
              (truthy? agg-deps)
              (and this-is-main (truthy? prologue)))
              ;; selectors)
          (format outp ",~%"))
      ))
