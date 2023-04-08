(if *mibl-debug-s7*
    (format #t "loading bazel/executables.scm\n"))

;; (define (bazel-emit-test-target outp pkg stanza)
;;   (format #t "~A: ~A~%" (blue "bazel-emit-test-target") stanza))

(define (bazel-emit-testsuite outp pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "bazel-emit-test-target") stanza))
  (format outp "#############\n")
  (format outp "test_suite(~%")
  (format outp "    name     = \"~A\",\n" (assoc-val :name (cdr stanza)))
  (format outp "    tests    = [~%")
  (format outp "~{        \":~A.exe\"~^,~%~}~%" (assoc-val :tests (cdr stanza)))
  (format outp "    ]~%")
  (format outp ")~%"))

(define (bazel-emit-test-target outp ws pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "bazel-emit-test-target") stanza))
  ;; (bazel-emit-executable-target outp ws :test pkg stanza (assoc :prologue (cdr stanza))))
  (bazel-emit-executable-target outp ws :test pkg stanza))

(define  (-decode-sh-test-deps deps ws pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "-decode-sh-test-deps") deps))
  (let ((deps (flatten (map (lambda (dep)
                              (if *mibl-debug-s7*
                                  (format #t "~A: ~A~%" (blue "dep") dep))
                              (if (eq? ::tools (car dep))
                                  (begin) ;; ignore

                                  (if (alist? (cdr dep))
                                      (format #f "$(location ~A)" (label-list->label-string (cdr dep)))
                                      (case (cdr dep)
                                        ((::unresolved)
                                         (format #f "~A" (if (keyword? (car dep)) (keyword->symbol (car dep)) (car dep))))
                                        (else (format #f "FIXME:~A" dep))))
                                  ;; (if (alist? (cdr dep))
                                  ;;     ;;FIXME: unless in this pkg
                                  ;;     (format #f "//~A:~A"
                                  ;;             (assoc-val :pkg (cdr dep))
                                  ;;             (assoc-val :tgt (cdr dep)))
                                  ;;     (if (equal? ::unresolved (cdr dep))
                                  ;;         (format #f "~A" (car dep))
                                  ;;         (format #f ":~A" (car dep))))
                                  ))
                            deps))))
    deps))

;; FIXME: file args need $(location ...)
(define (-decode-sh-test-args args ws pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "-decode-sh-test-args") args))
  (let* ((stanza-alist (cdr stanza))
         (deps (assoc-val :deps stanza-alist))
         (exports (car (assoc-val :exports
                                  (assoc-val ws *mibl-project*)))))
    (if *mibl-debug-s7*
        (begin
          (format #t "~A: ~A~%" (uwhite "deps") deps)
          (format #t "~A: ~A~%" (uwhite "exports tbl") exports)))
    (if deps
        (let ((cooked
               (map (lambda (arg)
                      (if *mibl-debug-s7*
                          (format #t "~A: ~A~%" (uyellow "decoding arg") arg))
                      (let ((xarg (flatten (filter-map
                                            (lambda (dep)
                                              (if *mibl-debug-s7*
                                                  (format #t "~A: ~A~%" (yellow "dep") dep))
                                              (if (eq? ::tools (car dep))
                                                  (let ((t (filter-map (lambda (tool)
                                                                         (if *mibl-debug-s7*
                                                                             (format #t "~A: ~A~%" (yellow "tool") tool))
                                                                         (if (eq? arg (car tool))
                                                                             (label-list->label-string (cdr tool))
                                                                             #f))
                                                                       (cdr dep))))
                                                    (if t
                                                        (if (null? t)
                                                            #f
                                                            (begin
                                                              (if *mibl-debug-s7*
                                                                  (format #t "~A: ~A~%" (yellow "decoded tool") t))
                                                              t))
                                                        #f))
                                                  ;;FIXME: detect file args and wrap in $(location ...)
                                                  (if (eq? arg (car dep))
                                                      (begin
                                                        (if *mibl-debug-s7*
                                                            (format #t "~A: ~A~%" (yellow "found arg in dep") dep))
                                                        (if (alist? (cdr dep))
                                                            (format #f "$(location ~A)" (label-list->label-string (cdr dep)))
                                                            (case (cdr dep)
                                                              ((::unresolved)
                                                               (format #f "~A" (if (keyword? arg) (keyword->symbol arg) arg)))
                                                              (else (format #f "~A" arg))))) ;; (error 'fixme "non-label dep match"))
                                                      #f)))
                                            deps))))
                        (if (not (null? xarg))
                            (begin
                              (if *mibl-debug-s7*
                                  (format #t "~A: ~A~%" (yellow "decoded to dep") xarg))
                              xarg)
                            (begin
                              ;; should not happen, all dep labels should have been resolved by prev pass
                              (if *mibl-debug-s7*
                                  (format #t "~A: ~A~%" (yellow "NOT decoded to dep") xarg))
                              (let ((xarg (hash-table-ref exports arg)))
                                (if xarg
                                    (begin
                                      (if *mibl-debug-s7*
                                          (format #t "~A: ~A~%" (yellow "decoded to export") xarg))
                                      xarg)
                                    arg))))))
                    args)))
          (if *mibl-debug-s7*
              (format #t "~A: ~A~%" (uwhite "cooked args") cooked))
          ;; (error 'fixme "STOP decode")
          (flatten cooked))
        ;; else no deps - args must be files in cwd?
        (begin
          (if *mibl-debug-s7*
              (format #t "~A: ~A~%" (bgred "FIXME") "args w/o deps"))
          args))
        ))

(define (bazel-emit-sh-test-target outp ws pkg stanza)
  (if *mibl-debug-s7*
      (format #t "~A: ~A~%" (ublue "bazel-emit-sh-test-target") stanza))
  (let* ((stanza-alist (cdr stanza))
         ;; (findlib-name (car (assoc-val :alias stanza-alist)))
         (findlib-name (car (assoc-val :name stanza-alist)))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "findlib-name") findlib-name)))
         ;;FIXME: assuming one tool
         (tools (let* ((tool (cadr
                              (assoc-in '(:actions :cmd :tool) stanza-alist))))
                  (if *mibl-debug-s7*
                      (format #t "~A: ~A~%" (uwhite "tool") tool))
                  (case tool
                    ((::diff) "diff")
                    ((:node) "node")
                    (else
                     (-tool-for-genrule (assoc-val :pkg-path pkg)
                                        tool
                                        (assoc-val :deps stanza-alist))
                     ;; (error 'FIXME
                     ;;        (format #f "bad sh-test tool: ~A" tool))
                     ))))
                  ;;      (pkg-path (assoc-val :pkg (cdr ts)))
                       ;; (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "pkg-path")
                       ;;            pkg-path)))
                  ;;      (tgt (assoc-val :tgt (cdr ts)))
                  ;;      (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "tgt") tgt))))
                  ;; (if (equal? pkg-path (car (assoc-val :pkg-path pkg)))
                  ;;     (format #f "~A" tgt)
                  ;;     (format #f "//~A:~A" pkg-path tgt))))
         (deps (assoc-val :deps stanza-alist))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "deps") deps)))
         (deps (if deps (-decode-sh-test-deps deps ws pkg stanza) deps))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "cooked deps") deps)))

         (args (cdr (assoc-in '(:actions :cmd :args) stanza-alist)))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "args") args)))
         (args (-decode-sh-test-args args ws pkg stanza))
         (_ (if *mibl-debug-s7* (format #t "~A: ~A~%" (uwhite "Cooked args") args)))
         )
    (if *mibl-debug-s7*
        (format #t "TARGET: ~A\n" findlib-name))
    ;; (error 'fixme "STOP sh-test")

    ;; (format #t "MAIN: ~A\n" mainname)
    (begin
      (format outp "#############\n")
      (format outp "sh_test(\n"))
    (format outp "    name     = \"~A\",\n" findlib-name)
    (format outp "    srcs     = [\"~A\"],\n" tools)
    (if deps ;; (not (null? deps))
        (begin
          (format outp "    data     = [\n")
          (format outp "~{        \"~A\"~^,~%~}~%" deps)
          (format outp "    ],\n")))
    (if args ;; (not (null? args))
        (begin
          (format outp "    args     = [\n")
          (format outp "~{        ~S~^,~%~}~%" args)
          ;; (format outp "~{        \"$(location ~A)\"~^,~%~}~%" args)
          (format outp "    ],\n")))
    (format outp "    visibility = [\"//visibility:public\"],\n")
    (format outp ")\n")
    (newline outp)
    ))

;; (define (Xbazel-emit-test-target outp pkg stanza)
;;   (format #t "~A: ~A~%" (blue "bazel-emit-test-target") stanza)
;;   (let* ((stanza-alist (cdr stanza))
;;          (privname (assoc-val :privname stanza-alist))
;;          ;; (mainname (normalize-module-name privname))
;;          (findlib-name (if-let ((findlib-name (assoc-val :findlib-name stanza-alist)))
;;                           findlib-name
;;                           privname))
;;          (tgtname (format #f "~A" findlib-name))
;;          (exename privname)

;;          ;; 'name', i.e. main, is required by dune so we always have it
;;          (main (cadr (assoc-in '(:link :main) stanza-alist)))
;;          (_ (if *mibl-debug-s7* (format #t "main: ~A~%" main)))

;;          ;; (deps (assoc-in '(:compile :deps)))
;;          ;; (_ (if *mibl-debug-s7* (format #t "compile deps: ~A~%" deps)))
;;          (manifest (sort! (if-let ((mani
;;                              (assoc-in '(:link :manifest :modules)
;;                                        stanza-alist)))
;;                                  (cdr mani) '())
;;                           sym<?))
;;          (_ (if *mibl-debug-s7* (format #t "manifest: ~A~%" manifest)))
;;          (manifest (remove main manifest))
;;          (_ (if *mibl-debug-s7* (format #t "manifest: ~A~%" manifest)))
;;          )
;;     (let-values (((link-std link-opts)
;;                   (link-flags->mibl stanza)))
;;       (format #t "link opts: ~A~%" link-opts)
;;       (format #t "link std: ~A~%" link-std)

;;       (format #t "TARGET: ~A\n" tgtname)
;;       ;; (format #t "MAIN: ~A\n" mainname)
;;       (format #t "MANIFEST: ~A\n" manifest)
;;       ;; (format #t "SUBMs: ~A\n" submodules)
;;       ;; (format #t "DEPS: ~A\n" deps)

;;       ;; (let-values (((flags opens) (stanza-opts stanza-alist)))
;;       ;;   (if (or flags opens
;;       ;;           (assoc-in '(:opts :raw) stanza-alist))
;;       ;;       (begin
;;       ;;         ;; (format #t "FLAGS: ~A\n" flags)
;;       ;;         ;; (format #t "OPENS: ~A\n" opens)

;;       ;;         (format outp "~A = [\n" (name->opts-sym findlib-name))
;;       ;;         (if flags
;;       ;;             (for-each (lambda (flag)
;;       ;;                         (format outp "    \"~A\",\n" flag))
;;       ;;                       (cadr flags)))
;;       ;;         (if opens
;;       ;;             (for-each (lambda (open)
;;       ;;                         (format outp "    \"-open\", \"~A\",\n" open))
;;       ;;                       (cadr opens)))

;;       ;;         (if (assoc-in '(:opts :raw :standard) stanza-alist)
;;       ;;             (format outp "    ##FIXME: dune (:standard)\n"))

;;       ;;         (format outp "]\n")
;;       ;;         (newline outp))))

;;       ;; for executables, 'deps' are libdeps ('libraries' fld), and
;;       ;; 'modules' are module (src) deps
;;       ;; (if (not (null? deps))
;;       ;; (begin
;;       ;;   (format outp "~A = [\n" (name->deps-sym findlib-name))
;;       ;;   (for-each (lambda (dep)
;;       ;;               (format outp "    \"~A\",\n" dep)
;;       ;;               )
;;       ;;             deps)
;;       ;;   (format outp "]\n")
;;       ;;   (newline outp))
;;       ;; ;; )


;;       ;; (begin
;;       ;;   (format outp "###########\n")
;;       ;;   (format outp "ocaml_test(\n")
;;       ;;   (format outp "    name     = \"~A\",\n" tgtname)
;;       ;;   (if (not (null? link-opts))
;;       ;;       (format outp "    opts     = [~{\"~A\"~^, ~}],\n" link-opts))

;;       ;;   (if main
;;       ;;       (format outp "    main     = \"~A\",\n" main))

;;       ;;   ;; (if (not (null? manifest))
;;       ;;   ;;     (format outp "    manifest = [~{\":~A\"~^, ~}]\n\n" manifest))

;;       ;;       ;; (begin
;;       ;;       ;;   ;; (format #t "MODDEPS: ~A\n" modules)
;;       ;;       ;;   (format outp "    deps = [\n"))
;;       ;;       ;;   (let ((mods (sort! (hash-table-keys
;;       ;;       ;;                       (remove-if list
;;       ;;       ;;                                  (lambda (entry)
;;       ;;       ;;                                    ;; (format #t "ENTRY ~A\n" entry)
;;       ;;       ;;                                    (equal? (cdr entry) :main))
;;       ;;       ;;                                  modules))
;;       ;;       ;;                      sym<?)))
;;       ;;       ;;     (for-each (lambda (mod) ;; mod:: (modsym . type)
;;       ;;       ;;                 ;; (format #t "mod: ~A\n" mod)
;;       ;;       ;;                 ;; (if (not (equal? (cdr mod) :main))
;;       ;;       ;;                 (format outp "        \":~A\",\n"
;;       ;;       ;;                         (symbol->string
;;       ;;       ;;                          (normalize-module-name mod))
;;       ;;       ;;                         )
;;       ;;       ;;                 ;; )
;;       ;;       ;;                 )
;;       ;;       ;;               mods))
;;       ;;       ;;   ;; (for-each (lambda (mod)
;;       ;;       ;;   ;;             (format outp "        \"~A\",\n" mod))
;;       ;;       ;;   ;;           modules)
;;       ;;       ;;   (format outp "    ],\n")))

;;       ;;   (format outp "    visibility = [\"//visibility:public\"],\n")
;;       ;;   (format outp ")\n")
;;       ;;   (newline outp)

;;         ;; emit the test runner module
;;         ;; (let ((runner (assoc-in
;;         ;; (if pkg-modules (-emit-modules outp pkg pkg-modules))
;;         ;; (if (or sigs *mibl-build-dyads*)
;;         ;;     (-emit-signatures outp pkg sigs pkg-modules))
;;         ;; ))

;;         ;;(format outp "#############################\n")
;;         )))

(define (bazel-emit-test-targets outp ws pkg) ;;fs-path stanzas)
  (if *mibl-debug-s7*
      (format #t "~A\n" (ublue "bazel-emit-test-targets")))

  (let* ((stanzas (assoc-val :mibl pkg))
         (hdr-flag #t))
    (for-each (lambda (stanza)
                (if *mibl-debug-s7*
                    (format #t "~A: ~A\n" (uwhite "stanza") (car stanza)))
                (case (car stanza)
                  ((:testsuite)
                   (if hdr-flag
                       (begin
                         (format outp "##############################\n")
                         (format outp "####  Test Suites  ####\n")
                         (set! hdr-flag #f)))
                   (bazel-emit-testsuite outp pkg stanza))

                  ((:test)
                   (if hdr-flag
                       (begin
                         (format outp "##############################\n")
                         (format outp "####  Test Targets  ####\n")
                         (set! hdr-flag #f)))
                   (bazel-emit-test-target outp ws pkg stanza))

                  ((:tests)
                   (error 'bad-arg "unexpected :tests stanza")
                   ;; (bazel-emit-tests
                   ;;  outp fs-path stanza)
                   )

                  ((:sh-test)
                   (if hdr-flag
                       (begin
                         (format outp "##############################\n")
                         (format outp "####  Test Targets  ####\n")
                         (set! hdr-flag #f)))
                   (bazel-emit-sh-test-target outp ws pkg stanza))

                  (else ;; ignore others
                   ;; (error 'bad-arg "unexpected test stanza")
                   )))
              stanzas)))

