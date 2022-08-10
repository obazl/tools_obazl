;; (format #t "loading starlark/executables.scm\n")

;; (define (starlark-emit-test-target outp pkg stanza)
;;   (format #t "~A: ~A~%" (blue "starlark-emit-test-target") stanza))

(define (starlark-emit-test-target outp pkg stanza)
  (format #t "~A: ~A~%" (ublue "starlark-emit-test-target") stanza)
  (starlark-emit-executable-target outp :test pkg stanza))

(define (starlark-emit-sh-test-target outp pkg stanza)
  (format #t "~A: ~A~%" (ublue "starlark-emit-sh-test-target") stanza)
  (let* ((stanza-alist (cdr stanza))
         (pubname (car (assoc-val :alias stanza-alist)))
         (_ (format #t "~A: ~A~%" (uwhite "pubname") pubname))
         ;;FIXME: assuming one tool
         (tools (let* ((ts (cadr
                            (assoc-in '(:deps ::tools) stanza-alist)))
                       (_ (format #t "~A: ~A~%" (uwhite "ts") ts))
                       (pkg-path (assoc-val :pkg (cdr ts)))
                       (_ (format #t "~A: ~A~%" (uwhite "pkg-path")
                                  pkg-path))
                       (tgt (assoc-val :tgt (cdr ts)))
                       (_ (format #t "~A: ~A~%" (uwhite "tgt") tgt)))
                  (if (equal? pkg-path (car (assoc-val :pkg-path pkg)))
                      (format #f "~A" tgt)
                      (format #f "//~A:~A" pkg-path tgt))))
         (args  (cdr (assoc-in '(:actions :cmd :args) stanza-alist)))
         )
      (format #t "TARGET: ~A\n" pubname)
      ;; (format #t "MAIN: ~A\n" mainname)
      (begin
        (format outp "#############\n")
        (format outp "sh_test(\n"))
        (format outp "    name     = \"~A\",\n" pubname)
        (format outp "    srcs     = [\"~A\"],\n" tools)
        ;; (format outp "    deps     = [\"~A\"],\n" "dep1")
        (format outp "    data     = [\n")
        (format outp "~{        \"~A\"~^,~%~}~%" (cdr args))
        (format outp "    ],\n")
        (format outp "    args     = [\n")
        (format outp "~{        \"~A\"~^,~%~}~%" (cdr args))
        (format outp "    ],\n")
        (format outp "    visibility = [\"//visibility:public\"],\n")
        (format outp ")\n")
        (newline outp)
        ))

;; (define (Xstarlark-emit-test-target outp pkg stanza)
;;   (format #t "~A: ~A~%" (blue "starlark-emit-test-target") stanza)
;;   (let* ((stanza-alist (cdr stanza))
;;          (privname (assoc-val :privname stanza-alist))
;;          ;; (mainname (normalize-module-name privname))
;;          (pubname (if-let ((pubname (assoc-val :pubname stanza-alist)))
;;                           pubname
;;                           privname))
;;          (tgtname (format #f "~A" pubname))
;;          (exename privname)

;;          ;; 'name', i.e. main, is required by dune so we always have it
;;          (main (cadr (assoc-in '(:link :main) stanza-alist)))
;;          (_ (format #t "main: ~A~%" main))

;;          ;; (deps (assoc-in '(:compile :deps)))
;;          ;; (_ (format #t "compile deps: ~A~%" deps))
;;          (manifest (sort! (if-let ((mani
;;                              (assoc-in '(:link :manifest :modules)
;;                                        stanza-alist)))
;;                                  (cdr mani) '())
;;                           sym<?))
;;          (_ (format #t "manifest: ~A~%" manifest))
;;          (manifest (remove main manifest))
;;          (_ (format #t "manifest: ~A~%" manifest))
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

;;       ;;         (format outp "~A = [\n" (name->opts-sym pubname))
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
;;       ;;   (format outp "~A = [\n" (name->deps-sym pubname))
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
;;         ;; (if (or sigs *build-dyads*)
;;         ;;     (-emit-signatures outp pkg sigs pkg-modules))
;;         ;; ))

;;         ;;(format outp "#############################\n")
;;         )))

(define (starlark-emit-test-targets outp pkg) ;;fs-path stanzas)
  (format #t "~A\n" (ublue "starlark-emit-test-targets"))

  (let* ((stanzas (assoc-val :dune pkg))
         (hdr-flag #t))
    (for-each (lambda (stanza)
                (format #t "~A: ~A\n" (uwhite "stanza") (car stanza))
                (case (car stanza)
                  ((:test)
                   (if hdr-flag
                       (begin
                         (format outp "##############################\n")
                         (format outp "####  Test Targets  ####\n")
                         (set! hdr-flag #f)))
                   (starlark-emit-test-target outp pkg stanza))

                  ((:tests)
                   (error 'bad-arg "unexpected :tests stanza")
                   ;; (starlark-emit-tests
                   ;;  outp fs-path stanza)
                   )

                  ((:sh-test)
                   (if hdr-flag
                       (begin
                         (format outp "##############################\n")
                         (format outp "####  Test Targets  ####\n")
                         (set! hdr-flag #f)))
                   (starlark-emit-sh-test-target outp pkg stanza))

                  (else ;; ignore others
                   ;; (error 'bad-arg "unexpected test stanza")
                   )))
              stanzas)))

