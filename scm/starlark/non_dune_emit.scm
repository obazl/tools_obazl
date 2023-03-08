(if (or *debug-emit* *debugging*)
    (format #t "loading non_dune_emit.scm~%"))

(define (emit-non-dune-global-vars outp pkg obazl-rules)
  (if (or *debug-emit* *debugging*)
      (begin
        (format #t "~A: ~A~%" (ublue "emit-non-dune-global-vars") obazl-rules)
        (format #t "Pkg: ~A~%" pkg)))
  (if (member :sig obazl-rules)
      (format outp "OPTS_SIG    = []\n"))
  (if (member :struct obazl-rules)
      (format outp "OPTS_MODULE = []\n"))
  (newline outp)
  )

(define (emit-non-dune-aggregate-target outp pkg)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A\n" (ublue "emit-non-dune-aggregate-target") pkg))
  (let* ((pkg-modules (if-let ((ms (assoc-in '(:modules) pkg)))
                              (cdr ms) '()))
         (pkg-structs (pkg->structs pkg))
         (pkg-sigs (pkg->sigs pkg))
         )

    (if (or *debug-emit* *debugging*)
        (begin
          (format #t "~A: ~A\n" (ucyan "pkg-modules") pkg-modules)
          (format #t "~A: ~A\n" (ucyan "pkg-structs") pkg-structs)
          (format #t "~A: ~A\n" (ucyan "pkg-sigs") pkg-sigs)))

    (if (or (truthy? pkg-modules)
            (truthy? pkg-structs)
            (truthy? pkg-sigs))

        (let* ((kind :library)
               (libname (normalize-module-name (pkg->pkg-name pkg)))
               (submodules (if-let ((submods (assoc-val :modules pkg)))
                                   (map car submods) '()))
               (substructs (if-let ((submods (assoc-in '(:structures :static)
                                                       pkg)))
                                   (map car (cdr submods)) '()))
               (subs (append submodules substructs))
               )

          (format outp "##############\n")
          (format outp "ocaml_library(\n")
          (format outp "    name     = \"lib~A\",\n" libname)
          (format outp "    manifest   = [~%")
          (format outp "~{        \":~A\"~^,~%~}~%" subs)
          (format outp "    ],~%")
          ;; (if cc-deps
          ;;     (begin
          ;;       (if (or *debug-emit* *debugging*)
          ;;           (format #t "~A: ~A~%" (ugreen "cc-deps") cc-deps))
          ;;       (format outp "    cc_deps    = [\"__lib~A__\"]," (cdadr cc-deps))
          ;;       (newline outp)))
          (format outp ")\n\n")))))

(define (emit-non-dune-cc-target outp cc)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A~%" (ublue "emit-cc-target") cc))
  (format outp "################ ################")
  (newline outp)

  (let ((nm (car (assoc-val :name cc))))

    (format outp "cc_selection_proxy(\n")
    (format outp "    name = \"__lib~A__\",~%" nm)
    (format outp "    selection = select({~%")
    (format outp "        \"@ocaml//platforms:vm?\": [\"dll~A.stubs.so\"],~%" nm)
    (format outp "        \"@ocaml//platforms:sys?\": [\"~A.stubs\"],~%" nm)
    (format outp "        \"//conditions:default\": [\"~A.stubs\"]" nm)
    (format outp "    })~%")
    (format outp ")~%")

    (newline outp)
    (format outp "cc_binary(\n")
    (format outp "    name = \"dll~A.stubs.so\",~%" (car (assoc-val :name cc)))
    (format outp "    linkshared = True,~%")
    (format outp "    srcs = [~{\"~A.c\"~^, ~}],~%" (car (assoc-val :c-srcs cc)))
    (format outp "    deps = [\"@ocaml//c\"],~%")
    (format outp "    copts = [\"-I\", \"external/ocaml/c\"]~%")
    (format outp ")~%")

    (newline outp)
    (format outp "cc_library(\n")
    (format outp "    name = \"~A.stubs\",~%" (car (assoc-val :name cc)))
    (format outp "    linkstatic = True,~%")
    (format outp "    srcs = [~{\"~A.c\"~^, ~}],~%" (car (assoc-val :c-srcs cc)))
    (format outp "    deps = [\"@ocaml//c\"],~%")
    (format outp "    copts = [\"-I\", \"external/ocaml/c\"]~%")
    (format outp ")~%"))
    )

(define (emit-non-dune-cc-targets outp ws pkg)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A\n" (bgblue "emit-non-dune-cc-targets") pkg))
  (let ((cc-srcs (if-let ((cc-srcs (assoc-in '(:cc-srcs :static) pkg)))
                         (cdr cc-srcs) '()))
        (cc-hdrs (if-let ((cc-hdrs (assoc-in '(:cc-hdrs :static) pkg)))
                         (cdr cc-hdrs) '())))
    ;; (format outp "~A\n" cc-srcs)
    ;; (format outp "~A\n" cc-hdrs)
    (if (or (truthy? cc-srcs) (truthy? cc-hdrs))
        (begin
          (format outp "################################################################\n")
          (format outp "cc_library(\n")
          (format outp "    name = \"FIXME.stubs\",~%")
          (format outp "    linkstatic = True,~%")
          (format outp "    srcs = [~{\"~A\"~^, ~}],~%" (append cc-srcs cc-hdrs))
          (format outp "    deps = [\"@ocaml//c\"],~%")
          (format outp "    copts = [\"-I\", \"external/ocaml/c\"]~%")
          (format outp ")~%")))))

(define (non-dune-pkg->obazl-rules pkg)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A~%" (ublue "non-dune-pkg->obazl-rules") pkg))
  (let* ((rules (if-let ((ms (assoc :modules pkg)))
                         '(:struct :sig) '()))
         (rules (if (truthy? rules) rules
                    (if (assoc '(:structures) pkg)
                        `(:struct ,@rules) rules)))
         (rules (if (member :sig rules) rules
                    (if (assoc :signatures pkg)
                        `(:sig ,@rules) rules)))
         (rules (if (assoc :lex pkg)
                    `(:lex ,@rules) rules))
         (rules (if (assoc :yacc pkg)
                    `(:yacc ,@rules) rules))
         )
    (if (or *debug-emit* *debugging*)
        (format #t "~A: ~A~%" (bgred "rules ct") (length rules)))
    ;; dedup
    rules ;;(sort! rules sym<?)
    ))

(define (-emit-non-dune-sig outp ws pkg sig)
  (if (or *debug-emit* *debugging*)
      (begin
        (format #t "~A\n" (bgblue "-emit-non-dune-sig"))
        (format #t "~A: ~A\n" (blue "sig") sig)))
  (let* (;; (stanza-alist (cdr stanza))
         ;; (libname (string-append
         ;;           (string-upcase
         ;;            (stringify
         ;;             (assoc-val :privname (cdr stanza))))))
         ;; (ns (assoc-val :ns (cdr stanza)))
         ;; (opts (if-let ((opts (assoc-val :opts stanza-alist)))
         ;;               opts #f))
         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "OPTS") opts)))
         ;; (opts-tag (if opts
         ;;               (if (number? opts) opts
         ;;                   libname)
         ;;               #f))
         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (ugreen "opts-tag") opts-tag)))

         (deps-tag #f)
         ;; (deps-tag (if-let ((shared (assoc-in '(:deps :resolved) stanza-alist)))
         ;;                   (if (number? (cdr shared)) (cdr shared) libname)
         ;;                   libname))
         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "deps-tag") deps-tag)))

         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (green "module") (car module))))
         (this-is-main #f)
         ;; (this-is-main (if-let ((main (assoc-val :main stanza-alist)))
         ;;                       (begin
         ;;                         (format #t "~A: ~A~%" (green "main") main)
         ;;                         (if (string=? (format #f "~A" main) (format #f "~A" (car module)))
         ;;                             main #f))))
         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (bggreen "this-is-main") this-is-main)))

         (exec-lib? #f)
         ;; (exec-lib? (if-let ((exec-lib (assoc-val :exec-lib stanza-alist)))
         ;;                    (let ((m (normalize-module-name pkg-name)))
         ;;                      (if (> pkg-exec-libs-ct 0)
         ;;                          (format #f "~A_execlib_~A" m exec-lib)
         ;;                          (format #f "~A_execlib" m)))
         ;;                    #f))

         ;; (agg-deps (if-let ((deps (assoc-val :deps stanza-alist)))
         ;;                   (dissoc '(:conditionals :seldeps) deps)
         ;;               ;; else :executable or :test
         ;;               ;; FIXME: should not be any deps in :compile ?
         ;;               (if-let ((deps (assoc-in '(:compile :deps)
         ;;                                        stanza-alist)))
         ;;                       deps
         ;;                       '())))
         ;; (agg-deps (if (number? deps-tag)
         ;;               (dissoc '(:deps :resolved) agg-deps)
         ;;               agg-deps))

         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "Agg-deps") agg-deps)))

         (local-deps (if-let ((locals (assoc :mli-deps (cdr sig))))
                             (cdr locals)
                             '()))

         ;; (deps-conditional (assoc-in '(:deps :conditionals) stanza-alist))
         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "deps-conditional") deps-conditional)))

         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "module") module)))
         (testsuite #f)

         )

    (let* ((modname (car sig))
           (sigfile (if (proper-list? sig)
                        (-module-record->sigfile (cdr sig))
                        (cdr sig))))

      (if (or *debug-emit* *debugging*)
          (format #t "emitting signature A: ~A\n" modname))

      (format outp "ocaml_signature(\n")
      (format outp "    name          = \"~A_cmi\",\n" modname)
      ;; (if (and ns (not *ns-topdown*))
      ;;     (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
      (format outp "    src           = \"~A\",\n" sigfile)
      (format outp "    opts          = OPTS_SIG,\n")
      (if (or *debug-emit* *debugging*)
          (format #t "~A: ~A~%" (blue "emitting deps A") deps-tag))
      (-emit-deps outp
                  #f ;;this-is-main
                  #f ;; exec-lib?
                  #f ;; deps-tag
                  ;; stanza
                  '() ;; agg-deps
                  local-deps
                  #f ;; dep-selectors
                  #f ;;testsuite
                  )
      ;; (-emit-deps outp this-is-main exec-lib? deps-tag
      ;;             ;; stanza
      ;;             agg-deps local-deps dep-selectors testsuite)

      (format outp ")~%")
      (newline outp)
      )))

(define (-emit-non-dune-signatures outp ws pkg sigs pkg-modules)
  (if (or *debug-emit* *debugging*)
      (begin
        (format #t "~A: ~A\n" (bgblue "-emit-non-dune-signatures") sigs)
        (format #t "*build-dyads*: ~A\n" *build-dyads*)
        (format #t "PKG: ~A\n" pkg)))

  (if (truthy? sigs)
      (-emit-sigs-hdr outp ws sigs pkg-modules))

  (for-each
   (lambda (sig)
     (if (or *debug-emit* *debugging*)
         (format #t "~A: ~A\n" (uwhite "free-standing sig") sig))
     (-emit-sig-freestanding outp ws sig))
   sigs)
  )

(define (-emit-non-dune-module outp ws module pkg)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A\n" (bgblue "-emit-non-dune-module") module))
  (let* ((pkg-name (pkg->pkg-name pkg))
         ;;TODO: get archive flag, ns from miblrc?
         (local-deps (if (proper-list? module)
                         (if (alist? (cdr module))
                             ;; (A (:ml a.ml) (:mli a.mli) (:ml-deps...) (:mli-deps...))
                             (let* ((ml-locals (if-let ((locals (assoc :ml-deps (cdr module))))
                                                       (cdr locals)
                                                       '()))
                                    (mli-locals (if-let ((locals (assoc :mli-deps (cdr module))))
                                                        (cdr locals)
                                                        '()))
                                    (locals (concatenate ml-locals mli-locals)))
                               (remove-duplicates locals))
                             ;; else (A a.ml Foo Bar ...)
                             (cdr module))
                         ;; else (A . a.ml) from :structures
                         ;; should not happen?
                         (cdr module)))

         (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "Local-deps") local-deps)))

         (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "module") module)))

         ;;TODO: get opts miblrc?
         ;; (opts (if-let ((opts (assoc-val :opts stanza-alist)))
         ;;               opts #f))
         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (uwhite "OPTS") opts)))

         ;; (opts-tag (if (number? opts) opts libname))
         ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (ugreen "opts-tag") opts-tag)))

         ;;TODO: get ppx info from miblrc?

         )
    ;; FIXME: if *build-dyads* then emit both ocaml_module and ocaml_signature

    ;; (if (proper-list? module)
        (if (alist? (cdr module))
            ;; proper alist (A (:ml a.ml)(:mli a.mli)) (or :ml_, :mli_)
            (let* ((_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%"
                              (red "emitting module (proper assoc-list)") module)))
                   (modname (car module))
                   (srcs    (cdr module))
                   (select-sigfile #f)
                   ;; (select-sigfile (assoc-val :mli_ srcs))
                   ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (red "select-sigfile") select-sigfile)))
                   ;; (sigfile (if select-sigfile
                   ;;              (make-selector module stanza)
                   ;;              (assoc-val :mli srcs)))
                   (select-structfile #f) ;; (assoc-val :ml_ srcs))
                   ;; (_ (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (red "select-structfile") select-structfile)))
                   ;; (structfile (if select-structfile
                   ;;                 (make-selector module stanza)
                   ;;                 (assoc-val :ml srcs)))
                   ;; ;; for case deps/dynamic
                   ;; (structfile (if structfile structfile
                   ;;                 (assoc-val :ml_ srcs)))

                   (sigfile (if-let ((mli (assoc-val :mli srcs)))
                                    mli
                                    (assoc-val :mli_ srcs)))
                   (structfile (if-let ((mli (assoc-val :ml srcs)))
                                    mli
                                    (assoc-val :ml_ srcs)))
                   )
              ;; (error 'STOP "STOP selmod")
              ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
              ;;         ;;               (cdr opts) '())))
              (if (or *debug-emit* *debugging*)
                  (format #t "emitting module (proper): ~A: ~A\n" modname srcs))

              (format outp "ocaml_module(\n")

              (format outp "    name          = \"~A\",\n" modname)
              ;; (if (and ns (not *ns-topdown*))
              ;;     (format outp "    ns_resolver   = \":ns.~A\",\n" ns))

              (format outp "    struct        = \"~A\",~%" structfile)

              ;; (if (or select-structfile select-sigfile)
              ;;     (format outp "    module        = \"~A\",\n" modname))

              (if *build-dyads*
                  (format outp "    sig           = \":~A_cmi\",\n" modname)
                  ;; else
                  (if select-sigfile
                      (begin
                        (format outp "    sig           = select(~%")
                        (format outp "        {~%")
                        (format outp "~{~{~8T~S: \"~S\",~%~}~}" sigfile)
                        (format outp "        },~%")
                        (format outp "        no_match_error=\"no file selected\"),\n")
                        )
                      ;; else
                      (begin
                        (format outp "    sig           = \"~A\",\n" sigfile)
                        )))

              (format outp "    opts          = OPTS_MODULE,\n")

              ;; (format outp "## emitting deps A: ~A~%" deps-tag)
              ;; deps-tag: #f
              (-emit-deps outp
                          #f ;;this-is-main
                          #f ;; exec-lib?
                          #f ;; deps-tag
                          ;; stanza
                          '() ;; agg-deps
                          local-deps
                          #f ;; dep-selectors
                          #f ;;testsuite
                          )

              ;; (if (not (null? local-deps))
              ;;     (format outp "    local-deps          = ~A,\n" local-deps))

              (format outp ")\n")
               (newline outp)
              )
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; proper list, not alist: (Mytest mytest.ml Hello)
            (let* ((_ (if (or *debug-emit* *debugging*) (format #t "emitting module (proper list): ~A\n" module)))
                   (modname (car module))
                   (structfile (if (proper-list? module) (cadr module)  (cdr module)))
                   (local-deps (if (proper-list? module) (cddr module) '())))
                   ;; (opts (if-let ((opts (assoc :opts (cdr stanza))))
                   ;;               (cdr opts) '())))

              (format outp "ocaml_module(\n")
              (format outp "    name          = \"~A\",\n" modname)
              ;; (if (and ns (not *ns-topdown*))
              ;;      (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
              (format outp "    struct        = \"~A\",~%" structfile)

               (format outp "    opts          = OPTS_MODULE,\n")

               ;; (format outp "## emitting deps B: ~A~%" deps-tag)
               (-emit-deps outp
                           #f ;; this-is-main
                           #f ;; exec-lib?
                           #f ;; deps-tag
                           ;; stanza
                           '() ;;agg-deps
                           local-deps
                           #f ;; dep-selectors
                           #f ;; testsuite
                           )

               (format outp ")\n")
               (newline outp)
               ))
    )

  (if *build-dyads*
      (if (alist? (cdr module))
          (if (or (assoc :mli (cdr module))
                  (assoc :mli_ (cdr module)))
              (-emit-non-dune-sig outp ws pkg module)))))

(define (-emit-non-dune-modules outp ws pkg modules)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A\n" (bgblue "-emit-non-dune-modules") modules))

  (for-each
   (lambda (module)
     (if (or *debug-emit* *debugging*)
         (format #t "~%~A: ~A  ~A\n" (ublue "next module")
                 (ured (car module)) module))
     (format outp "#############~%") ;;FIXME: if not first
     (-emit-non-dune-module outp ws module pkg))
   ;;(sort! modules (lambda (a b) (sym<? (car a) (car b))))
   modules
   ))

(define (emit-non-dune-singletons outp ws pkg)
  (if (or *debug-emit* *debugging*)
      (format #t "~A: ~A\n" (blue "emit-non-dune-singletons") pkg))

  (let* ((pkg-modules (if-let ((ms (assoc-in '(:modules) pkg)))
                              (cdr ms) '()))
         (pkg-structs (pkg->structs pkg))
         ;; (structs-static (if-let ((structs (assoc-in
         ;;                                    '(:structures :static) pkg)))
         ;;                         (cdr structs) '()))
         ;; (pkg-modules (concatenate modules-static pkg-structs)) ;; structs-static))

         (pkg-sigs (pkg->sigs pkg))
         ;; (sigs-static (if-let ((sigs (assoc-in
         ;;                              '(:signatures :static) pkg)))
         ;;                      (cdr sigs) '()))
         ;; (sigs sigs-static))
         )

    (if (or *debug-emit* *debugging*)
        (begin
          (format #t "~A: ~A\n" (ucyan "pkg-modules") pkg-modules)
          (format #t "~A: ~A\n" (ucyan "pkg-structs") pkg-structs)
          (format #t "~A: ~A\n" (ucyan "pkg-sigs") pkg-sigs)))

    (if (or (not (null? pkg-modules))
            (not (null? pkg-structs))
            (not (null? pkg-sigs)))
        (begin
          (format outp "######################## Modules & Signatures ########################")
          (newline outp)))

    (if (truthy? pkg-modules)
        (begin
          (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (bgblue "emitting pkg-modules") pkg-modules))
          (-emit-non-dune-modules outp ws pkg pkg-modules)))

    (if (truthy? pkg-structs)
        (begin
          (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%"
                                  (bgblue "emitting pkg-structs") pkg-structs))
          (-emit-non-dune-modules outp ws pkg pkg-structs)))

    ;; (if (equal? (car pkg :ocamlc))
    ;;       (-emit-non-dune-modules outp ws pkg pkg-structs)))

    (if pkg-sigs ;;(or pkg-sigs *build-dyads*)
        (begin
          (if (or *debug-emit* *debugging*) (format #t "~A: ~A~%" (bgblue "emitting pkg-sigs") pkg-sigs))
          (-emit-non-dune-signatures outp ws pkg pkg-sigs pkg-modules)))
    ))

(if (or *debug-emit* *debugging*)
    (format #t "loaded non_dune_emit.scm~%"))
