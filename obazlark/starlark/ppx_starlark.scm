(define (-get-ppx-id ws stanza-alist)
  (if-let ((ppx (assoc-val :ppx stanza-alist)))
          (begin
            (format #t "~A: ~A~%" (ucyan "ppx") ppx)
            (let* ((ppx-key (assoc-val :manifest (cdr ppx)))
                   (_ (format #t "~A: ~A~%" (green "ppx-key") ppx-key))
                   (ppx-tbl (car (assoc-val :shared-ppx
                                            (assoc-val ws -mibl-ws-table))))
                   (ppx-ct (length (hash-table-keys ppx-tbl))))
              (if-let ((ppx-id (hash-table-ref ppx-tbl ppx-key)))
                      ppx-id
                      (begin
                        (hash-table-set! ppx-tbl ppx-key (+ 1 ppx-ct))
                        (+ 1 ppx-ct)))))
          #f))

(define (ppx-args->string-list ppx-args)
  (map (lambda (arg)
         (if (symbol? arg)
             (symbol->string arg)
             (if (number? arg)
                 (number->string arg)
                 arg)))
       ppx-args))

;; find stanza with ppx whose scope includes module
;; return (ppx-name string . ppx-args list)
(define (module->ppx-alist pkg-path module stanzas)
  (format #t "module->ppx-alist ~A: ~A\n" pkg-path module)
  ;; (format #t "stanzas ct: ~A\n" (length stanzas))
  ;; iterate over stanzas searching for ppx whose scope includes module
  (let recur ((stanzas stanzas))
    (if (null? stanzas)
        #f
        (if (equal :library (caar stanzas))
            (if-let ((ppxes (assoc :ppx (cadr (car stanzas)))))
                    ;; ppxes is list of ppx-alists
                    (begin
                      (format #t "PPXes: ~A\n" ppxes)
                      (let recur2 ((ppxes (cadr ppxes)))
                        (if (null? ppxes)
                            #f
                            (begin
                              (format #t "PPX: ~A\n" (car ppxes))
                              (let* ((ppx-alist (car ppxes))
                                     (scope (cadr (assoc :scope ppx-alist))))
                                (format #t "SCOPE: ~A\n" scope)
                                (if (equal? :all scope)
                                    (car stanzas) ;; ppx-alist
                                    (if (member module scope)
                                        (car stanzas)
                                        (recur2 (cdr ppxes)))))))))
                    ;; else no ppxes
                    #f)
            ;; not a lib, so recur
            (begin
              ;; (format #t "skipping non-lib: ~A\n" (caar stanzas))
              (recur (cdr stanzas)))))))

;; same as stanza-deps->labels, but for :ppx-deps
;; (define (stanza->ppx-deps-labels fs-path ppx-alist)
;;   (let ((deps
;;          (if-let ((deps (assoc :deps ppx-alist)))
;;                  (map (lambda (dep)
;;                           (if-let ((namerec (names-tbl dep)))
;;                                   (cadr (assoc :label namerec))
;;                                   (if-let ((namerec (names-tbl dep)))
;;                                           (cadr (assoc :label namerec))
;;                                           (if-let ((opam (resolve-opam dep)))
;;                                                   opam
;;                                                   ;; no opam
;;                                                   (if-let ((stdlib (resolve-stdlib dep)))
;;                                                           stdlib
;;                                                           (format #t "#UNRESOLVED 4: ~A: ~A\n"
;;                                                                   namerec dep))))))
;;                       (cadr deps))
;;                  '())))
;;     (sort! deps string<?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (starlark-emit-ppx-target outp ppx) ;;pkg stanza)
  (format #t "~A: ~A\n" (blue "starlark-emit-ppx-target") ppx)

  (let* ((ppx-id (car ppx))
         (ppx-alist (cdr ppx))
         (args #f)
         (manifest (assoc-val :manifest ppx-alist))
         )

    ;;;;;;;;;;;;;;;;
    (newline outp)
    (format outp "###############\n")
    (format outp "ppx_executable(\n")
    (format outp "    name    = \"ppx_~A.exe\",\n" ppx-id)

    ;; main    = "@obazl//ppx:Driver",
    (format outp "    main    = \"~A\",\n"
            (if *local-ppx-driver* ":Ppx_driver" "//ppx:Driver"))

    (if args
        (begin
          (format outp "    args    = [\n")
          (for-each (lambda (arg)
                      (format outp "        \"~A\",\n" arg))
                    args)
          (format outp "    ],\n")))

    ;; (if (not (null? deps))
    (if manifest
        (begin
          (format outp "    deps = [\n")
          (format outp "~{        \"~A\"~^,\n~}\n" manifest)
          (format outp "    ],\n")))
    (if (assoc :inline-tests ppx-alist)
        (format outp "    ## @opam_ppx_inline_test//lib/ppx_inline_test~%"))

    (format outp ")\n")

    ;;;;;;;;;;;;;;;;
    (newline outp)
    (format outp "#############\n")
    (format outp "ppx_module(\n")
    (format outp "    name       = \"Ppx_driver\",\n")
    (format outp "    struct     = \":ppx_driver.ml\",\n")
    (format outp "    visibility = [\"//visibility:public\"],\n")
    (format outp "    deps       = [\"@opam_ppxlib//lib/ppxlib\"],\n")
    (format outp ")\n")
    (newline outp)

    (format outp "########\n")
    (format outp "genrule(\n")
    (format outp "    name = \"__ppx_driver__\",\n")
    (format outp "    outs = [\"ppx_driver.ml\"],\n")
    (format outp "    cmd = \"\\n\".join([\n");
    (format outp "        \"echo \\\"(* GENERATED FILE - DO NOT EDIT *)\\\" > \\\"$@\\\"\",\n")
    (format outp "        \"echo \\\"let () = Ppxlib.Driver.standalone ()\\\" >> \\\"$@\\\"\",\n")
    (format outp "    ])\n")
    (format outp ")~%")
    (newline outp)
    ))

;; (define (Xstarlark-emit-ppx-target outp pkg stanza) ;; fs-path ppx-alist stanza-alist)
;;   (format #t "~A: ~A\n" (blue "starlark-emit-ppx-target") stanza)

;;   (if-let ((ppx-tbl (assoc-in '(:dune :shared-ppx) pkg)))
;;           (let* ((ppx-tbl (cadr ppx-tbl))
;;                  (ppx-ct (length ppx-tbl)))
;;             (format #t "~A: ~A~%" (bgmagenta "shared ppx-tbl") ppx-tbl)
;;             (let* ((stanza-alist (cdr stanza))
;;                    (privname (assoc-val ':privname stanza-alist))
;;                    (name 'ppx.exe) ;; FIXME

;;                    (libname (string-append
;;                              (string-upcase
;;                               (stringify
;;                                (assoc-val :privname stanza-alist)))))
;;                    (_ (format #t "em libname: ~A~%" libname))
;;                    (ppx-alist (assoc-val :ppx stanza-alist))
;;                    (args (if-let ((args (assoc :args ppx-alist)))
;;                                  (cdr args) #f))
;;                    (manifest (assoc-val :manifest ppx-alist))
;;                    ;; (deps (stanza->ppx-deps-labels fs-path ppx-alist))
;;                    )
;;               (format #t "ppx manifest: ~A\n" manifest)

;;               (begin
;;                 (newline outp)
;;                 (format outp "###############\n")
;;                 (format outp "ppx_executable(\n")
;;                 (format outp "    name    = \"~A.exe\",\n" libname)
;;                 (format outp "    main    = \"~A\",\n"
;;                         (if *local-ppx-driver* ":Ppx_driver" "//ppx:Driver"))

;;                 (if args
;;                     (begin
;;                       (format outp "    args    = [\n")
;;                       (for-each (lambda (arg)
;;                                   (format outp "        \"~A\",\n" arg))
;;                                 args)
;;                       (format outp "    ],\n")))

;;                 ;; (if (not (null? deps))
;;                 (if manifest
;;                     (begin
;;                       (format outp "    manifest = [\n")
;;                       (format outp "~{        \"~A\"~^,\n~}\n" manifest)
;;                       (format outp "    ],\n")))

;;                 (format outp ")\n")
;;                 )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (starlark-emit-ppx-driver outp ppx)
  (format #t "~A: ~A\n" (blue "starlark-emit-ppx-driver") ppx)

  ;; (let* ((stanza-alist (cdr stanza))
  ;;        (privname (assoc-val ':privname stanza-alist))
  ;;        (name 'ppx.exe) ;; FIXME
  ;;        (libname (string-append
  ;;                  (string-upcase
  ;;                   (stringify
  ;;                    (assoc-val :privname stanza-alist)))))
  ;;        (_ (format #t "em libname: ~A~%" libname))
  ;;        (ppx-alist (assoc-val :ppx stanza-alist))
  ;;        (args (if-let ((args (assoc :args ppx-alist)))
  ;;                      (cdr args) #f))
  ;;        (manifest (assoc-val :manifest ppx-alist))
  ;;        ;; (deps (stanza->ppx-deps-labels fs-path ppx-alist))
  ;;        )
  ;;     (begin
  ;;       (newline outp)
  ;;       (format outp "#############\n")
  ;;       (format outp "ocaml_module(\n")
  ;;       (format outp "    name       = \"Ppx_driver\",\n")
  ;;       (format outp "    struct     = \":ppx_driver.ml\",\n")
  ;;       (format outp "    visibility = [\"//visibility:public\"],\n")
  ;;       (format outp "    deps       = [\"@ppxlib//lib/ppxlib\"],\n")
  ;;       (format outp ")\n")
  ;;       (newline outp)

  ;;       (format outp "########\n")
  ;;       (format outp "genrule(\n")
  ;;       (format outp "    name = \"__ppx_driver__\",\n")
  ;;       (format outp "    outs = [\"ppx_driver.ml\"],\n")
  ;;       (format outp "    cmd = \"\\n\".join([\n");
  ;;       (format outp "        \"echo \\\"(* GENERATED FILE - DO NOT EDIT *)\\\" > \\\"$@\\\"\",\n")
  ;;       (format outp "        \"echo \\\"let () = Ppxlib.Driver.standalone ()\\\" >> \\\"$@\\\"\",\n")
  ;;       (format outp "    ])\n")
  ;;       (format outp ")")
  ;;       (newline outp)
  ;;       ))
  )

;; (define (Xstarlark-emit-ppx-driver outp pkg stanza)
;;   (format #t "~A: ~A\n" (blue "starlark-emit-ppx-driver") stanza)

;;   (let* ((stanza-alist (cdr stanza))
;;          (privname (assoc-val ':privname stanza-alist))
;;          (name 'ppx.exe) ;; FIXME
;;          (libname (string-append
;;                    (string-upcase
;;                     (stringify
;;                      (assoc-val :privname stanza-alist)))))
;;          (_ (format #t "em libname: ~A~%" libname))
;;          (ppx-alist (assoc-val :ppx stanza-alist))
;;          (args (if-let ((args (assoc :args ppx-alist)))
;;                        (cdr args) #f))
;;          (manifest (assoc-val :manifest ppx-alist))
;;          ;; (deps (stanza->ppx-deps-labels fs-path ppx-alist))
;;          )
;;       (begin
;;         (newline outp)
;;         (format outp "#############\n")
;;         (format outp "ocaml_module(\n")
;;         (format outp "    name       = \"Ppx_driver\",\n")
;;         (format outp "    struct     = \":ppx_driver.ml\",\n")
;;         (format outp "    visibility = [\"//visibility:public\"],\n")
;;         (format outp "    deps       = [\"@ppxlib//lib/ppxlib\"],\n")
;;         (format outp ")\n")
;;         (newline outp)

;;         (format outp "########\n")
;;         (format outp "genrule(\n")
;;         (format outp "    name = \"__ppx_driver__\",\n")
;;         (format outp "    outs = [\"ppx_driver.ml\"],\n")
;;         (format outp "    cmd = \"\\n\".join([\n");
;;         (format outp "        \"echo \\\"(* GENERATED FILE - DO NOT EDIT *)\\\" > \\\"$@\\\"\",\n")
;;         (format outp "        \"echo \\\"let () = Ppxlib.Driver.standalone ()\\\" >> \\\"$@\\\"\",\n")
;;         (format outp "    ])\n")
;;         (format outp ")")
;;         (newline outp)
;;         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ppx-hdr outp)
  ;; (if flag
  ;;     (begin
        ;; (format outp "###########################\n")
        ;; (format outp "####  PPX Executables  ####\n")
        (format outp
                "load(\"@rules_ocaml//build:rules.bzl\", \"ppx_executable\", \"ppx_module\")~%"))

;; emit all ppxes in :shared-ppx of pkg
(define (starlark-emit-pkg-ppxes outp ws pkg) ;; fs-path stanzas)
  (format #t "~A: ~A\n" (ublue "starlark-emit-pkg-ppxes") pkg)
  (if-let ((ppx-tbl (assoc-in '(:dune :shared-ppx) pkg)))
          (let* ((ppx-tbl (cadr ppx-tbl))
                 (ppx-ct (length ppx-tbl)))
            (format #t "~A: ~A~%" (blue "shared ppx-tbl") ppx-tbl)
            (for-each (lambda (ppx)
                        (format #t "~A: ~A~%" (bgyellow "emitting ppx") ppx)
                        (starlark-emit-ppx-target outp ppx) ;; pkg stanza)
                        (if *local-ppx-driver*
                            (starlark-emit-ppx-driver outp ppx) ;;pkg stanza)))
                            ))
                        ppx-tbl)

            ;; (for-each (lambda (stanza)
            ;;             (format #t "ppx stanza? ~A\n" stanza)
            ;;             (case (car stanza)
            ;;               ((:archive :library :ns-archive :ns-library)
            ;;                (if-let ((ppx-id (assoc-val :ppx (cdr stanza))))
            ;;                        (begin
            ;;                          (format #t "~A: ~A~%" (ucyan "ppx-id") ppx-id)
            ;;                          (let ((ppx (assoc ppx-id ppx-tbl)))
            ;;                            (format #t "~A: ~A~%" (bgcyan "found ppx") ppx)
            ;;                            (starlark-emit-ppx-target outp pkg stanza)
            ;;                            (if *local-ppx-driver*
            ;;                                (starlark-emit-ppx-driver outp pkg stanza)))
            ;;                          )))))
            ;;           (assoc-val :dune pkg))
            )))

(define (starlark-emit-global-ppxes ws)
  (format #t "~A~%" (bgblue "starlark-emit-global-ppxes"))

  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (ppx-tbl (car (assoc-val :shared-ppx @ws))))
    (format #t "~A: ~A~%" (red "ppx table") ppx-tbl)

    (mkdir-recursive *shared-ppx-pkg* mkdir-permissions)

    ;;FIXME: deal with existing *shared-ppx-pkg*/BUILD.bazel

    (let* ((build-file (format #f "~A/BUILD.bazel" *shared-ppx-pkg*))
           (_ (format #t "~A: ~A~%" (uwhite "ppx build-file") build-file))
           (outp
            (catch #t
                   (lambda ()
                     (open-output-file build-file))
                   (lambda args
                     (format #t "OPEN ERROR")
                     (error 'STOP "open profiles error"))
                   )))

      (format outp "package(default_visibility = [\"//visibility:public\"])")
      (newline outp)
      (newline outp)

      (ppx-hdr outp)
      (for-each (lambda (ppx)
                  (format #t "~A: ~A~%" (bggreen "emitting ppx") ppx)
                  ;; (format outp "~{~A, ~}" (car ppx))
                  ;; (newline outp)
                  ;; (newline outp)

                  (newline outp)
                  (format outp "###############\n")
                  (format outp "ppx_executable(\n")
                  (format outp "    name    = \"ppx_~A.exe\",\n" (cdr ppx))
                  (format outp "    main    = \"Ppx_driver\",~%")
                          ;; (if *local-ppx-driver* ":Ppx_driver" "//ppx:Driver"))

                  ;; (if args
                  ;;     (begin
                  ;;       (format outp "    args    = [\n")
                  ;;       (for-each (lambda (arg)
                  ;;                   (format outp "        \"~A\",\n" arg))
                  ;;                 args)
                  ;;       (format outp "    ],\n")))

                  ;; (if (not (null? deps))
                  ;; (if manifest
                  ;;     (begin
                  (format outp "    deps = [\n")
                  (format outp "~{        \"~A\"~^,\n~}\n" (car ppx))
                  (format outp "    ],\n")

                  (format outp ")")
                  (newline outp)
                  )
                ppx-tbl)

      (newline outp)
      (format outp "#############\n")
      (format outp "ppx_module(\n")
      (format outp "    name       = \"Ppx_driver\",\n")
      (format outp "    struct     = \":ppx_driver.ml\",\n")
      (format outp "    visibility = [\"//visibility:public\"],\n")
      (format outp "    deps       = [\"@opam_ppxlib//lib/ppxlib\"],\n")
      (format outp ")\n")
      (newline outp)

      (format outp "########\n")
      (format outp "genrule(\n")
      (format outp "    name = \"__ppx_driver__\",\n")
      (format outp "    outs = [\"ppx_driver.ml\"],\n")
      (format outp "    cmd = \"\\n\".join([\n");
      (format outp "        \"echo \\\"(* GENERATED FILE - DO NOT EDIT *)\\\" > \\\"$@\\\"\",\n")
      (format outp "        \"echo \\\"let () = Ppxlib.Driver.standalone ()\\\" >> \\\"$@\\\"\",\n")
      (format outp "    ])\n")
      (format outp ")")
      (newline outp)

      (close-output-port outp))))


