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
(define (starlark-emit-ppx-target outp pkg stanza) ;; fs-path ppx-alist stanza-alist)
  (format #t "~A: ~A\n" (blue "starlark-emit-ppx-target") stanza)

  (let* ((stanza-alist (cdr stanza))
         (privname (assoc-val ':privname stanza-alist))
         (name 'ppx.exe) ;; FIXME
         (libname (string-append
                   (string-upcase
                    (stringify
                     (assoc-val :privname stanza-alist)))))
         (_ (format #t "em libname: ~A~%" libname))
         (ppx-alist (assoc-val :ppx stanza-alist))
         (args (if-let ((args (assoc :args ppx-alist)))
                       (cdr args) #f))
         (manifest (assoc-val :manifest ppx-alist))
         ;; (deps (stanza->ppx-deps-labels fs-path ppx-alist))
         )
    (format #t "ppx manifest: ~A\n" manifest)

      (begin
        (newline outp)
        (format outp "###############\n")
        (format outp "ppx_executable(\n")
        (format outp "    name    = \"~A.ppx\",\n" libname)
        (format outp "    main    = \"//ppx:driver\",\n")

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
              (format outp "    manifest = [\n")
              (format outp "~{        \"~A\"~^,\n~}\n" manifest)
              (format outp "    ],\n")))

        (format outp ")\n")
        ))
  )

(define (ppx-hdr outp)
  (if flag
      (begin
        (format outp "###########################\n")
        (format outp "####  PPX Executables  ####\n")
        (format outp
                "load(\"@obazl_rules_ocaml//ocaml:rules.bzl\", \"ppx_executable\")\n"))))

(define (starlark-emit-ppxes outp pkg) ;; fs-path stanzas)
  (format #t "~A: ~A\n" (blue "starlark-emit-ppxes") pkg)
  ;; (format #t "stanzas: ~A\n" stanzas)
  (for-each (lambda (stanza)
              (format #t "ppx stanza? ~A\n" stanza)
              (case (car stanza)
                ((:archive :library :ns-archive :ns-library)
                 (if-let ((ppx (assoc :ppx (cdr stanza))))
                         (starlark-emit-ppx-target outp pkg stanza))
                )))
            (assoc-val :dune pkg)))

