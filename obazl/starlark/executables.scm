;; (format #t "loading starlark/executables.scm\n")

;; (define (starlark-emit-executable-target outp pkg stanza)
;;   (format #t "~A: ~A~%" (blue "starlark-emit-executable-target") stanza))

(define (starlark-emit-executable-target outp kind pkg stanza)
  (format #t "~A: ~A~%" (ublue "starlark-emit-executable-target") stanza)
  (format #t "~A: ~A~%" (blue "kind") kind)
  (let* ((stanza-alist (cdr stanza))
         (privname (assoc-val :privname stanza-alist))
         ;; (mainname (normalize-module-name privname))
         (pubname (if-let ((pubname (assoc-val :pubname stanza-alist)))
                          pubname
                          privname))
         (tgtname (format #f "~A" privname))
         (exename privname)

         ;; 'name', i.e. main, is required by dune so we always have it
         (main (cadr (assoc-in '(:link :main) stanza-alist)))
         (_ (format #t "main: ~A~%" main))

         ;; (deps (assoc-in '(:compile :deps)))
         ;; (_ (format #t "compile deps: ~A~%" deps))
         (manifest (sort! (if-let ((mani
                             (assoc-in '(:link :manifest :modules)
                                       stanza-alist)))
                                 (cdr mani) '())
                          sym<?))
         (_ (format #t "manifest: ~A~%" manifest))
         (manifest (remove main manifest))
         (_ (format #t "manifest: ~A~%" manifest))
         )
    (let-values (((link-std link-opts)
                  (link-flags->mibl stanza)))
      (format #t "link opts: ~A~%" link-opts)
      (format #t "link std: ~A~%" link-std)

      (format #t "TARGET: ~A\n" tgtname)
      ;; (format #t "MAIN: ~A\n" mainname)
      (format #t "MANIFEST: ~A\n" manifest)
      ;; (format #t "SUBMs: ~A\n" submodules)
      ;; (format #t "DEPS: ~A\n" deps)

      ;; (let-values (((flags opens) (stanza-opts stanza-alist)))
      ;;   (if (or flags opens
      ;;           (assoc-in '(:opts :raw) stanza-alist))
      ;;       (begin
      ;;         ;; (format #t "FLAGS: ~A\n" flags)
      ;;         ;; (format #t "OPENS: ~A\n" opens)

      ;;         (format outp "~A = [\n" (name->opts-sym pubname))
      ;;         (if flags
      ;;             (for-each (lambda (flag)
      ;;                         (format outp "    \"~A\",\n" flag))
      ;;                       (cadr flags)))
      ;;         (if opens
      ;;             (for-each (lambda (open)
      ;;                         (format outp "    \"-open\", \"~A\",\n" open))
      ;;                       (cadr opens)))

      ;;         (if (assoc-in '(:opts :raw :standard) stanza-alist)
      ;;             (format outp "    ##FIXME: dune (:standard)\n"))

      ;;         (format outp "]\n")
      ;;         (newline outp))))

      ;; for executables, 'deps' are libdeps ('libraries' fld), and
      ;; 'modules' are module (src) deps
      ;; (if (not (null? deps))
      ;; (begin
      ;;   (format outp "~A = [\n" (name->deps-sym pubname))
      ;;   (for-each (lambda (dep)
      ;;               (format outp "    \"~A\",\n" dep)
      ;;               )
      ;;             deps)
      ;;   (format outp "]\n")
      ;;   (newline outp))
      ;; ;; )

      (begin
        (format outp "#############\n")
        (case kind
          ((:executable) (format outp "ocaml_binary(\n"))
          ((:test) (format outp "ocaml_test(\n"))
          (else (error 'fixme "unexpected kind for executable")))
        (format outp "    name     = \"~A\",\n" tgtname)

        (if (eq? kind :executable)
            (begin
              ;; attr 'exe': string name of outfile excluding extension,
              ;; not a dependency
              (format outp "    exe      = \"~A\",\n" exename)
              ;; (format outp "    main    = \":~A\",\n" mainname)
              ))

        (if (not (null? link-opts))
            (format outp "    opts     = [~{\"~A\"~^, ~}],\n" link-opts))

        (if main
            (format outp "    main     = \"~A\",\n" main))

        (if (not (null? manifest))
            (format outp "    manifest = [~{\":~A\"~^, ~}],\n" manifest))

            ;; (begin
            ;;   ;; (format #t "MODDEPS: ~A\n" modules)
            ;;   (format outp "    deps = [\n"))
            ;;   (let ((mods (sort! (hash-table-keys
            ;;                       (remove-if list
            ;;                                  (lambda (entry)
            ;;                                    ;; (format #t "ENTRY ~A\n" entry)
            ;;                                    (equal? (cdr entry) :main))
            ;;                                  modules))
            ;;                      sym<?)))
            ;;     (for-each (lambda (mod) ;; mod:: (modsym . type)
            ;;                 ;; (format #t "mod: ~A\n" mod)
            ;;                 ;; (if (not (equal? (cdr mod) :main))
            ;;                 (format outp "        \":~A\",\n"
            ;;                         (symbol->string
            ;;                          (normalize-module-name mod))
            ;;                         )
            ;;                 ;; )
            ;;                 )
            ;;               mods))
            ;;   ;; (for-each (lambda (mod)
            ;;   ;;             (format outp "        \"~A\",\n" mod))
            ;;   ;;           modules)
            ;;   (format outp "    ],\n")))

        (format outp "    visibility = [\"//visibility:public\"],\n")
        (format outp ")\n")
        (newline outp)
        ;;(format outp "#############################\n")
        ))
    ;; now emit modules for compilation
    ;; (if pkg-modules (-emit-modules outp pkg pkg-modules))
    ;; (if (or sigs *build-dyads*)
    ;;     (-emit-signatures outp pkg sigs pkg-modules))
    ;; (starlark-emit-singleton-targets outp pkg)
    ))

(define (starlark-emit-executable-targets outp pkg) ;;fs-path stanzas)
  (format #t "~A\n" (ublue "starlark-emit-executable-targets"))

  (let* ((stanzas (assoc-val :dune pkg))
         (flag #t))
    (for-each (lambda (stanza)
                ;; (format #t "stanza x: ~A ~A\n" fs-path (car stanza))
                (case (car stanza)
                  ((:executable)
                   (if flag
                       (begin
                         (format outp "##############################\n")
                         (format outp "####  Executable Targets  ####\n")
                         (set! flag #f)))
                   (starlark-emit-executable-target outp :executable pkg stanza))

                  ((:executables)
                   (error 'bad-arg "unexpected :executables stanza")
                   ;; (starlark-emit-executables
                   ;;  outp fs-path stanza)
                   )
                  (else ;; ignore others
                   ;; (format outp "UNCAUGHT: ~A\n" stanza)
                   )))
              stanzas)))

;; (format #t "loaded starlark/executables.scm\n")
