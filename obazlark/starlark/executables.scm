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

         ;; (modes (if-let ((ms (assoc-in '(:link :modes) stanza-alist)))
         ;;                (cdr ms) ms))
         (_ (format #t "MODES: ~A~%" (assoc-val :modes stanza-alist)))
         (modes (if-let ((ms (assoc-val :modes stanza-alist)))
                        ms '()))
         (_ (format #t "~A: ~A~%" (bgred "modes") modes))
         ;; (_ (if (truthy? modes) (error 'X "X")))

         ;; 'name', i.e. main, is required by dune so we always have it
         ;; (main (cadr (assoc-in '(:link :main) stanza-alist)))
         (main (assoc-val :main stanza-alist))
         (_ (format #t "main: ~A~%" main))

         (exec-lib (assoc-val :exec-lib stanza-alist))
         (exec-lib (if *dune-execlib-includes-main* exec-lib (append exec-lib (list main))))
         (_ (format #t "exec-lib: ~A~%" exec-lib))

         (libdeps (if-let ((deps (assoc-in '(:deps :resolved) stanza-alist)))
                          (cdr deps) '()))
         (libdeps (if (number? libdeps)
                      (let ((shared-deps (assoc-in '(:dune :shared-deps) pkg)))
                        (format #t "~A: ~A~%" (bggreen "share-deps")
                                shared-deps)
                        libdeps)
                      libdeps))
         (_ (format #t "libdeps: ~A~%" libdeps))

         ;; local module deps
         ;; (deps (assoc-in '(:compile :deps)))
         ;; (_ (format #t "compile deps: ~A~%" deps))
         (deps (sort! (if-let ((deps (assoc-in '(:deps :modules) stanza-alist)))
                             ;; (assoc-in '(:link :manifest :modules)
                             ;;           stanza-alist)))
                              (cdr deps) '())
                      sym<?))
         (deps (append deps (list main)))
         (_ (format #t "local deps: ~A~%" deps))
         ;; (deps (remove main deps))
         ;; (_ (format #t "deps: ~A~%" deps))
         )
    (let-values (((link-std link-opts)
                  (link-flags->mibl stanza)))
      (format #t "link opts: ~A~%" link-opts)
      (format #t "link std: ~A~%" link-std)

      (format #t "TARGET: ~A\n" tgtname)
      ;; (format #t "MAIN: ~A\n" main)
      (format #t "DEPS: ~A\n" deps)
      ;; (format #t "SUBMs: ~A\n" submodules)
      ;; (format #t "DEPS: ~A\n" deps)
      ;; (error 'STOP "STOP exec linkflags")

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

      (if (member 'js modes)
          (begin
            (format outp "#############\n")
            (case kind
              ((:executable) (format outp "js_binary(\n"))
              ((:test) (format outp "js_test(\n"))
              (else (error 'fixme "unexpected kind for executable")))
            (format outp "    name        = \"~A.exe.js\",\n" tgtname)

            ;; (if (eq? kind :executable)
            ;;     ;; do we always want this?
            ;;     (begin
            ;;       ;; attr 'exe': string name of outfile excluding extension,
            ;;       ;; not a dependency
            ;;       (format outp "    exe      = \"~A.exe.js\",\n" exename)
            ;;       ;; (format outp "    main    = \":~A\",\n" mainname)
            ;;       ))

            (if (not (null? link-opts))
                (format outp "    opts        = [~{\"~A\"~^, ~}],\n" link-opts))

            (format outp "    entry_point = \":~A.exe.jsoo\",\n" tgtname)
            (format outp "    visibility  = [\"//visibility:public\"],\n")
            (format outp ")\n")
            (newline outp)

            ;; (format outp "#############\n")
            (case kind
              ((:executable) (format outp "jsoo_binary(\n"))
              ((:test) (format outp "jsoo_test(\n"))
              (else (error 'fixme "unexpected kind for executable")))
            (format outp "    name     = \"~A.exe.jsoo\",\n" tgtname)

            ;; (if (eq? kind :executable)
            ;;     ;; do we always want this?
            ;;     (begin
            ;;       ;; attr 'exe': string name of outfile excluding extension,
            ;;       ;; not a dependency
            ;;       (format outp "    exe      = \"~A.exe.js\",\n" exename)
            ;;       ;; (format outp "    main    = \":~A\",\n" mainname)
            ;;       ))

            (if (not (null? link-opts))
                (format outp "    opts     = [~{\"~A\"~^, ~}],\n" link-opts))

            (format outp "    main   = \":~A.exe\",\n" tgtname)
            (format outp "    visibility = [\"//visibility:public\"],\n")
            (format outp ")\n")
            (newline outp)))

      (begin
        ;; (format outp "#############\n")
        (case kind
          ((:executable) (format outp "ocaml_binary(\n"))
          ((:test) (format outp "ocaml_test(\n"))
          (else (error 'fixme "unexpected kind for executable")))
        (format outp "    name     = \"~A.exe\",\n" tgtname)

        (if (eq? kind :executable)
            (begin
              (if *dune-execlib-includes-main*
                  ;; (if (truthy? exec-lib)
                  ;;     (format outp "    main    = \":~A\",\n"
                  ;;             (normalize-module-name tgtname))
                  ;;     (format outp "    deps    = [\":__lib_~A__\"],\n"
                  ;;             tgtname))
                  (format outp "    main     = \":~A\",\n"
                          (normalize-module-name tgtname))
                  ;; else
                  (format outp "    main     = [\":__Lib_~A__\"],\n" tgtname)
                  )))

        (if (not (null? link-opts))
            (format outp "    opts     = [~{\"~A\"~^, ~}],\n" link-opts))

        ;; (format outp "    deps       = [\":__lib_~A__\"],\n" tgtname)
        (format outp "    visibility = [\"//visibility:public\"],\n")

        (if modes
            (if (equal? '(byte) modes)
                (format outp "    target_compatible_with = [\"@ocaml//host/target:vm?\"]~%")))

        (format outp ")\n")
        (newline outp)

        (format #t "~A: ~A ~A~%" (bggreen "exedeps") tgtname deps)
        ;; (if (equal? (format #f "~A" tgtname) "js_of_ocaml")
        ;;     (error 'x "x"))

        ;; (if (truthy? deps)

        ;;(if *mibl-bin-main*
        ;;
        (if (truthy? exec-lib)
            (begin
              ;; deps must be namespaced, or at least have unique names
              (format outp "ocaml_ns_library(~%")
              (format outp "    name = \"~A_execlib\",~%" tgtname)
              ;; (format outp "    ns   = \"~A_execlib\",~%" tgtname)

              ;; (if (not (null? deps))
              ;;     (begin
              (format outp "    manifest = [~%")
              (format #t "~A: ~A~%" (bgcyan "exec-lib") exec-lib)
              (format outp "~{        \":~A\"~^,~%~}~%" exec-lib)
              (format outp "    ],\n")
              ;; ))
              (format outp "    visibility = [\"//visibility:private\"],~%")
              (format outp ")")
              (newline outp)
              (newline outp)))
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
