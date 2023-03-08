(if (or *mibl-debug-executables* *mibl-debugging*)
    (format #t "loading starlark/executables_starlark.scm\n"))

;; (define (starlark-emit-executable-target outp pkg stanza)
;;   (format #t "~A: ~A~%" (blue "starlark-emit-executable-target") stanza))

(define (starlark-emit-executable-target outp ws kind pkg stanza exec-libs)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (begin
        (format #t "~A: ~A~%" (ublue "starlark-emit-executable-target") stanza)
        (format #t "~A: ~A~%" (blue "kind") kind)
        (format #t "~A: ~A~%" (blue "exec-libs") exec-libs)))
  (let* ((stanza-alist (cdr stanza))

         (pkg-name (pkg->pkg-name pkg))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (blue "pkg-name") pkg-name)))

         (privname (assoc-val :privname stanza-alist))
         ;; (mainname (normalize-module-name privname))
         (pubname (if-let ((pubname (assoc-val :pubname stanza-alist)))
                          pubname
                          privname))
         (tgtname (format #f "~A" privname))
         (exename privname)

         ;; (modes (if-let ((ms (assoc-in '(:link :modes) stanza-alist)))
         ;;                (cdr ms) ms))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "MODES: ~A~%" (assoc-val :modes stanza-alist))))
         (modes (if-let ((ms (assoc-val :modes stanza-alist)))
                        ms '()))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (bgred "modes") modes)))
         ;; (_ (if (truthy? modes) (error 'X "X")))

         ;; 'name', i.e. main, is required by dune so we always have it
         ;; (main (cadr (assoc-in '(:link :main) stanza-alist)))
         (main (assoc-val :main stanza-alist))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "main: ~A~%" main)))

         ;; TODO: support *mibl-dune-execlib-includes-main*
         ;; (exec-lib (if *mibl-dune-execlib-includes-main* exec-lib (append exec-lib (list main))))
         (exec-lib (if-let ((el (assoc-val :exec-lib stanza-alist)))
                           el #f))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (bgred "exec-lib") exec-lib)))

        ;; we will use exec-lib-serial and pkg-name to form exec-lib name
         (exec-lib-serial (if exec-lib
                              (if (number? exec-lib)
                                  #f
                                  (if-let ((match (rassoc (list exec-lib) exec-libs)))
                                          (car match)
                                          '(error 'FIXME
                                                  (format #f "~A: ~A~%" (red "exec-lib not found in exec-libs") exec-lib))))
                              #f))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "~A: ~A~%" (bgred "exec-lib serial") exec-lib-serial)))

         (exec-lib-name (if exec-lib-serial
                            (if (< (length exec-libs) 2)
                                (format #f "~A_execlib" main)
                                (format #f "~A_execlib_~A" pkg-name exec-lib-serial))
                            #f))

         (libdeps (if-let ((deps (assoc-in '(:deps :resolved) stanza-alist)))
                          (cdr deps) '()))
         (libdeps (if (number? libdeps)
                      (let ((shared-deps (assoc-in '(:mibl :shared-deps) pkg)))
                        (if (or *mibl-debug-executables* *mibl-debugging*)
                            (format #t "~A: ~A~%" (bggreen "share-deps")
                                    shared-deps))
                        libdeps)
                      libdeps))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "libdeps: ~A~%" libdeps)))

         ;; local module deps
         ;; (deps (assoc-in '(:compile :deps)))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "compile deps: ~A~%" deps)))
         (deps (sort! (if-let ((deps (assoc-in '(:deps :modules) stanza-alist)))
                             ;; (assoc-in '(:link :manifest :modules)
                             ;;           stanza-alist)))
                              (cdr deps) '())
                      sym<?))
         (deps (append deps (list main)))
         (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "local deps: ~A~%" deps)))
         ;; (deps (remove main deps))
         ;; (_ (if (or *mibl-debug-executables* *mibl-debugging*) (format #t "deps: ~A~%" deps)))
         )
    (let-values (((link-std link-opts)
                  (link-flags->mibl stanza)))
      (if (or *mibl-debug-executables* *mibl-debugging*)
          (begin
            (format #t "link opts: ~A~%" link-opts)
            (format #t "link std: ~A~%" link-std)
            (format #t "TARGET: ~A\n" tgtname)
            ;; (format #t "MAIN: ~A\n" main)
            (format #t "DEPS: ~A\n" deps)
            ;; (format #t "SUBMs: ~A\n" submodules)
            ;; (format #t "DEPS: ~A\n" deps)
            ;; (error 'STOP "STOP exec linkflags")
            ))

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
            (if *mibl-js-emit-rules-js*
                (begin
                  (format outp "#############\n")
                  (case kind
                    ((:executable) (format outp "js_binary(\n"))
                    ((:test) (format outp "js_test(\n"))
                    (else (error 'fixme "unexpected kind for executable")))
                  (format outp "    name        = \"~A.bc.js\",\n" tgtname)

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
                  (newline outp))
                ;; else
                (if *mibl-js-emit-rules-swc*
                    (begin)
                    (if *mibl-js-emit-rules-closure*
                        (begin))))

            (if (or *mibl-js-emit-rules-jsoo*
                    *mibl-js-emit-rules-js*
                    *mibl-js-emit-rules-swc*
                    *mibl-js-emit-rules-closure*)
                (begin
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
                  (newline outp)))))

      (begin
        (begin ;; ocaml_binary
          (case kind
            ((:executable) (format outp "ocaml_binary(\n"))
            ((:test) (format outp "ocaml_test(\n"))
            (else (error 'fixme "unexpected kind for executable")))
          (format outp "    name     = \"~A.exe\",\n" tgtname)

          (if (eq? kind :executable)
              (begin
                (if *mibl-dune-execlib-includes-main* ;; default: #f
                    (format outp "    main     = [\":__Lib_~A__\"],\n" tgtname)
                    (format outp "    main     = \":~A\",\n" main) ;;(normalize-module-name tgtname))
                    ;; else
                    )))

          (if (not (null? link-opts))
              (format outp "    opts     = [~{\"~A\"~^, ~}],\n" link-opts))

          ;; (format outp "    deps       = [\":__lib_~A__\"],\n" tgtname)
          (format outp "    visibility = [\"//visibility:public\"],\n")

          (if modes
              (if (equal? '(byte) modes)
                  (format outp "    target_compatible_with = [\"@ocaml//platforms:vm?\"]~%")))

          (format outp ")\n")) ;; end ocaml_binary

        (newline outp)

        ;;FIXME: insert the main exec-module target here
        (if (or *mibl-debug-executables* *mibl-debugging*)
            (format #t "~A: ~A~%" (cyan "finding main module") main))
        (let ((main-module (find-module-in-pkg main pkg)))
          (if (or *mibl-debug-executables* *mibl-debugging*)
              (format #t "~A: ~A~%" (cyan "## main exec module") main-module))
          ;; (format outp "## main: ~A~%" main)
          ;; (format outp "## main exec module: ~A~%" main-module)
          (if main-module
              (begin
                (-emit-module outp ws main-module stanza pkg)
                (newline outp)))
          )
        )
      )
    ;; now emit modules for compilation
    ;; (if pkg-modules (-emit-modules outp pkg pkg-modules))
    ;; (if (or sigs *mibl-build-dyads*)
    ;;     (-emit-signatures outp pkg sigs pkg-modules))
    ;; (starlark-emit-singleton-targets outp pkg)
    ))

(define (starlark-emit-executable-targets outp ws pkg) ;;fs-path stanzas)
  (if (or *mibl-debug-executables* *mibl-debugging*)
      (format #t "~A\n" (ublue "starlark-emit-executable-targets")))

  ;; Multiple executables may share the same execlib. for example we
  ;; may have two (executables), each with n executables and a list
  ;; of (modules). Each (named) executable serves as main
  ;; exec-module; the list of (modules) constitute the execlib. In
  ;; this case we emit two ocaml_ns_library targets, one for each
  ;; execlib. Rather than one per main exec-module.

  ;; Task: iterate the executables to collect the unique execlibs.
  ;; Also: determine name for the execlib, each main exec-module
  ;; needs it (above). Since we may have n execlibs for m main
  ;; exec-modules, we cannot use the main module names. We use the
  ;; directory name of the pkg with a suffixed serial number.

  ;; FIXME: support *mibl-dune-execlib-includes-main*, wherein each each
  ;; executable uses an exec-lib containing the main exec-module,
  ;; which means each gets its own exec-lib, with none shared.

    ;; (format #t "~A~%" (bgcyan "collecting exec-libs"))

    (let* ((stanzas (assoc-val :mibl pkg))
           (pkg-name (pkg->pkg-name pkg))
           (exec-libs (assoc-val :exec-libs stanzas)))
           ;; (exec-libs (fold (lambda (stanza accum)
           ;;                    (if (equal? (car stanza) :executable)
           ;;                        (let ((exec-lib (assoc-val :exec-lib (cdr stanza))))
           ;;                          (if (truthy? exec-lib)
           ;;                              (begin
           ;;                                (format #t "~A: ~A~%" (green "accum") accum)
           ;;                                (let ((match (rassoc exec-lib accum)))
           ;;                                  (if (null? match)
           ;;                                      (let ((serial (if (null? accum) 0 (caar accum))))
           ;;                                        (format #t "~A: ~A ~A~%" (green "new exec-lib") serial exec-lib)
           ;;                                        (acons  (+ 1 serial) exec-lib accum))
           ;;                                      (begin
           ;;                                        (format #t "~A: ~A~%" (green "dup exec-lib") match)
           ;;                                        accum))))
           ;;                              accum))
           ;;                        accum))
           ;;                  '() stanzas))
           ;; (exec-libs (remove '() exec-libs)))
      (if (or *mibl-debug-executables* *mibl-debugging*)
          (format #t "~A: ~A~%" (bgcyan "pkg exec-libs") exec-libs))
      ;; now process :executable stanzas, passing the exec-libs
      (let* ((flag #t))
        (for-each (lambda (stanza)
                    ;; (format #t "stanza x: ~A ~A\n" fs-path (car stanza))
                    (case (car stanza)
                      ((:executable)
                       (if flag
                           (begin
                             (format outp "##############################\n")
                             (format outp "####  Executable Targets  ####\n")
                             (set! flag #f)))
                       (starlark-emit-executable-target outp ws :executable pkg stanza exec-libs))

                      ((:executables)
                       (error 'bad-arg "unexpected :executables stanza")
                       ;; (starlark-emit-executables
                       ;;  outp fs-path stanza)
                       )
                      (else ;; ignore others
                       ;; (format outp "UNCAUGHT: ~A\n" stanza)
                       )))
                  stanzas)

        (if (truthy? exec-libs)
            (begin
              (format outp "############################ Exec Libs ###############################~%")
              (for-each (lambda (exec-lib)
                          (if (or *mibl-debug-executables* *mibl-debugging*)
                              (begin
                                (format #t "~A: ~A~%" (ucyan "emitting exec-libs for pkg") pkg)
                                (format #t "~A: ~A~%" (ucyan "emitting exec-lib") exec-lib)))
                          ;; deps must be namespaced, or at least have unique names

                          (format outp "ocaml_ns_library(~%")
                          ;; (format outp "## exec-lib~%")
                          (format outp "    name = \"~A_execlib\",~%"
                                  (normalize-module-name pkg-name))
                          ;; (format outp "    ns   = \"~A_execlib\",~%" tgtname)

                          ;; (if (not (null? deps))
                          ;;     (begin
                          (format outp "    manifest = [~%")
                          (if (or *mibl-debug-executables* *mibl-debugging*)
                              (format #t "~A: ~A~%" (bgcyan "exec-lib") exec-lib))
                          (format outp "~{        \":~A\"~^,~%~}~%" (assoc-val :modules (cdr exec-lib)))
                          (format outp "    ],\n")
                          ;; ))
                          (format outp "    visibility = [\"//visibility:private\"],~%")
                          (format outp ")")
                          (newline outp)
                          (newline outp))
                        (remove '() exec-libs))))
        )))

(if (or *mibl-debug-executables* *mibl-debugging*)
    (format #t "loaded starlark/executables_starlark.scm\n"))
