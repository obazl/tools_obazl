(define (-module-record->sigfile module)
  (mibl-trace-entry "-module-record->sigfile" module)
  ;; module form: ((:mli . a.mli) (:ml . a.ml))
  ;;          or: ((:mli a.mli DepA) (:ml a.ml DepA))
  ;; where either :mli or :ml may have trailing _
  (if-let ((sig (assoc-val :mli module)))
          (if (list? sig)
              (car sig)
              sig)
          (if-let ((sig (assoc-val :mli_ module)))
                  (if (list? sig)
                      (car sig)
                      sig)
                  (error 'missing-mli "module record missing :mli, :mli_"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-emit-sig outp ws pkg sig stanza)
  (mibl-trace-entry "-emit-sig" sig)
  (mibl-trace "stanza" stanza)
  (let* ((stanza-alist (cdr stanza))
         (module-name (car sig))
         (libname (string-append
                   (string-upcase
                    (stringify
                     (assoc-val :privname (cdr stanza))))))
         (ns (assoc-val :ns (cdr stanza)))

         (opts (if-let ((opts (assoc-val :opts stanza-alist)))
                       opts #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "OPTS") opts)))
         (opts-tag (if opts
                       (if (number? opts) opts
                           libname)
                       #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (ugreen "opts-tag") opts-tag)))

         (deps-tag (if-let ((shared (assoc-in '(:deps :resolved) stanza-alist)))
                           (if (number? (cdr shared)) (cdr shared) libname)
                           libname))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "deps-tag") deps-tag)))

         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (green "module") (car module))))
         (this-is-main #f)
         ;; (this-is-main (if-let ((main (assoc-val :main stanza-alist)))
         ;;                       (begin
         ;;                         (format #t "~A: ~A~%" (green "main") main)
         ;;                         (if (string=? (format #f "~A" main) (format #f "~A" (car module)))
         ;;                             main #f))))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (bggreen "this-is-main") this-is-main)))

         (prologue? #f)
         ;; (prologue? (if-let ((prologue (assoc-val :prologue stanza-alist)))
         ;;                    (let ((m (normalize-module-name pkg-name)))
         ;;                      (if (> pkg-prologue-ct 0)
         ;;                          (format #f "~A_execlib_~A" m prologue)
         ;;                          (format #f "~A_execlib" m)))
         ;;                    #f))

         (agg-deps (if-let ((deps (assoc-val :deps stanza-alist)))
                           (dissoc '(:conditionals :seldeps) deps)
                       ;; else :executable or :test
                       ;; FIXME: should not be any deps in :compile ?
                       (if-let ((deps (assoc-in '(:compile :deps)
                                                stanza-alist)))
                               deps
                               '())))
         (agg-deps (if (number? deps-tag)
                       (dissoc '(:deps :resolved) agg-deps)
                       agg-deps))

         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "Agg-deps") agg-deps)))

         (deps-conditional (assoc-in '(:deps :conditionals) stanza-alist))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "deps-conditional") deps-conditional)))

         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "module") module)))
         (target-selector
          (if deps-conditional
              ;;FIXME: deps-conditional may have more than one entry
              (let ((x
                     (find-then (lambda (conditional)
                                  (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                      (format #t "~A: ~A~%" (bgred "conditional") conditional))
                                  (let ((ctarget (assoc-val :target conditional)))
                                    (if (alist? (cdr sig))
                                        (find-then (lambda (msrc)
                                                     (if (equal? ctarget (cdr msrc))
                                                         conditional #f))
                                                 (cdr sig))
                                        (if (equal? ctarget (cdr sig))
                                            conditional #f))))
                                (cdr deps-conditional))))
                x)
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "target-selector") target-selector)))

         (src-selectors
          (if target-selector
              (flatten
               (map
                (lambda (sel)
                  (cons (car sel)
                        (cadr sel)))
                (assoc-val :selectors target-selector)))
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "src-selectors") src-selectors)))

         (dep-selectors
          (if target-selector
              (map
               (lambda (sel)
                 (let* ((protasis (car sel))
                        (apodosis (last sel)))
                 (list protasis apodosis)))
               (assoc-val :selectors target-selector))
              #f))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "~A: ~A~%" (uwhite "dep-selectors") dep-selectors)))

         (testsuite (if-let ((ts (assoc-val :in-testsuite (cdr stanza))))
                            (string-upcase (format #f "~A" ts)) #f))

         (ppx-pkg (assoc-val :pkg-path pkg))
         (ppx-id (if-let ((ppx (assoc-val :ppx stanza-alist)))
                         ppx #f))

         (shared-ppx (if-let ((shppx (assoc-in '(:mibl :shared-ppx) pkg)))
                             (cadr shppx) #f))

         (ppx-alist (if ppx-id (assoc-val ppx-id shared-ppx) #f))

         ;; (ppx-alist (if-let ((ppx (assoc-val :ppx (cdr stanza))))
         ;;                    ppx #f))
         ;; (module->ppx-alist fs-path mname stanzas))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "ppx-alist: ~A\n" ppx-alist)))

         (ppx-args #f) ;;FIXME

         (ppx-name (if ppx-alist (format #f "~A.ppx" libname)))
         (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "ppx-nme: ~A\n" ppx-name)))
         ;; (ppx-id (get-ppx-id ws (cdr stanza)))
         )
    (let-values (((ml-local-deps mli-local-deps) (module->bazel-local-deps module-name pkg)))
      (mibl-trace "ml-local-deps" ml-local-deps)
      (mibl-trace "mli-local-deps" mli-local-deps)
      ;; (error 'x "X")

      (let* ((modname (car sig))
             (sigfile (if (proper-list? sig)
                          (-module-record->sigfile (cdr sig))
                          (cdr sig))))

        (if (or *mibl-debug-emit* *mibl-debug-s7*)
            (format #t "emitting signature A: ~A\n" modname))

        (format outp "ocaml_signature(\n")
        (format outp "    name          = \"~A_cmi\",\n" modname)
        (if (and ns (not *mibl-ns-topdown*))
            (format outp "    ns_resolver   = \":ns.~A\",\n" ns))
        (format outp "    src           = \"~A\",\n" sigfile)
        (if opts-tag
            (format outp "    opts          = OPTS_~A,\n" opts-tag))

        (if (or *mibl-debug-emit* *mibl-debug-s7*)
            (format #t "~A: ~A~%" (blue "emitting deps D") deps-tag))
        (-emit-deps outp this-is-main prologue? deps-tag
                    ;; stanza
                    agg-deps mli-local-deps dep-selectors testsuite)

        (if ppx-alist
            (begin
              (if (or *mibl-debug-emit* *mibl-debug-s7*)
                  (begin
                    (format #t "~A: ~A~%" (red "stanza") stanza)
                    (format #t "~A: ~A~%" (red "ppx-alist") ppx-alist)
                    (format #t "~A: ~A~%" (red "len ppx-alist") (length ppx-alist))))
              (if (> (length shared-ppx) 1)
                  (begin (error 'stop "SIG")
                         (format outp "    ppx           = \":Cppx_~A.exe\",\n" ppx-id))
                  (format outp "    ppx           = \":ppx.exe\",\n"))
              ;; "    ppx           = \"//~A:ppx_~A.exe\", #X0 \n"
              ;; ppx-pkg ppx-id) ;; ppx-name)
              ;; (cadr (assoc :name ppx-alist)))
              ;; (if ppx-args
              ;;     (format outp
              ;;             "    ppx_args = [~{~S, ~}], #A2 \n" ppx-args))
              (if ppx-args
                  (format outp
                          "    ppx_args      = PPX_ARGS + [~{~S~^, ~}],\n" ppx-args)
                  )
              ;; (if (not
              ;;      (equal? :all (cadr (assoc :scope
              ;;                                ppx-alist))))
              ;;     (format outp
              ;;             "    ppx_args = [~{~S, ~}],\n"
              ;;             (cadr (assoc :args ppx-alist))))
              ))
        (format outp ")~%")
        (newline outp)
        ))))

(define (-emit-sig-freestanding outp ws sig)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (format #t "~A: ~A~%" (blue "-emit-sig-freestanding") sig))
  (let* (;; (libname (string-append
         ;;           (string-upcase
         ;;            (stringify
         ;;             (assoc-val :privname (cdr stanza))))))
         ;; (ns (assoc-val :ns (cdr stanza)))

         ;; (ppx-alist (if-let ((ppx (assoc-val :ppx (cdr stanza))))
         ;;                    ppx #f))
         ;; ;; (module->ppx-alist fs-path mname stanzas))
         ;; (_ (if (or *mibl-debug-emit* *mibl-debug-s7*) (format #t "ppx-alist: ~A\n" ppx-alist)))

         ;; (ppx-name (if ppx-alist (format #f "~A.ppx" libname)))
         )

    (let* ((modname (car sig))
           (sigfile (if (proper-list? sig)
                        (-module-record->sigfile (cdr sig))
                        (cdr sig))))

      (if (or *mibl-debug-emit* *mibl-debug-s7*)
          (format #t "emitting signature B: ~A\n" modname))

      (format outp "ocaml_signature(\n")
      (format outp "    name          = \"~A_cmi\",\n" modname)
      (format outp "    src           = \"~A\",\n" sigfile)
      ;; (format outp "    opts          = ~A_OPTS,\n" libname)
      ;; (format outp "    deps          = ~A_DEPS,\n" libname)

      ;; (if ppx-alist
      ;;     (begin
      ;;       (format outp
      ;;              "    ppx           = \":~A\",\n" ppx-name)
      ;;               ;; (cadr (assoc :name ppx-alist)))
      ;;       (if (not
      ;;            (equal? :all (cadr (assoc :scope
      ;;                                      ppx-alist))))
      ;;           (format outp
      ;;                   "    ppx_args = [~{~S, ~}],\n"
      ;;                   (cadr (assoc :args ppx-alist))))))
      (format outp ")~%")
      (newline outp)
      )))

(define (-emit-sigs-hdr outp ws sigs pkg-modules)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A~%" (ublue "-emit-sigs-hdr") sigs)
        (format #t "~A: ~A~%" (blue "pkg-modules") pkg-modules)
        (format #t "~A: ~A~%" (blue "*mibl-build-dyads*") *mibl-build-dyads*)))
  (if *mibl-build-dyads*
      (if (or (not (null? sigs))
              (find-if (lambda (m)
                         (if (proper-list? m)
                             (or (assoc :mli (cdr m)) (assoc :mli_ (cdr m)))
                             (eq? 0 (fnmatch "*.mli"
                                             (format #f "~A" (cdr m)) 0))))
                       pkg-modules))
          (begin
            (format outp "#############################\n")
            (format outp "####  Signature Targets  ####\n")
            (newline outp)))))
      ;; (if (not (null? sigs))
      ;;     (begin
      ;;       (format outp "#############################\n")
      ;;       (format outp "####  Signature Targets  ####\n")
      ;;       (newline outp)))))

(define (-emit-signatures outp ws pkg sigs pkg-modules)
  (if (or *mibl-debug-emit* *mibl-debug-s7*)
      (begin
        (format #t "~A: ~A\n" (bgblue "-emit-signatures") sigs)
        (format #t "*mibl-build-dyads*: ~A\n" *mibl-build-dyads*)
        (format #t "pkg: ~A\n" pkg)))

  (if (truthy? sigs)
      (-emit-sigs-hdr outp ws sigs pkg-modules))

  ;; (if *mibl-build-dyads*
  ;; (for-each
  ;;  (lambda (module)
  ;;    (format #t "dyad: : ~A\n" module)
  ;;    (if (proper-list? module)
  ;;        (let ((modname (car module))
  ;;              ;; (mli (if-let ((mli (assoc-val :mli (cdr module))))
  ;;              ;;              mli
  ;;              ;;              (if-let ((mli (assoc-val :mli_ (cdr module))))
  ;;              ;;                      mli
  ;;              ;;                      #f)))
  ;;              )
  ;;          (format #t "sig for: ~A\n" modname)
  ;;          (let* ((aggregator
  ;;                  (find-if
  ;;                   (lambda (stanza)
  ;;                     (format #t "checking stanza for msig ~A\n" stanza)
  ;;                     (case (car stanza)
  ;;                       ((:archive :library :ns-archive :ns-library)
  ;;                        ;; (if (eq? :library (car stanza))
  ;;                        (if-let ((submods
  ;;                                  (assoc-in '(:manifest :modules)
  ;;                                             (cdr stanza))))
  ;;                                (begin
  ;;                                  (format #t "submods: ~A\n" submods)
  ;;                                  (if (member modname (cdr submods))
  ;;                                      #t
  ;;                                      #f))))
  ;;                       (else #f)))
  ;;                   (assoc-val :mibl pkg)))
  ;;                 )
  ;;            (if aggregator
  ;;                (-emit-sig outp ws pkg module aggregator)))
  ;;            ;; (format #t "aggregator: ~A\n" aggregator)
  ;;          ;; (-emit-sig outp mli stanza)
  ;;          )
  ;;        ;; else improper list - ignore for *mibl-build-dyads*
  ;;        ))
  ;;  pkg-modules)
  ;; else just free-standing sigs
  (for-each
   (lambda (sig)
     (if (or *mibl-debug-emit* *mibl-debug-s7*)
         (format #t "~A: ~A\n" (uwhite "free-standing sig") sig))
     (let* ((modname (car sig))
            (aggregator (find-if
                         (lambda (stanza)
                           (if (or *mibl-debug-emit* *mibl-debug-s7*)
                               (format #t "~A: ~A\n" (uwhite "checking stanza") stanza))
                           (case (car stanza)
                             ((:archive :library)
                              ;; (if (eq? :library (car stanza))
                              (if-let ((subsigs
                                        (assoc-val :subsigs
                                                   (cdr stanza))))
                                      (begin
                                        (if (or *mibl-debug-emit* *mibl-debug-s7*)
                                            (format #t "~A: ~A\n" (red "subsigs") subsigs))
                                        (if (member modname subsigs)
                                            (-emit-sig outp ws pkg sig stanza)
                                            #f))))
                             (else #f)))
                         (assoc-val :mibl pkg)))
            )
       (if (not aggregator)
           (-emit-sig-freestanding outp ws sig))))
   sigs)
  ;; )
  )
