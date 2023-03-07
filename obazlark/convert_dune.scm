(if *debugging*
    (format #t "loading convert-dune.scm~%"))

(load "starlark.scm")

(define arg
  "deps/literals/cwd"
  ;; "rules/with-stdout-to/cat/literal_deps"
  ;; "rules/with-stdout-to/cat/glob"
  ;; "rules/with-stdout-to/run"

  ;; "rules/install/run"
  ;; "rules/install/run/mypgm"
  )

(define (-list-pkgs ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws)))
         )
    (format #t "~A: ~A~%" (yellow "pkg ct") (length pkgs))
    (for-each (lambda (k)
                (format #t "~A: ~A~%" (blue "pkg") k))
              (sort! (hash-table-keys pkgs) string<?))
    pkgs))

(define (-dump-ppx ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (ppx-tbl (car (assoc-val :shared-ppx @ws))))
    (format #t "~A: ~A~%" (bgcyan "shared-ppx") ppx-tbl)))

(define (-dump-opam ws)
  (let* ((@ws (assoc-val ws *mibl-project*))
         (opam (car (assoc-val :opam @ws))))
    (format #t "~A: ~A~%" (red "opam keys") (hash-table-keys opam))
    (for-each (lambda (ws)
                (format #t "~A: ~A~%" (red "opam ws") (car ws))
                (for-each (lambda (item)
                            (format #t "  ~A~%" item))
                          (cdr ws)))
              opam)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-mibl-load-project root-path pkg-path)
  (if *debugging*
      (begin
        (format #t "~A~%" (ublue "-mibl-load-project"))
        (format #t "~A: ~A~%" (blue "root-path") root-path)
        (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)))

  ;; NB: mibl-load-project is implemented in @mibl//src/load_project.c
  ;; and initialized in @mibl//src/config_s7_dune.c
  (let* ((_wss (mibl-load-project root-path pkg-path))
          ;; (if (truthy? root-path) (mibl-load-project root-path)
          ;;          (mibl-load-project)))
         )
    _wss))

(define (-miblize ws)
  (if *debugging*
      (format #t "~A: ~A~%" (blue "-miblize") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws)))
         (mpkg-alist (map (lambda (kv)
                            ;; (format #t "~A: ~A~%" (red "pkg") (cdr kv))
                            (if (assoc 'dune (cdr kv))
                                (let ((mibl-pkg (dune-pkg->mibl :@ (cdr kv))))
                                  ;; (format #t "~A: ~A~%" (red "miblized") mibl-pkg)
                                  (hash-table-set! pkgs (car kv) mibl-pkg)
                                  mibl-pkg)
                                (begin
                                  ;; (format #t "~A: ~A~%" (red "miblize: no dune file") kv)
                                  (cdr kv))
                                ))
                         pkgs)))
        ;; (_ (format #t "~A: ~A~%" (blue "mpkg-alist")
        ;;            mpkg-alist))
        ;; (_ (for-each (lambda (k)
        ;;                (format #t "~A: ~A~%" (blue "pkg") k))
        ;;              (sort! (hash-table-keys pkgs) string<?)))
    (if *debugging*
        (format #t "~A: ~A~%" (blue "mpkg ct") (length mpkg-alist)))
    mpkg-alist))

(define (-emit-mibl ws)
  (if *debugging*
      (format #t "~A: ~A~%" (blue "-emit-mibl") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (if *debugging*
                    (format #t "~A: ~A~%" (blue "-emit-mibl pkg") (cdr kv)))
                (if (not (null? (cdr kv)))
                    (emit-mibl-pkg (cdr kv)))
                )
              pkgs)))

(define (-emit-starlark ws)
  (if *debugging*
      (format #t "~A: ~A~%" (blue "-emit-starlark") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (if *debugging*
                    (format #t "~A: ~A~%" (blue "emitting") (car kv)))
                (if (not (null? (cdr kv)))
                    (mibl-pkg->build-bazel ws (cdr kv))
                    (if *debugging*
                        (format #t "~A: ~A~%" (blue "skipping") (car kv))))
                )
              pkgs)))

;; called by @obazl//convert
(define* (-dune->obazl return root-path pkg-path)
  ;; (set! *debugging* #t)
  (if *debugging*
      (format #t "convert_dune.scm::dune->obazl: ~A, ~A~%" root-path pkg-path))
  ;; (format #t "*mibl-project*: ~A~%" *mibl-project*)
  ;; (format #t "BYE~%"))

  (if *debugging*
      (format #t "~A: ~A~%" (bgred "*emit-bazel-pkg*") *emit-bazel-pkg*))

  (set! *build-dyads* #t)
  (set! *shared-deps* '("compiler/tests-compiler")) ;;  "toplevel/bin"))

  ;; (set! *wrapped-libs-to-ns-archives* #f)
  ;; (set! *unwrapped-libs-to-archives* #f)

  ;; NB: :@ is key of the root workspace in *mibl-project*
  ;; (set! *debugging* #t)

  (-mibl-load-project root-path pkg-path)
  (if *log-parsetree*
      (begin
        (format #t "PARSETREE~%")
        (debug-print-pkgs :@)
        (return)))

  (let* (;;(_wss (-mibl-load-project root-path pkg-path))
         (mpkgs (-miblize :@))
         (mpkgs (add-filegroups-to-pkgs :@))
         (mpkgs (normalize-manifests! :@))
         (mpkgs (normalize-rule-deps! :@))
         )
    ;; start dune-specific
    (miblarkize :@)
    (resolve-pkg-file-deps :@)

    (resolve-labels! :@)

    (handle-shared-ppx :@)

    (if *shared-deps*
        (begin
          (handle-shared-deps :@)
          (handle-shared-opts :@)
          ))

    ;; (ppx-inline-tests! :@)

    (if *log-mibl*
        (begin
          (format #t "~A~%" (bgred "DUMP MIBL"))
          (debug-print-pkgs :@)
          (return)))

    ;; (return)

    ;; end dune-specific?

    (if *emit-mibl*
        (emit-mibl))
        ;; (emit-mibl :@))

    (if *emit-starlark*
        (begin
          ;; (set! *debugging* #t)
          ;;FIXME: rename emit-starlark
          (ws->starlark :@)))

    ;; ;; (ws->opam-bundles :@)

    (if *log-starlark*
      (begin
        (format #t "STARLARK~%")
        (debug-print-pkgs :@)))

    ;; (if *debugging*
    ;;     (format #t "~A: ~A~%" (green "selectors"))
    ;;         (remove-duplicates *select-protases*))

    (if *log-exports*
        (debug-print-exports-table :@))

    ;; (-dump-ppx :@)

    ;; (debug-print-filegroups :@)

    ;; (-dump-opam :@)

    (if (not *mibl-quiet*)
        (format #t "~A: Converted ~A dunefiles.~%" (green "INFO") *dunefile-count*)))
  '())

(define* (dune->obazl root-path pkg-path)
  (call-with-exit (lambda (return)
                    (-dune->obazl return root-path pkg-path))))

(if *debugging*
    (format #t "loaded convert-dune.scm~%"))
