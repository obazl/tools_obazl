(if *mibl-debugging*
    (format #t "loading obazl_main.scm~%"))

;; (set! *load-path* (cons ((*libc* 'pwd)) *load-path*))

;; (let ((cwd ((*libc* 'pwd))))
;;   (format #t "CWD: ~A~%" cwd))

;; (format #t "*load-path*: ~A~%" *load-path*)

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
  (if *mibl-debugging*
      (begin
        (format #t "~A~%" (ublue "-mibl-load-project"))
        (format #t "~A: ~A~%" (blue "root-path") root-path)
        (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)))

  ;; NB: mibl-load-project is implemented in @mibl//src/load_project.c
  ;; and initialized in @mibl//src/config_s7.c
  (let* ((_wss (mibl-load-project root-path pkg-path))
          ;; (if (truthy? root-path) (mibl-load-project root-path)
          ;;          (mibl-load-project)))
         )
    _wss))

(define (-miblize ws)
  (if *mibl-debugging*
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
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (blue "mpkg ct") (length mpkg-alist)))
    mpkg-alist))

(define (-emit-mibl ws)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (blue "-emit-mibl") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (blue "-emit-mibl pkg") (cdr kv)))
                (if (not (null? (cdr kv)))
                    (emit-mibl-pkg (cdr kv)))
                )
              pkgs)))

(define (-emit-starlark ws)
  (if *mibl-debugging*
      (format #t "~A: ~A~%" (blue "-emit-starlark") ws))
  (let* ((@ws (assoc-val ws *mibl-project*))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (if *mibl-debugging*
                    (format #t "~A: ~A~%" (blue "emitting") (car kv)))
                (if (not (null? (cdr kv)))
                    (mibl-pkg->build-bazel ws (cdr kv))
                    (if *mibl-debugging*
                        (format #t "~A: ~A~%" (blue "skipping") (car kv))))
                )
              pkgs)))

;; called by @obazl//convert
(define* (-dune->obazl return root-path pkg-path)
  ;; (set! *mibl-debugging* #t)
  (if *mibl-debugging*
      (format #t "convert_dune.scm::dune->obazl: ~A, ~A~%" root-path pkg-path))
  ;; (format #t "*mibl-project*: ~A~%" *mibl-project*)
  ;; (format #t "BYE~%"))

  (if *mibl-debugging*
      (format #t "~A: ~A~%" (bgred "*mibl-emit-bazel-pkg*") *mibl-emit-bazel-pkg*))

  (set! *mibl-build-dyads* #t)
  (set! *mibl-shared-deps* '("compiler/tests-compiler")) ;;  "toplevel/bin"))

  ;; (set! *mibl-wrapped-libs-to-ns-archives* #f)
  ;; (set! *mibl-unwrapped-libs-to-archives\* #f)

  ;; NB: :@ is key of the root workspace in *mibl-project*
  ;; (set! *mibl-debugging* #t)

  (-mibl-load-project root-path pkg-path)
  (if *mibl-show-parsetree*
      (begin
        (format #t "PARSETREE~%")
        (mibl-debug-print-pkgs :@)
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

    (if *mibl-shared-deps*
        (begin
          (handle-shared-deps :@)
          (handle-shared-opts :@)
          ))

    ;; (ppx-inline-tests! :@)

    (if *mibl-show-mibl*
        (begin
          (format #t "~A~%" (bgred "DUMP MIBL"))
          (mibl-debug-print-pkgs :@)
          (return)))

    ;; (return)

    ;; end dune-specific?

    (if *mibl-emit-mibl*
        (emit-mibl))
        ;; (emit-mibl :@))

    (if *mibl-emit-starlark*
        (begin
          ;; (set! *mibl-debugging* #t)
          ;;FIXME: rename emit-starlark
          (ws->starlark :@)))

    ;; ;; (ws->opam-bundles :@)

    (if *mibl-show-starlark*
      (begin
        (format #t "STARLARK~%")
        (mibl-debug-print-pkgs :@)))

    ;; (if *mibl-debugging*
    ;;     (format #t "~A: ~A~%" (green "selectors"))
    ;;         (remove-duplicates *select-protases*))

    (if *mibl-show-exports*
        (mibl-debug-print-exports-table :@))

    ;; (-dump-ppx :@)

    ;; (mibl-debug-print-filegroups :@)

    ;; (-dump-opam :@)

    (if (not *mibl-quiet*)
        (format #t "~A: Converted ~A dunefiles.~%" (green "INFO") *mibl-dunefile-count*)))
  '())

(define* (-main root-path pkg-path)
  (call-with-exit (lambda (return)
                    (-dune->obazl return root-path pkg-path))))

(if *mibl-debugging*
    (format #t "loaded obazl_main.scm~%"))
