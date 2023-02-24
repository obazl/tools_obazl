(format #t "loading convert-dune.scm~%")

(define arg
  "deps/literals/cwd"
  ;; "rules/with-stdout-to/cat/literal_deps"
  ;; "rules/with-stdout-to/cat/glob"
  ;; "rules/with-stdout-to/run"

  ;; "rules/install/run"
  ;; "rules/install/run/mypgm"
  )

(define (-list-pkgs ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws)))
         )
    (format #t "~A: ~A~%" (yellow "pkg ct") (length pkgs))
    (for-each (lambda (k)
                (format #t "~A: ~A~%" (blue "pkg") k))
              (sort! (hash-table-keys pkgs) string<?))
    pkgs))

(define (-dump-ppx ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (ppx-tbl (car (assoc-val :shared-ppx @ws))))
    (format #t "~A: ~A~%" (bgcyan "shared-ppx") ppx-tbl)))

(define (-dump-opam ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (opam (car (assoc-val :opam @ws))))
    (format #t "~A: ~A~%" (red "opam keys") (hash-table-keys opam))
    (for-each (lambda (ws)
                (format #t "~A: ~A~%" (red "opam ws") (car ws))
                (for-each (lambda (item)
                            (format #t "  ~A~%" item))
                          (cdr ws)))
              opam)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-load-dune root-path pkg-path)
  (format #t "~A~%" (ublue "-load-dune"))
  (format #t "~A: ~A~%" (blue "root-path") root-path)
  (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)

  ;; NB: load-dune is implemented in @mibl//src/load_dune.c
  ;; and initialized in @mibl//src/config_s7_dune.c
  (let* ((_wss (load-dune root-path pkg-path))
          ;; (if (truthy? root-path) (load-dune root-path)
          ;;          (load-dune)))
         )
    _wss))

(define (-miblize ws)
  (format #t "~A: ~A~%" (blue "-miblize") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws)))
         (mpkg-alist (map (lambda (kv)
                            ;; (format #t "~A: ~A~%" (red "pkg") (cdr kv))
                            (let ((mibl-pkg (dune-pkg->mibl :@ (cdr kv))))
                            ;; (format #t "~A: ~A~%" (red "miblized") mibl-pkg)
                             (hash-table-set! pkgs (car kv) mibl-pkg)
                             mibl-pkg))
                         pkgs)))
        ;; (_ (format #t "~A: ~A~%" (blue "mpkg-alist")
        ;;            mpkg-alist))
        ;; (_ (for-each (lambda (k)
        ;;                (format #t "~A: ~A~%" (blue "pkg") k))
        ;;              (sort! (hash-table-keys pkgs) string<?)))
    (format #t "~A: ~A~%" (blue "mpkg ct") (length mpkg-alist))
    mpkg-alist))

(define (-emit-mibl ws)
  (format #t "~A: ~A~%" (blue "-emit-mibl") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (blue "-emit-mibl pkg") (cdr kv))
                (if (not (null? (cdr kv)))
                    (emit-mibl-pkg (cdr kv)))
                )
              pkgs)))

(define (-emit-starlark ws)
  (format #t "~A: ~A~%" (blue "-emit-starlark") ws)
  (let* ((@ws (assoc-val ws -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws))))

    (for-each (lambda (kv)
                (format #t "~A: ~A~%" (blue "emitting") (car kv))
                (if (not (null? (cdr kv)))
                    (mibl-pkg->build-bazel ws (cdr kv))
                    (format #t "~A: ~A~%" (blue "skipping") (car kv)))
                )
              pkgs)))

;; called by @obazl//convert
(define* (dune->obazl root-path pkg-path)
  (format #t "convert_dune.scm::dune->obazl: ~A, ~A~%" root-path pkg-path)
  ;; (format #t "-mibl-ws-table: ~A~%" -mibl-ws-table)
  ;; (format #t "BYE~%"))

  (format #t "~A: ~A~%" (bgred "*emit-bazel-pkg*") *emit-bazel-pkg*)

  (set! *build-dyads* #t)
  (set! *shared-deps* '("compiler/tests-compiler")) ;;  "toplevel/bin"))

  ;; (set! *wrapped-libs-to-ns-archives* #f)
  ;; (set! *unwrapped-libs-to-archives* #f)

  ;; NB: :@ is key of the root workspace in -mibl-ws-table

  (let* ((_wss (-load-dune root-path pkg-path))
         (mpkgs (-miblize :@))
         (mpkgs (add-filegroups-to-pkgs :@))
         (mpkgs (normalize-manifests! :@))
         (mpkgs (normalize-rule-deps! :@))
         )

    ;; start dune-specific
    (miblarkize :@)
    (resolve-labels! :@)
    (resolve-pkg-file-deps :@)

    (handle-shared-ppx :@)

    (if *shared-deps*
        (begin
          (handle-shared-deps :@)
          (handle-shared-opts :@)
          ))
    ;; end dune-specific?

    (if *emit-starlark*
        (ws->starlark :@))

    (ws->opam-bundles :@)

    (debug-print-pkgs :@)

    (format #t "~A: ~A~%" (green "selectors")
            (remove-duplicates *select-protases*))

    (debug-print-exports-table :@)

    (-dump-ppx :@)

    (debug-print-filegroups :@)

    ;; (-dump-opam :@)

    )
  '())
