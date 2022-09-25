(format #t "loading new.scm~%")

(load "ostdlib.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;
(define (-load-dune root-path pkg-path)
  (format #t "~A~%" (ublue "-load-dune"))
  (format #t "~A: ~A~%" (blue "root-path") root-path)
  (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
  (let* ((_wss (load-dune root-path pkg-path))
          ;; (if (truthy? root-path) (load-dune root-path)
          ;;          (load-dune)))
         )
    _wss))


(define (update-local-deps! ws-id)
  (format #t "~A: ~A~%" (ublue "update-local-deps!") ws-id)
  (let* ((@ws (assoc-val ws-id -mibl-ws-table))
         (pkgs (car (assoc-val :pkgs @ws)))
         )
    (format #t "~A: ~A~%" (green "*new-pkg-paths*") *new-pkg-paths*)
    (for-each (lambda (pkg-path)
                (if (find-if (lambda (path)
                               (string-prefix? path pkg-path)) *new-pkg-paths*)
                    (begin
                      (format #t "~A: ~A (t: ~A)~%" (blue "pkg key") pkg-path (type-of pkg-path))
                      (let* ((pkg (hash-table-ref pkgs pkg-path))
                             (pkg-modules (if-let ((elts (assoc-val :modules pkg))) elts '()))
                               (pkg-structs-static (if-let ((structs (assoc-in '(:structures :static) pkg)))
                                                       (cdr structs) '()))
                               (_ (format #t "~A: ~A~%" (blue "pkg-structs-static") pkg-structs-static))
                               (pkg-structs-dynamic (if-let ((structs (assoc-in '(:structures :dynamic) pkg)))
                                                            (cdr structs) '()))
                               (_ (format #t "~A: ~A~%" (blue "pkg-structs-dynamic") pkg-structs-dynamic))
                               (pkg-structs (append (or pkg-structs-static '()) (or pkg-structs-dynamic '())))
                               (_ (format #t "~A: ~A~%" (blue "pkg-structs") pkg-structs))
                               (-all (append pkg-modules pkg-structs))
                               (_ (format #t "~A: ~A~%" (blue "-all") -all))
                               (pkg-module-names (map car -all)))
                          (format #t "~A: ~A~%" (blue "pkg-module-names") pkg-module-names)
                          (for-each (lambda (module)
                                      (format #t "~%  ~A: ~A~%" (ugreen "module") module)
                                      (let* ((ml-static (assoc-val :ml (cdr module)))
                                             (ml-dynamic (assoc-val :ml_ (cdr module)))
                                             (ml (cons (or ml-static '()) (or ml-dynamic '())))
                                             (mli-static (assoc-val :mli (cdr module)))
                                             (mli-dynamic (assoc-val :mli_ (cdr module)))
                                             (mli (cons (or mli-static '()) (or mli-dynamic '()))))
                                        (format #t "~A: ~A~%" (green "    ml") ml)
                                        (format #t "~A: ~A~%" (green "    mli") mli)
                                        ;; struct
                                        (let* ((local-deps (srcfile->local-deps pkg-path (car ml)))
                                               (local-deps (filter (lambda (dep)
                                                                     (member dep pkg-module-names)
                                                                     )
                                                                   local-deps)))
                                          (format #t "~A: ~A~%" (bggreen "local-deps") local-deps)
                                          (if (truthy? local-deps)
                                              (set-cdr! module (append (cdr module) `((:ml-deps ,@local-deps))))))
                                        ;; sig
                                        (let* ((local-deps (srcfile->local-deps pkg-path (car mli)))
                                               (local-deps (filter (lambda (dep)
                                                                     (member dep pkg-module-names)
                                                                     )
                                                                   local-deps)))
                                          (format #t "~A: ~A~%" (bggreen "local-deps") local-deps)
                                          (if (truthy? local-deps)
                                              (set-cdr! module (append (cdr module) `((:mli-deps ,@local-deps))))))
                                        ))
                                    pkg-modules)

                          (for-each (lambda (struct)
                                      (format #t "~%  ~A: ~A~%" (ugreen "struct") struct)
                                      (let* ((ml (cdr struct)))
                                        (format #t "~A: ~A~%" (green "    ml") ml)
                                        (let* ((local-deps (srcfile->local-deps pkg-path ml))
                                               (_ (format #t "~A: ~A~%" (ugreen "struct locals") local-deps))
                                               (local-deps (filter (lambda (dep)
                                                                     (member dep pkg-module-names))
                                                                   local-deps)))
                                          (format #t "~A: ~A~%" (bggreen "local-deps") local-deps)
                                          (if (truthy? local-deps)
                                              (set-cdr! struct (list (cdr struct) `,@local-deps))))))
                                    pkg-structs)
                        ;; (for-each (lambda (sig)
                        ;;             (format #t "~A: ~A~%" (green "  sig") sig)
                        ;;             )
                        ;;           (if-let ((elts (assoc-in '(:signatures :static) pkg))) (cdr elts) '()))
                        ))))
              (sort! (hash-table-keys pkgs) string<?))))

;; called by @obazl//convert
(define* (new-pkgs . pkgs)
  (format #t "new.scm::new-pkgs: ~A~%" pkgs)
  ;; (format #t "-mibl-ws-table: ~A~%" -mibl-ws-table)
  ;; (format #t "BYE~%"))

  (format #t "~A: ~A~%" (green "*new-pkg-paths") *new-pkg-paths*)

  (set! *build-dyads* #f)
  (set! *shared-deps* '("compiler/tests-compiler")) ;;  "toplevel/bin"))

  ;; (set! *wrapped-libs-to-ns-archives* #f)
  ;; (set! *unwrapped-libs-to-archives* #f)

  (stdlib->module-names!)

  (if (truthy? pkgs)
      (for-each (lambda (pkg)
              (let* ((_wss (-load-dune "." pkg)))
                (update-local-deps! :@)
                ))
                pkgs)
      (let* ((_wss (-load-dune "." ".")))
        (update-local-deps! :@)
        ))

  (debug-print-pkgs :@)

         ;; (mpkgs (-miblize :@))
         ;; (mpkgs (add-filegroups-to-pkgs :@))
         ;; (mpkgs (normalize-manifests! :@))
         ;; )

    ;; (resolve-labels! :@)
    ;; (resolve-pkg-file-deps :@)

    ;; (-miblarkize :@)

    ;; (handle-shared-ppx :@)

    ;; (if *shared-deps*
    ;;     (begin
    ;;       (handle-shared-deps :@)
    ;;       (handle-shared-opts :@)
    ;;       ))

    ;; (ws->starlark :@)

    ;; (ws->opam-bundles :@)

    ;; (debug-print-pkgs :@)

    ;; (format #t "~A: ~A~%" (green "selectors")
    ;;         (remove-duplicates *select-protases*))

    ;; (debug-print-exports-table :@)

    ;; (-dump-ppx :@)

    ;; (debug-print-filegroups :@)

    ;; (-dump-opam :@)

    ;; )
  '())
