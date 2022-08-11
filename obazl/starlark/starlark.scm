(format #t "loading obazl/starlark.scm\n")

(load "dune.scm")
(load "opam.scm")
(load "string.scm")
(load "starlark/starlark_emit_rules.scm")
;; (load "s7/stuff.scm")
(load "utils.scm")

(define (stanza->alist stanza)
  (if (alist? stanza)
      stanza
      (if (pair? stanza)
          '()
          (error 'bad-type "cannot map stanza to alist"))))

(define (name->opts-sym name)
  (let* ((x (apply string (map (lambda (ch) (if (char=? ch #\.) #\_ ch))
                               (symbol->string name))))
         (nm (undash (string-upcase x))))
    (string-append nm "_OPTS")))
  ;; (string-append (undash (string-upcase (symbol->string name)))
  ;;                "_OPTS"))

(define (name->deps-sym name)
  (let* ((x (apply string (map (lambda (ch) (if (char=? ch #\.) #\_ ch))
                               (symbol->string name))))
         (nm (undash (string-upcase x))))
    (string-append nm "_DEPS")))

(define (starlark-emit-build-file-hdr outp dune-pkg-tbl)

  ;; if write_file, copy_file, etc, emit:
  ;; load("@bazel_skylib//lib:paths.bzl", "write_file") ;; etc.

  (format outp "load(\"@obazl_rules_ocaml//ocaml:rules.bzl\",\n")

  (if (pkg-has-archive? dune-pkg-tbl)
      (format outp "     \"ocaml_archive\",\n"))

  (if (or (assoc-in '(:stanzas :executable) (cdr dune-pkg-tbl))
          (assoc-in '(:stanzas :executables) (cdr dune-pkg-tbl)))
      (format outp "     \"ocaml_executable\",\n"))

  (if (assoc-in '(:stanzas :ocamllex) (cdr dune-pkg-tbl))
      (format outp "     \"ocaml_lex\",\n"))

  (if (pkg-has-library? dune-pkg-tbl)
      (if (pkg-namespaced? dune-pkg-tbl)
          (format outp "     \"ocaml_ns_library\",\n")
          (format outp "     \"ocaml_library\",\n")))

  (if (or (assoc-in '(:stanzas :library) (cdr dune-pkg-tbl))
          (assoc-in '(:stanzas :executable) (cdr dune-pkg-tbl))
          (assoc-in '(:stanzas :executables) (cdr dune-pkg-tbl)))
      (format outp "     \"ocaml_module\",\n"))

  (if (pkg-has-archive? dune-pkg-tbl)
      (if (pkg-namespaced? dune-pkg-tbl)
          (format outp "     \"ocaml_ns_archive\",\n")))

  ;; (if (assoc-in '(:stanzas :signature) (cdr dune-pkg-tbl))
  (if (pkg-has-signature? dune-pkg-tbl)
      (format outp "     \"ocaml_signature\",\n"))

  (format outp ")\n")

  (newline outp)
  ;; (format outp "#############################\n")

  '())

(define (standard-flags)
  '(-O2))

(define (resolve-stdlib module)
  ;; FIXME: drop 'bin:' prefix first
  ;; (format #t "resolve-stdlib: ~A\n" module)
  (let ((std (stdlib-tbl module)))
    ;; (format #t " resolve?: ~A\n" opam)
    std))

(define (match-opener opener dep-sym)
  ;;FIXME: ns-tbl keys are pubnames, opens are module names
  (if-let ((module-alist (ns-tbl opener)))
          ;;FIXME: :modules for exes, :submodules for aggs
          (if-let ((submods (assoc :submodules module-alist)
                              ))
                    (if-let ((hit (hash-table-ref submods dep-sym)))
                            opener
                            #f)
                    #f)
          #f))

(define (resolve-unopened-file-dep ns module-name dep-sym)
  (if-let ((opam (resolve-opam dep-sym)))
          opam
          #f))
          ;; (if-let ((libdep
          ;;           (resolve-libdep ns module-name
          ;;                           dep-sym)))
          ;;         libdep
          ;;         #f)))

;; map file deps (from codept) to bazel labels
;; each dep is list of module (private) name, possibly namespaced
(define (resolve-file-dep ns module-name dep stanzas srcfiles opens)
  ;; (if (equal? ns 'Tezos_crypto)
  ;; (format #t "resolve-file-dep ~A[~A]: ~A\n" ns module-name dep)
  ;; )

  ;; (if (equal? 'Node_storage_command module-name)
  ;;     (format #t "resolve-file-dep: ~A[~A]:: ~A\n" ns module-name dep))
  ;; FIXME: handle compound deps, e,g. (Tezos_client_base Client_context)

  ;; resolution algorithm:
  ;; 0. if dep is namespaced, return the namespace label
  ;; 1. check if dep is in same directory (colon dep)
  ;; 2. search '-open' deps
  ;; 3. search 'libraries' deps
  ;; 4. search opam deps
  ;; 5. search stdlib deps
  ;; if not found, assume its in one of the opam deps

  (if (> (length dep) 1)
      (begin
        ;; (format #t "NAMESPACED: ~A\n" dep)
        (let ((dep-ns (if-let ((pub (assoc :public (names-tbl (car dep)))))
                              (cadr pub) #f)))
          ;; (format #t "dep-ns: ~A\n" dep-ns)
          (if (equal? ns dep-ns)
            (string-append ":" (symbol->string (cadr dep)))
            ;; else assume its covered by one of the aggregate deps
            ;;(cadr (assoc :label (names-tbl (car dep))))
            #f
            ))
          )
      (let ((dep-sym (car dep)))
        ;; (format #t "dep-sym: ~A\n" dep-sym)
        (if-let ((label (in-srcfiles? dep-sym srcfiles)))
                label
                #f
                ;; assume that opam deps are included in agg deps
                ;; (if-let ((opam (resolve-opam dep-sym)))
                ;;         opam
                ;;         #f)
                ))

        ;; (if opens
        ;;       (let recur ((opens (cadr opens)))
        ;;         ;; (if (equal? 'Node_storage_command module-name)
        ;;         ;; (format #t "recurring: ~A\n"
        ;;         ;;         (if (null? opens) "()" (car opens)))
        ;;         ;; )
        ;;         (if (null? opens)
        ;;             (resolve-unopened-file-dep ns module-name dep-sym)
        ;;             ;; else try next opened dep
        ;;             (begin
        ;;               ;; (format #t "checking opened ~A\n" (car opens))
        ;;               (if-let ((m (match-opener (car opens) dep-sym)))
        ;;                       m
        ;;                       (recur (cdr opens)))
        ;;               )))

        ;;       ;; no -open flags - try libdeps, then opam
        ;;       (resolve-unopened-file-dep ns module-name dep-sym)
        ;;       )
        ))

(define (normalize-resolved-deps labels)
    ;; (format #t "normalize-resolved-deps: ~A\n" labels)
    (let* ((sorted
            (sort! labels
                   (lambda (a b)
                     ;; (format #t "sorting a: ~A, b: ~A\n" a b)
                     (let* ((a (cadr a))
                            (a (if (symbol? a) (symbol->string a) a))
                            (b (cadr b))
                            (b (if (symbol? b) (symbol->string b) b)))
                       (if (boolean? a)
                           #f
                           (if (boolean? b)
                               #t
                               (string<? a b)))))))
           ;; (format #t "deduping ~A\n" sorted)
           (deduped (let recur ((items sorted)
                                (result '()))
                      (if (null? items)
                          result
                          (if (null? result)
                              (recur (cdr items) (cons (car items) result))
                              (begin
                                ;; (format #t "car items: ~A, car result:  ~A\n"
                                ;;         (car items) (car result))
                                (if (equal? (cadar items) (cadar result))
                                    ;;FIXME: copy module comment
                                    (recur (cdr items) result)
                                    (recur (cdr items)
                                           (cons (car items) result)))))))))
      ;; for testing, no dedup
      ;; sorted))
      deduped))
  ;; ;; FIXME: avoid multiple sorts
  ;; (sort! deduped
  ;;        (lambda (a b)
  ;;          ;; (format #t "sorting a: ~A, b: ~A\n" a b)
  ;;          (let* ((a (cadr a))
  ;;                 (a (if (symbol? a) (symbol->string a) a))
  ;;                 (b (cadr b))
  ;;                 (b (if (symbol? b) (symbol->string b) b)))
  ;;            (string<? a b))))))

(define (deps->labels deps
                      ns ns-module
                      module-name
                      stanzas
                      srcfiles)
  ;; (if (equal? ns 'tezos-hacl-glue)
  ;; (format #t "deps->labels ~A: ~A\n" ns module-name)
  ;; (format #t "deps: ~A\n" deps)
  ;; )
  ;; deps: list of lists,
  ;; e.g. ((String) (Printf) (Option) (List) (Json) (Ezjsonm))
  ;; (if (equal? module-name 'Api)
  ;; (begin
  ;; (format #t "deps->labels ~A[~A]\n" ns module-name)
  ;;   (format #t " deps: ~A\n" deps))
  ;; )

  ;; for each dep we (may) need to obtain the opens list for the module
  ;; so we can search them for the dep. only need to get the list once

  (let* (;; (opens (module-name->opens-list stanzas module-name))
         (resolved-labels
          (let recur ((deps deps)
                      ;; labels: alist modulename -> label
                      (labels '()))
            ;; (format #t "DEP: ~A\n" (if (null? deps) "()" (car deps)))
            ;; (format #t "LABELS: ~A\n" labels)
            ;; (if (equal? 'Node_storage_command module-name)
            ;;     (format #t "dep->label: ~A\n"
            ;;             (if (null? deps) "()" (car deps))))
            (if (null? deps)
                labels
                (let ((lbl (resolve-file-dep ;; rename: module-dep->label
                            ns module-name
                            (car deps)
                            stanzas
                            srcfiles
                            ;;opens
                            '()
                            )))
                  (if lbl
                      (begin
                        ;; (if (equal? ns 'tezos-hacl-glue)
                        ;;     (begin
                        ;;       (format #t "(car deps): ~A\n" (car deps))
                        ;;       (format #t "LBL: ~A\n" lbl)
                        ;;       (format #t "    labels: ~A\n" labels)))
                        (if (and (equal? ns-module (caar deps))
                                 (equal? module-name (cadr (car deps))))
                            (recur (cdr deps) labels) ;; omit circular dep
                            (recur (cdr deps)
                                   (cons `(,(if (> (length (car deps)) 1)
                                                (cadr (car deps))
                                                (caar deps)) ,lbl)
                                         labels))))
                      (recur (cdr deps) labels)))))))
         ;; (format #t "LABELS: ~A\n" labels)
    ;; (format #t "deps->labels resolved\n")
    (normalize-resolved-deps resolved-labels)))

;; map stanza deps to bazel labels
;; each dep is public name, look it up in names-tbl
(define (stanza-deps->labels fs-path stanza-alist)
  ;; (format #t "stanza-deps->labels: ~A//~A\n" fs-path modname)
  ;; (format #t "    stanza: ~A\n" stanza-alist)
  ;; (if (equal? modname 'tezos-protocol-environment-sigs)
  ;;     (format #t "~A stanza-deps->labels: ~A\n" modname stanza-alist))
  ;; NB: resolved deps use (always?) public_name

  ;; (:deps ((:constant (...)) (:contingent (select clauses...))))
  (let ((deps
         (if-let ((deps (assoc-in '(:deps :constant) stanza-alist)))
                 (begin
                   ;; (format #t "stanza-deps->labels resolved: ~A\n" deps)
                   (map (lambda (dep)
                          ;; (format #t "%%%% dep: ~A %%%%\n" dep)
                          ;; ;; (format #t "priv->pub: ~A => ~A\n"
                          ;; ;;         dep (private-name->public-name dep))
                          ;; (format #t "pub->mod: ~A => ~A\n"
                          ;;         dep (public-name->module-name dep))
                          ;; (format #t "mapping ~A: ~A\n"
                          ;;         dep (public-name->module-name
                          ;;                    (private-name->public-name dep)))

                          ;; lookup: first names tbl, then opam, then stdlib

                          (if-let ((namerec (names-tbl dep)))
                                  ;; (private-name->public-name dep))))
                                  (begin
                                    ;;(format #t "PUBNAME: ~A\n" pubname)
                                    ;; (format #t "NAMEREC: ~A\n" namerec)
                                    (cadr (assoc :label namerec))
                                    )

                                  (if-let ((namerec (names-tbl dep)))
                                                     ;; (private-name->public-name dep))))
                                          (cadr (assoc :label namerec))

                                          (if-let ((opam (resolve-opam dep)))
                                                  opam
                                                  ;; no opam
                                                  (if-let ((stdlib (resolve-stdlib dep)))
                                                          stdlib
                                                          (format #t "#UNRESOLVED 3: ~A: ~A\n"
                                                                  namerec dep))))))
                        (cadr deps)))
                 '())))
    ;; deps))
    (sort! deps string<?)))

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

(define (stanza-opts stanza-alist)
  ;; (format #t "STANZA-OPTS: ~A\n" stanza-alist)
  (let ((flags (if-let ((flags (assoc-in '(:opts :flags) stanza-alist)))
                       flags #f))
        (opens (if-let ((opens (assoc-in '(:opts :opens) stanza-alist)))
                       opens #f)))
    (values flags opens)))

(define (expand-modules-list modules)
  ;; (format #t "expand-modules-list: ~A\n" modules)
  (if-let ((sublist (member :standard modules)))
      (begin
        ;; (format #t "SUBLIST ~A\n" sublist)
        sublist)
      (begin
        (format #t "HUH?\n")
        modules)))

;; returns list
(define (stanza-modules libname stanza-alist)
  ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
  ;; (if (equal? libname 'Tezos_base)
  ;;       (format #t "stanza-submodules ~A: ~A\n" libname stanza-alist))
  (if-let ((modules (assoc :modules stanza-alist)))
          ;; (sort!
          (cadr modules)
          ;;sym<?)
          #f))
;; (let ((modules-direct (if-let (directs (assoc-in
;;                                         '(:modules :direct) stanza-alist))
;;                               (cdr directs) '()))
;;       (modules-indirect (if-let (indirects (assoc-in
;;                                      '(:modules :indirect) stanza-alist))
;;                                 (cdr indirects) '())))
;;   ;; (if (equal? libname 'Tezos_protocol_environment_sigs)
;;   ;; (if (equal? libname 'Tezos_base)
;;       ;; (begin
;;       ;;   (format #t "BASE direct subms: ~A\n" modules-direct)
;;       ;;   (format #t "BASE indirect subms: ~A\n" modules-indirect))
;;   (concatenate modules-direct modules-indirect)))

;; (submodules (stanza-submodules modname stanza-alist)))
(define (stanza-submodules modname stanza-alist)
  ;; (format #t "stanza-submodules: ~A: ~A\n" modname stanza-alist)
  ;; :direct modules: src files
  ;; :indirect modules: generated
  (let ((modules-direct (if-let (directs (assoc-in
                                          '(:submodules :direct) stanza-alist))
                                (cdr directs) '()))
        (modules-indirect (if-let (indirects (assoc-in
                                              '(:submodules :indirect) stanza-alist))
                                  (cdr indirects) '())))
    ;; (if (equal? modname 'Tezos_protocol_environment_sigs)
    ;; (if (equal? modname 'Tezos_base)
    ;; (begin
    ;;   (format #t "BASE direct subms: ~A\n" modules-direct)
    ;;   (format #t "BASE indirect subms: ~A\n" modules-indirect))
    (concatenate modules-direct modules-indirect)))

(define (module->executable-deps _stanzas module)
  ;; (format #t "module->executable-deps: ~A\n" module)
  ;; (if (equal? module 'Registerer)
  ;;     (format #t " Registerer stanzas: ~A\n" _stanzas))
  (let ((result
         (let recur ((stanzas _stanzas))
           ;; (if (not (null? stanzas))
           ;;     (format #t " car stanzas: ~A\n" (car stanzas)))
           (if (null? stanzas)
               #f
               (if (equal? :executable (caar stanzas))
                   (let* ((stanza-alist (cadr (car stanzas)))
                          ;; (_ (format #t "xSTANZA-ALIST: ~A\n" stanza-alist))
                          ;; first get all 'modules', so we can check if ours is included
                          (modules-main
                           (if-let ((ms
                                     (assoc-in
                                      '(:modules :main) stanza-alist)))
                                   (cdr ms) '()))
                          (modules-direct
                           (if-let ((ms
                                     (assoc-in
                                      '(:modules :direct) stanza-alist)))
                                   (cdr ms) '()))
                          (modules-indirect
                           (if-let ((ms
                                     (assoc-in
                                      '(:modules :indirect) stanza-alist)))
                                   (cdr ms) '()))
                          (modules (concatenate
                                    modules-main modules-direct modules-indirect))
                          )
                     ;; (if (equal? module 'Main_byte)
                     ;;     (format #t "Main_byte MODULES: ~A\n" modules))
                     (if modules
                         (begin
                           (if (member module modules)
                               (let* ((constants
                                       (if-let ((ms
                                                 (assoc-in
                                                  '(:deps :constant) stanza-alist)))
                                               (cadr ms) '()))
                                      (contingents
                                       (if-let ((ms
                                                 (assoc-in
                                                  '(:deps :contingents) stanza-alist)))
                                               (cadr ms) '()))
                                      (deps (concatenate constants contingents)))
                                 deps)
                               (recur (cdr stanzas))))
                         ;; our module not in exec's module list
                         ;; recur??
                         #f))
                   ;; not and executable stanza, skip
                   (recur (cdr stanzas)))
               ))))
    result))

(define (explicit-ns? modname pubname)
  (if (equal? modname pubname)
      #f
      (let ((s1 (if (symbol? modname) (symbol->string modname) modname))
            (s2 (if (symbol? pubname) (symbol->string pubname) pubname)))
        (not (and (string=? (substring s1 1) (substring s2 1))
                  (char=? (char-upcase (string-ref s1 0))
                          (char-upcase (string-ref s2 0))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WE SHOULD NOT HAVE 'executables' - all should be normalized to list
;; of 'executable'
;; (define (starlark-emit-executables outp fs-path stanza)
;;   ;; (format #t "starlark-emit-executables ~A\n" stanza)
;;   (error 'unexpected-stanza "ERROR: unexpected 'executables' stanza")

;;   (for-each (lambda (exe)
;;               (format #t "EXE: ~A\n" exe))
;;             (cadr (assoc :names stanza)))

;;   ;; (let* (;; (libname (cdadr (assoc :name stanza)))
;;   ;;       (modname (cadr (assoc-in '(:name :module) stanza)))
;;   ;;       (pubname (if-let ((pn (assoc ':public_name stanza)))
;;   ;;                        (cadr pn) #f))
;;   ;;       (opts (stanza-opts stanza))
;;   ;;       (deps (stanza-deps->labels fs-path pubname stanza))
;;   ;;       (submodules (stanza-submodules modname stanza))
;;   ;;       )

;;   ;;   (format #t "SUBMs: ~A\n" submodules)
;;   ;;   ;; (format outp "~A = [\n" (name->opts-sym modname))
;;   ;;   ;; (for-each (lambda (opt)
;;   ;;   ;;             (format outp "    \"~A\",\n" opt)
;;   ;;   ;;             )
;;   ;;   ;;           opts)
;;   ;;   ;; (format outp "]\n")
;;   ;;   ;; (newline outp)

;;   ;;   ;; (format outp "~A = [\n" (name->deps-sym modname))
;;   ;;   ;; (for-each (lambda (dep)
;;   ;;   ;;             (format outp "    \"~A\",\n" dep)
;;   ;;   ;;           )
;;   ;;   ;;           deps)
;;   ;;   ;; (format outp "]\n")
;;   ;;   ;; (newline outp)

;;   ;;   (begin
;;   ;;     (format outp "#################\n")
;;   ;;     (format outp "ocaml_executable(\n")
;;   ;;     (format outp "    name    = \"~A\",\n" pubname)
;;   ;;     (format outp "    modules = [\n")
;;   ;;     (for-each (lambda (mod)
;;   ;;                 (format outp "        \":~A\",\n"
;;   ;;                         (symbol->string
;;   ;;                          (normalize-module-name mod))
;;   ;;                         ))
;;   ;;               submodules)
;;   ;;     ;; (for-each (lambda (mod)
;;   ;;     ;;             (format outp "        \"~A\",\n" mod))
;;   ;;     ;;           modules)
;;   ;;     (format outp "    ],")
;;   ;;     (format outp ")\n\n")
;;   ;;     ;;(format outp "#############################\n")
;;   ;;     ))
;;   )

(define (starlark-emit-executable-target outp fs-path stanza-alist)
  ;; (begin
    ;; (format #t "starlark-emit-executable-target ~A :: ~A\n"
    ;;         fs-path (cadr (assoc-in '(:name :private) stanza-alist)))
  ;;   (format #t " stanza-alist: ~A\n" stanza-alist))
  (let* ((privname (cadr (assoc-in '(:name :private) stanza-alist)))
         (mainname (normalize-module-name privname))
         (pubname (if-let ((pn (assoc-in '(:name :public) stanza-alist)))
                          (cadr pn)
                          privname))
         ;; (pubname (if-let ((pn (assoc :public_name stanza-alist)))
         ;;                  (cadr pn)
         ;;                  privname))
         (tgtname (string-append (symbol->string pubname) ".exe"))
         (exename privname)
         (deps (stanza-deps->labels fs-path stanza-alist))
         (submodules (assoc :submodules stanza-alist))
         ;; (submodules (stanza-submodules mainname stanza-alist))
         (modules (stanza-modules mainname stanza-alist))
         )

    ;; (begin
    ;; (format #t "TARGET: ~A\n" tgtname)
    ;; (format #t "MAIN: ~A\n" mainname)
    ;; (format #t "MODULES: ~A\n" modules)
    ;;   (format #t "SUBMs: ~A\n" submodules))
    ;; (format #t "DEPS: ~A\n" deps)

    (let-values (((flags opens) (stanza-opts stanza-alist)))
      (if (or flags opens
              (assoc-in '(:opts :raw) stanza-alist))
          (begin
            ;; (format #t "FLAGS: ~A\n" flags)
            ;; (format #t "OPENS: ~A\n" opens)

            (format outp "~A = [\n" (name->opts-sym pubname))
            (if flags
                (for-each (lambda (flag)
                            (format outp "    \"~A\",\n" flag))
                          (cadr flags)))
            (if opens
                (for-each (lambda (open)
                            (format outp "    \"-open\", \"~A\",\n" open))
                          (cadr opens)))

            (if (assoc-in '(:opts :raw :standard) stanza-alist)
                (format outp "    ##FIXME: dune (:standard)\n"))

            (format outp "]\n")
            (newline outp))))

    ;; for executables, 'deps' are libdeps ('libraries' fld), and
    ;; 'modules' are module (src) deps
    ;; (if (not (null? deps))
        (begin
          (format outp "~A = [\n" (name->deps-sym pubname))
          (for-each (lambda (dep)
                      (format outp "    \"~A\",\n" dep)
                      )
                    deps)
          (format outp "]\n")
          (newline outp))
        ;; )

    (begin
      (format outp "#################\n")
      (format outp "ocaml_executable(\n")
      (format outp "    name    = \"~A\",\n" tgtname)
      (format outp "    visibility = [\"//visibility:public\"],\n")
      ;; attr 'exe': string name of outfile excluding extension,
      ;; not a dependency
      (format outp "    exe     = \"~A\",\n" exename)
      (format outp "    main    = \":~A\",\n" mainname)

      (if (not (and (null? deps) (null? modules)))
          (begin
            ;; (format #t "MODDEPS: ~A\n" modules)
            (if (not (null? deps))
                (format outp "    deps = ~A + [\n" (name->deps-sym pubname))
                (format outp "    deps = [\n"))
            (let ((mods (sort! (hash-table-keys
                                (remove-if list
                                           (lambda (entry)
                                             ;; (format #t "ENTRY ~A\n" entry)
                                             (equal? (cdr entry) :main))
                                           modules))
                               sym<?)))
              (for-each (lambda (mod) ;; mod:: (modsym . type)
                          ;; (format #t "mod: ~A\n" mod)
                          ;; (if (not (equal? (cdr mod) :main))
                          (format outp "        \":~A\",\n"
                                  (symbol->string
                                   (normalize-module-name mod))
                                  )
                          ;; )
                          )
                        mods))
            ;; (for-each (lambda (mod)
            ;;             (format outp "        \"~A\",\n" mod))
            ;;           modules)
            (format outp "    ],\n")))

      (format outp ")\n")
      (newline outp)
      ;;(format outp "#############################\n")
      )))

(define (starlark-emit-executable-targets outp fs-path stanzas)
  ;; (format #t "starlark-emit-executable-targets ~A\n" ;; : ~A\n"
  ;;         fs-path)
  ;; (format #t "stanzas: ~A\n" stanzas)
  (let ((flag #t))
    (for-each (lambda (stanza)
                ;; (format #t "stanza x: ~A ~A\n" fs-path (car stanza))
                (case (car stanza)
                  ((:executable)
                   (if flag
                       (begin
                         (format outp "##############################\n")
                         (format outp "####  Executable Targets  ####\n")
                         (set! flag #f)))
                   (starlark-emit-executable-target
                    outp fs-path (cadr stanza)))

                  ((:executables)
                   (error 'bad-arg "unexpected :executables stanza")
                   ;; (starlark-emit-executables
                   ;;  outp fs-path stanza)
                   )
                  (else ;; ignore others
                   ;; (format outp "UNCAUGHT: ~A\n" stanza)
                   )))
              stanzas)))

;; (define (starlark-emit-stanza-deps-and-flags outp typ stanza)
;;   (format #t "starlark-emit-stanza-deps-and-flags\n")
;;   ;; (format #t "    stanza: ~A\n" stanza)
;;   (let ((modname (cadr (assoc-in '(:name :module) stanza)))
;;         (deps (stanza-deps->labels fs-path stanza))
;;         (submodules (stanza-submodules modname typ stanza)))
;;     (let-values (((flags opens) (stanza-opts stanza)))
;;       ;; (format #t "FLAGS: ~A\n" flags)
;;       ;; (format #t "OPENS: ~A\n" opens)
;;       (format outp "~A = [\n" (name->opts-sym pubname))
;;       (if flags
;;           (for-each (lambda (opt)
;;                       (format outp "    \"~A\",\n" opt))
;;                 (cadr flags)))
;;       (if opens
;;           (for-each (lambda (open)
;;                       (format outp "    \"-open\", \"~A\",\n" open))
;;                 (cadr opens)))
;;       ;; (for-each (lambda (opt)
;;       ;;             (format outp "    \"~A\",\n" opt)
;;       ;;             )
;;       ;;           opts)
;;       (format outp "]\n")
;;       (newline outp)

;;       (format outp "~A = [\n" (name->deps-sym pubname))
;;       (for-each (lambda (dep)
;;                   (format outp "    \"~A\",\n" dep))
;;                 deps)
;;       (format outp "]\n")
;;       (newline outp)
;;       )))

(define (library-namespaced? stanza)
  (assoc :namespaced (cadr stanza)))
  ;; (if-let ((wrapped (assoc 'wrapped (cadr stanza))))
  ;;         (if (equal? 'false (cadr wrapped))
  ;;             #f
  ;;             #t)
  ;;         #t))

;; (define (modules-list-contains-module? modules module)
;;   (display (format #f "modules-list-contains-module? ~A ~A"
;;                    modules module)) (newline)
;;   (any (lambda (m)
;;          (member module modules))
;;        modules)
;;   )

(define (stanza-alist->modules stanza-alist)
  (let* ((direct (if-let ((ms (assoc-in '(:submodules :direct) stanza-alist)))
                         (cdr ms) '()))
         (indirect (if-let ((ms (assoc-in '(:modules :direct) stanza-alist)))
                           (cdr ms) '()))
         (main (if-let ((ms (assoc-in '(:modules :main) stanza-alist)))
                       (cdr ms) '())))
    (concatenate main direct indirect)))

;; return aggregator stanza containing module
;; aggregator: library (submodules) or executable (module deps)
(define (module-name->aggregator module _stanzas)
  ;; (format #t "module-name->aggregator: ~A\n" module)
  ;; (if (equal? module 'Client_event_logging_commands)
  ;;     (format #t "stanzas: ~A\n" _stanzas))
  (let ((result
         (let recur ((stanzas _stanzas))
           ;; (if (equal? module 'Client_event_logging_commands)
           ;;     (format #t "Stanza: ~A\n" (car stanzas)))
           ;; (if (not (null? stanzas))
           ;;     (format #t " car stanzas: ~A\n"
           ;;             (aggregate-stanza? (car stanzas))))
           (if (null? stanzas)
               #f
               (if (aggregate-stanza? (car stanzas))
                   (let* ((stanza-alist (cadr (car stanzas)))
                          ;; FIXME: modules must include direct,
                          ;; indirect, and main (for executables)

                          ;; :submodules for aggregators,
                          ;; :modules for executables
                          ;; but only aggregators have ns so ignore execs
                          ;; (modules (stanza-alist->modules stanza-alist))
                          (submodules (if-let ((sms ;; library
                                                (assoc :submodules
                                                       stanza-alist)))
                                              (cadr sms)
                                              (if-let ((sms ;; :executable
                                                        (assoc :modules
                                                               stanza-alist)))
                                                      (cadr sms)
                                                      #f)))
                          )
                     ;; (format #t "submodules: ~A\n" submodules)
                     (if submodules
                         (begin
                           ;;(if (member module submodules)
                           (if (hash-table-ref submodules module)
                               (if-let ((nm (assoc-in '(:name :public) stanza-alist)))
                                       ;; return whole stanza-alist
                                       stanza-alist ;; (cadr nm)
                                       (if-let ((nm (assoc-in '(:name :private) stanza-alist)))
                                               ;; return whole stanza-alist
                                               stanza-alist
                                               ;;(cadr nm)
                                               (error 'bad-arg
                                                      (format #f "module ~A not listed in modules enum ~A\n"
                                                              module modules))))
                               (recur (cdr stanzas))))
                         ;; we could have an ns w/o modules, only resolver
                         (begin
                           (if (equal? :library (car stanza-alist))
                               (if-let ((pubname
                                         (assoc-in
                                          '(:name :pubname) stanza-alist)))
                                       (cadr pubname)
                                       #f)
                               ;; no modules => no aggregation
                               #f))))
                   (recur (cdr stanzas)))))))
    result))

;; find stanza containing module, then return its 'opens' list
(define (module-name->opens-list _stanzas module)
  (format #t "module-name->opens-list ~A\n" module)
  ;; (if (equal? 'Node_storage_command module)
  ;;     (begin
  ;;       (format #t "Node_storage_command stanzas: ~A\n" _stanzas)
  ;;       ;;(error 'stop "stop")
  ;;       ))
  ;; 1. get stanza for module
  ;; 2. get -open flags for module
  (let ((result
         (let recur ((stanzas _stanzas))
           ;; (if (not (null? stanzas))
           ;;     (format #t " car stanzas: ~A\n" (car stanzas)))
           (if (null? stanzas)
               #f
               (if (not (member (caar stanzas) '(:library :executable)))
                   (recur (cdr stanzas)) ;; rule stanzas not yet normalized
                   (let* ((stanza (cadr (car stanzas))))
                     (let ((modules (if-let ((ms (assoc :submodules stanza)))
                                            (cadr ms)
                                            (if-let ((ms (assoc :modules
                                                                stanza)))
                                                    (cadr ms)
                                                    #f))))
                       ;; (if (equal? module 'Node_storage_command)
                       ;; (format #t "MODULES: ~A\n" modules)
                       ;; )
                       (if modules
                           (begin
                             (if (hash-table-ref modules module)
                                 (begin
                                   ;; (format #t "found stanza containing module ~A; returning its :opens list\n" module)
                                   (assoc-in '(:opts :opens) stanza))
                                 (recur (cdr stanzas))))
                           ;; should not happen
                           (if (equal? :library (car stanza))
                               (let ((nm (assoc-in '(:name :module) stanza)))
                                 (cadr nm))
                               ;; recur??
                               #f)))))))))
    result))

;; (define (xxxresolve-libdep ns module-name dep stanzas)
;;   ;; (format #t "resolve-libdep ~A[~A]: ~A\n" ns module-name dep)
;;   ;; e.g. tezos-node[Node_config_file]: Block_hash
;;   ;; search 'libraries' of tezos-node stanza for Block_hash

;;   ;; module-name in ns depends on dep; search ns 'libraries' fld
;;   ;; NB: libraries fld uses public_names
;;   ;; example: Tezos_proxy[Light] depends on RPC_context
;;   ;; (modules-tbl 'Tezos_proxy) => :deps list contains tezos-rpc
;;   ;; (public-names-lookup 'tezos-rpc) => Tezos_rpc
;;   ;; (modules-tbl 'Tezos_rpc) => :submodules assoc list contains RPC_context
;;   ;; result: Light depends on Tezos_rpc
;;   (let* ((modspec (modules-tbl ns)) ;; e.g. Tezos_proxy
;;          (libdeps (assoc :deps modspec))) ;; :deps of Tezos_proxy
;;     ;; (format #t "modspec: ~A\n" modspec)
;;     ;; (format #t "libdeps: ~A\n" libdeps)
;;     ;; (if libdeps
;;     ;;     (format #t "  NS LIBDEPS: ~A:: ~A\n" ns (cadr libdeps)))
;;     (if libdeps
;;         (let recur ((libdeps (cadr libdeps))) ;; libdeps=list of public_names
;;           (if (null? libdeps)
;;               #f
;;               (begin
;;                 ;; (format #t "LIBDEP: ~A\n" (car libdeps))
;;                 (let ((libdep-module (private-name->public-name
;;                                       (car libdeps))))
;;                   ;; e.g. tezos-rpc => Tezos_rpc
;;                   (if-let ((libdep-mtbl (modules-tbl libdep-module)))
;;                           (if-let ((libdep-submods
;;                                     (assoc :submodules libdep-mtbl)))
;;                                   (begin
;;                                     ;; (format #t "lipdep submods: ~A:: ~A\n"
;;                                     ;;         libdep-module libdep-submods)
;;                                     (if (member (car dep) libdep-submods)
;;                                         (begin
;;                                           ;; (format #t "HIT: ~A[~A] :: ~A\n"
;;                                           ;;         ns (car dep) libdep-module)
;;                                           libdep-module)
;;                                         ;; dep not in submods of this libdep
;;                                         (recur (cdr libdeps))))
;;                                   ;; :submodules no found in libdep mtbl
;;                                   (recur (cdr libdeps)))
;;                           ;; FIXME not in modules-tbl - error?
;;                           (recur (cdr libdeps)))))))
;;         ;; ns not in modules-tbl - error?
;;         #f)))

(define (resolve-libdep ns module-name dep)
  ;; (if (equal? 'Block_header dep)
  ;; (format #t "resolve-libdep: ~A[~A]: ~A\n" ns module-name dep)
  ;; )
  ;; e.g. tezos-node[Node_config_file]: Block_hash
  ;; search 'libraries' of tezos-node stanza for Block_hash

  ;; module-name in ns depends on dep; search ns 'libraries' fld
  ;; NB: libraries fld uses public_names
  ;; example: Tezos_proxy[Light] depends on RPC_context
  ;; (modules-tbl 'Tezos_proxy) => :deps list contains tezos-rpc
  ;; (public-names-lookup 'tezos-rpc) => Tezos_rpc
  ;; (modules-tbl 'Tezos_rpc) => :submodules assoc list contains RPC_context
  ;; result: Light depends on Tezos_rpc

  ;;FIXME: convert dep to pubname (ns-tbl keys are pubnames)
  (if-let ((dep-names (names-tbl dep)))
          (let* ((dep-ns (cadr (assoc :public dep-names)))
                 (result
                  (if-let ((modspec (ns-tbl dep-ns)))
                          (begin
                            ;; (format #t "modspec: ~A\n" modspec)
                            (if-let ((label (assoc-in '(:label :ns) modspec)))
                                    (begin
                                      ;; (format #t "ns LABEL: ~A\n" label)
                                      (cadr label))
                                    (if-let ((label (assoc-in
                                                     '(:label :module) modspec)))
                                            (begin
                                              ;; (format #t "module LABEL: ~A\n" label)
                                              (cadr label))
                                            #f)))
                          #f)))
            result)
          #f))

;; FIXME: what if we have a singleton .mli file?
(define (in-srcfiles? module srcfiles)
  ;; (format #t "in-srcfiles? ~A :: ~A\n"
  ;;         module (if (null? srcfiles) "()" (car srcfiles)))
;; FIXME: lookup in modules-tbl instead?

  (let ((modname (normalize-module-name module)))
    (let recur ((srcfiles srcfiles))
      (if (null? srcfiles)
          #f
          (if (equal? modname
                      (file-name->module-name (car srcfiles)))
              (string-append ":"
                             (symbol->string (normalize-module-name module)))
              (in-srcfiles? module (cdr srcfiles)))))))

;; (define (ppx-args->string-list ppx-args)
;;   (map (lambda (arg)
;;          (if (symbol? arg)
;;              (symbol->string arg)
;;              (if (number? arg)
;;                  (number->string arg)
;;                  arg)))
;;        ppx-args))

;; ;; find stanza with ppx whose scope includes module
;; ;; return (ppx-name string . ppx-args list)
;; (define (module->ppx-alist pkg-path module stanzas)
;;   (format #t "module->ppx-alist ~A: ~A\n" pkg-path module)
;;   ;; (format #t "stanzas ct: ~A\n" (length stanzas))
;;   ;; iterate over stanzas searching for ppx whose scope includes module
;;   (let recur ((stanzas stanzas))
;;     (if (null? stanzas)
;;         #f
;;         (if (equal :library (caar stanzas))
;;             (if-let ((ppxes (assoc :ppx (cadr (car stanzas)))))
;;                     ;; ppxes is list of ppx-alists
;;                     (begin
;;                       (format #t "PPXes: ~A\n" ppxes)
;;                       (let recur2 ((ppxes (cadr ppxes)))
;;                         (if (null? ppxes)
;;                             #f
;;                             (begin
;;                               (format #t "PPX: ~A\n" (car ppxes))
;;                               (let* ((ppx-alist (car ppxes))
;;                                      (scope (cadr (assoc :scope ppx-alist))))
;;                                 (format #t "SCOPE: ~A\n" scope)
;;                                 (if (equal? :all scope)
;;                                     (car stanzas) ;; ppx-alist
;;                                     (if (member module scope)
;;                                         (car stanzas)
;;                                         (recur2 (cdr ppxes)))))))))
;;                     ;; else no ppxes
;;                     #f)
;;             ;; not a lib, so recur
;;             (begin
;;               ;; (format #t "skipping non-lib: ~A\n" (caar stanzas))
;;               (recur (cdr stanzas)))))))

(define (starlark-emit-file-targets outp fs-path stanzas dune-pkg)
  ;; (format #t "starlark-emit-file-targets: ~A\n" fs-path)

  ;; we emit targets for both static and generated source files; in
  ;; addition, we may have :indirect submodule deps (example:
  ;; src/lib_protocol_environment/sigs)

  (let* ((srcfiles (if-let ((srcs (assoc-in '(:srcfiles :ocaml :generated)
                                           dune-pkg)))
                       (cadr srcs)
                      '()))
         (srcfiles (if-let ((srcs (assoc-in '(:srcfiles :ocaml :static)
                                            dune-pkg)))
                           (sort! (concatenate (cadr srcs) srcfiles) string<?)
                           srcfiles))
        )
    (if srcfiles
        (begin
          (format outp "#############################\n")
          (format outp "####  Singleton Targets  ####\n")
          (newline outp)))

    (for-each (lambda (srcfile)
                ;; (format #t "SRCFILE: ~A/~A\n" fs-path srcfile)
                ;; (format #t ".") (flush-output-port)
                (let-values (((typ mname)
                              (if (string-suffix? ".ml" srcfile)
                                  (values :ml
                                          ;;(string-drop-right srcfile 3)
                                          (file-name->module-name srcfile))
                                  (if (string-suffix? ".mli" srcfile)
                                      (values :mli
                                              ;; (string-drop-right srcfile 4)
                                              (file-name->module-name srcfile)
                                              )
                                      (error 'bad-filename
                                             (string-append
                                              "extension should be .ml or .mli: "
                                              srcfile))))))
                  (let* ((aggregator-stanza
                          (module-name->aggregator mname stanzas))
                         ;; (_ (format #t "AGG for ~A:~A: ~A\n"
                         ;;            fs-path mname aggregator-stanza))

                         (ppx-alist ;; = ppx alist
                          (module->ppx-alist fs-path mname stanzas))
                         (_ (format #t "PPX-ALIST: ~A\n" ppx-alist))

                         (opts
                          (if aggregator-stanza
                              (if-let ((opts
                                        (assoc ':opts aggregator-stanza)))
                                      (cadr opts)
                                      #f)))

                         (namespace
                          (if aggregator-stanza
                              (if-let ((pub (assoc-in '(:name :public)
                                                            aggregator-stanza)))
                                      (cadr pub) #f)))

                         (ns-module
                          (if aggregator-stanza
                              (normalize-module-name
                               (cadr (assoc-in '(:name :private)
                                              aggregator-stanza)))
                              #f))
                         )
                    (format #t "namespace: ~A\n"
                            namespace)
                    (if (string-suffix? ".ml" srcfile)
                        (begin
                          (format outp "ocaml_module(\n")
                          (format outp "    name     = \"~A\",\n" mname)
                          (format outp "    struct   = \"~A\",\n" srcfile)
                          (let ((mlisrc (string-append
                                         ;; fs-path "/"
                                         srcfile "i")))
                            ;; (format #t "mlisrc: ~A\n" mlisrc)
                            ;; (format #t "srcfiles: ~A\n" srcfiles)
                            (if (member mlisrc srcfiles)
                                (format outp "    sig      = \":~A\",\n"
                                        (string-append
                                         (symbol->string mname) "_cmi"))))

                          (if ppx-alist
                              (begin
                                (format outp
                                        "    ppx      = \":~A\",\n"
                                        (cadr (assoc :name ppx-alist)))
                                (if (not
                                     (equal? :all (cadr (assoc :scope
                                                               ppx-alist))))
                                    (format outp
                                            "    ppx_args = [~{~S, ~}],\n"
                                            (cadr (assoc :args ppx-alist))))))
                          )

                        ;;else
                        (if (string-suffix? ".mli" srcfile)
                              (begin
                                (format outp "ocaml_signature(\n")
                                (format outp "    name     = \"~A_cmi\",\n" mname)
                                (format outp "    src      = \"~A\",\n" srcfile)

                          (format #t "YYYYYYYYYYYYYYYYy\n")
                                (if ppx-alist
                                  (begin
                                    (format outp
                                            "    ppx      = \":~A\",\n"
                                            (cadr (assoc :name ppx-alist)))
                                    (if (not
                                         (equal? :all (cadr (assoc :scope
                                                                   ppx-alist))))
                                        (format outp
                                            "    ppx_args = [~{~S, ~}],\n"
                                            (cadr (assoc :args ppx-alist))))))
                              )
                            (error 'bad-filename
                                   (string-append
                                    "extension should be .ml or .mli: "
                                    srcfile))))

                    ;; now opts and deps
                    ;; (starlark-emit-build-opts outp mname stanzas dune-pkg)
                    (if namespace
                        (if opts
                            (format outp "    opts     = ~A,\n"
                                    (name->opts-sym namespace))))
                        ;; (if-let ((exe-deps
                        ;;           (module->executable-deps stanzas mname)))
                        ;;         (begin
                        ;;           (format outp "    opts   = [\n")
                        ;;           (for-each (lambda (dep)
                        ;;                       (format outp
                        ;;                               "        \"~A\",\n" dep))
                        ;;                     exe-deps)
                        ;;           (format outp "    ],\n"))

                        ;;         ;; else: neither submodule nor exec dep
                        ;;         ;; => no opts?
                        ;;         ))

                    (begin
                      ;; (format #t "    processing deps\n")
                      (let* ((deps (if-let ((deps (filedeps-tbl
                                                   (string-append
                                                    fs-path "/" srcfile))))
                                           (cadr (assoc :deps deps))
                                           #f))
                             (dep-labels (if deps
                                             (deps->labels deps
                                                           namespace
                                                           ns-module
                                                           mname stanzas
                                                           srcfiles)
                                             '()))
                             )
                        (if (equal? mname 'Rand)
                            (begin
                              (format #t "RAND:\n")
                              (format #t "  namespace: ~A\n" namespace)
                              (format #t "  deps: ~A\n" deps)
                              (format #t "  dep-labels: ~A\n" dep-labels)
                              ))
                        (if namespace
                            ;; (if deps ;; (not (null? dep-labels))
                                (format outp "    deps     = ~A + [\n"
                                        (name->deps-sym namespace))
                                (format outp "    deps     = [\n"))
                        ;; )


                        (for-each (lambda (dep-label)
                                    (if (cadr dep-label)
                                        (format outp "       \"~A\",\n"
                                                ;; ## ~A\n"
                                                (cadr dep-label)
                                                ;; (car dep-label)
                                                )
                                        ;; (format outp "       ## ~A\n"
                                        ;;         (car dep-label))
                                        ))
                                  dep-labels))
                      (format outp "    ]\n"))
                    (format outp ")\n\n"))))
              srcfiles)
    ))

    ;; (let ((lib-stanzas (assoc+ :library stanzas)))
    ;;   (if (not (null? lib-stanzas))
    ;;       (for-each (lambda (stanza)
    ;;                   (newline outp)
    ;;                   (format outp "lib stanza: ~A\n"
    ;;                           (assoc 'name (cdr stanza)))
    ;;                   (let ((modules (assoc 'modules (cdr stanza)))
    ;;                         (flags (if-let
    ;;                                 ((flags (assoc 'flags (cdr stanza))))
    ;;                                 (cadr flags)
    ;;                                 '()))
    ;;                         (ocamlopt-flags
    ;;                          (if-let
    ;;                           ((flags (assoc 'ocamlopt_flags (cdr stanza))))
    ;;                           (cadr ocamlopt-flags)
    ;;                           '())))
    ;;                     (format outp "modules: ~A\n" modules)
    ;;                     (format outp "flags: ~A\n" flags)
    ;;                     (format outp "ocamlopt_flags: ~A\n" ocamlopt-flags)
    ;;                     ))
    ;;                 ;;(emit-lib-args fs-path stanza srcfiles out-port))
    ;;                 lib-stanzas))
    ;;   )
    ;; ))

(define (starlark-emit-ocamllex outp stanza)
  (format #t "starlark-emit-ocamllex: ~A\n" stanza)

  (for-each (lambda (ocamllex)
              (let* ((module-name (symbol->string ocamllex))
                     (target-name (string-copy module-name)))
                (string-set! target-name 0
                             (char-upcase (string-ref target-name 0)))
                (format #t "emitting ocamllex: ~A\n" ocamllex)

                ;; ocaml_module target emitted by aggregate emitter
                ;; (format outp "#############\n")
                ;; (format outp "ocaml_module(\n")
                ;; (format outp "    name     = \"~A\",\n" target-name)
                ;; (format outp "    struct   = \"~A\",\n"
                ;;         (string-append module-name ".ml"))
                ;; (format outp ")")
                ;; (newline outp)

                (format outp "##########\n")
                (format outp "ocaml_lex(\n")
                (format outp "    name  = \"lex_~A\",\n" module-name)
                (format outp "    src   = \"~A\",\n"
                        (string-append module-name ".mll"))
                (format outp "    out   = \"~A\",\n"
                        (string-append module-name ".ml"))
                (format outp ")")
                (newline outp)))
            (cadr stanza)))

(define (starlark-emit-file-generators outp fs-path stanzas)
  ;; (format #t "starlark-emit-file-generators")
  (let ((flag #t))
    (for-each (lambda (stanza)
                (case (car stanza)
                  ((:ocamllex)
                   (if flag
                       (begin
                         (format outp "########################\n")
                         (format outp "####  File Generators  ####\n")
                         (newline outp)
                         (set! flag #f)))))

                (case (car stanza)
                  ((:ocamllex)
                   (starlark-emit-ocamllex outp stanza))
                  ((:ocamlyacc)
                   )
                  ((:menhir)
                   )
                  ;; etc.
                  (else
                   ;; skip
                   )))
              stanzas)))

(define (starlark-emit-null-stanzas outp fs-path stanzas)
  (format #t "starlark-emit-null-stanzas: ~A\n" fs-path)
  (format outp "exports_files(glob([\"**\"]))\n"))

;; install targets - ignore
  ;; (if (assoc-in '(:stanzas install) (cdr dune-pkg-tbl))
  ;;     (begin
  ;;       (format outp "## install targets\n")
  ;;       (newline outp)))

(define (starlark-emit-build-files dune-pkg-tbls)
  (format #t "starlark-emit-build-files\n")
  (for-each
   (lambda (dune-pkg-tbl)
     (for-each
      (lambda (pkg-kv)
        ;; (format #t "pkg: ~A\n" (cdr pkg-kv))
        (let* ((fs-path (car pkg-kv))
               (stanzas-assoc (assoc :stanzas (cdr pkg-kv))))
          ;; (format #t "emitting stanzas for: ~A\n" fs-path)
          ;; (display (format #f "stanzas: ~A" stanzas-assoc)) (newline)
          (if (cadr stanzas-assoc)
              (let* ((stanzas (cadr stanzas-assoc))
                     (build-file (string-append fs-path "/BUILD.bazel"))
                     ;; fs-path should already exist
                     (outp
                      (catch #t
                             (lambda ()
                               (open-output-file build-file))
                             (lambda args
                               (format #t "OPEN ERROR"))
                               )))
                ;; (format #t "\nEmitting ~A, port ~A\n" build-file outp)

                (starlark-emit-build-file-hdr outp pkg-kv)
                ;; (newline outp)

                ;; (format #t "emitting executables\n")
                (starlark-emit-executable-targets outp fs-path stanzas)

                (format #t "emitting aggregates\n")
                (starlark-emit-aggregate-targets outp fs-path stanzas)

                ;; (format #t "emitting module files\n")
                (starlark-emit-file-targets outp fs-path stanzas (cdr pkg-kv))

                ;; (format #t "emitting file generators\n")
                ;; ocamllex, ocamlyacc, etc.
                (starlark-emit-file-generators outp fs-path stanzas)

                ;; (format #t "emitting ppxes\n")
                (starlark-emit-ppxes outp fs-path stanzas)

                ;; (format #t "emitting rules\n")
                (starlark-emit-rule-targets outp fs-path stanzas)

                (close-output-port outp)

                ;; (let ((stanzas (cdr (assoc :stanzas (cdr path_pkg))))
                ;;       (srcfiles (if-let ((srcs (assoc :srcfiles (cdr path_pkg))))
                ;;                         (cadr srcs)
                ;;                         '())))
                ;;   )

                ;; (let ((lib-stanzas (filter-stanzas :library stanzas)))
                ;;   (if (not (null? lib-stanzas))
                ;;       (emit-library-args fs-path lib-stanzas srcfiles out-port)))

                ;; (let ((exec-stanzas (filter-stanzas 'executable stanzas)))
                ;;   (if (not (null? exec-stanzas))
                ;;       (begin
                ;;         (emit-executable-args fs-path exec-stanzas srcfiles out-port))))

                ;; (let ((execs-stanzas (filter-stanzas 'executables stanzas)))
                ;;   (if (not (null? execs-stanzas))
                ;;       (emit-executables-args fs-path execs-stanzas srcfiles out-port)
                ;;         ))

                )
              ;; else null :stanzas
              (begin
                (format #t "NULL stanzas: ~A\n" fs-path)
                (let* ((build-file (string-append fs-path "/BUILD.bazel"))
                       (outp
                        (catch #t
                               (lambda ()
                                 (open-output-file build-file))
                               (lambda args
                                 (apply format #t (cadr args)))
                               )))
                  (starlark-emit-null-stanzas outp fs-path pkg-kv)
                  (close-output-port outp)))
              )))
      (cadr dune-pkg-tbl)))
   dune-pkg-tbls)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (starlark-elaborate-pkg-tbls dune-pkg-tbls modules-tbl)
;;   (for-each
;;    (lambda (dune-pkg-tbl)
;;      (for-each
;;       ;; for each pkg:
;;       ;;    for each srcfile:
;;       ;;       get module name for srcfile
;;       ;;       get depslist from dependencies sexp
;;       ;;       for each dep in depslist:
;;       ;;          is dep in an -opened ns?
;;       ;;             for each ns listed in 'flags' fld of aggregator:
;;       ;;                lookup the ns in modules tbl, search its submods
;;       ;;             alternatively: keep list of nss with module in m-tbl
;;       ;;             if :ns use it else use :name
;;       ;;             get path for label
;;       ;;       pull opts from aggregator module in dune-pkg
;;       ;;    for each library/executable stanza:
;;       ;;       for each srcdep ('modules' fld):
;;       ;;       for each libdep ('libraries' fld):

;;       (lambda (pkg-kv)
;;         ;; (format #t "pkg: ~A\n" (cdr pkg-kv))
;;         (let* ((fs-path (car pkg-kv))
;;                (pkg (cdr pkg-kv))
;;                (stanzas-assoc (assoc :stanzas pkg)))
;;           ;; (format #f "fs-path: ~A" fs-path)
;;           ;; (display (format #f "stanzas: ~A" stanzas-assoc)) (newline)
;;           (if stanzas-assoc ;; assoc returns #f if not found
;;               (let* ((stanzas (cadr stanzas-assoc)))

;;                 (starlark-emit-build-file-hdr outp pkg-kv)
;;                 ;; (newline outp)

;;                 (starlark-emit-aggregate-targets outp stanzas) ;; pkg-kv)
;;                 (newline outp)

;;                 ;; (cdr pkg-kv) == pkg

;;                 (starlark-emit-file-targets outp stanzas (cdr pkg-kv))
;;                 (newline outp)

;;                 (starlark-emit-rule-targets outp stanzas) ;; (cdr pkg-kv))
;;                 (newline outp)

;;                 (close-output-port outp)

;;                 ;; (let ((stanzas (cdr (assoc :stanzas (cdr path_pkg))))
;;                 ;;       (srcfiles (if-let ((srcs (assoc :srcfiles (cdr path_pkg))))
;;                 ;;                         (cadr srcs)
;;                 ;;                         '())))
;;                 ;;   )

;;                 ;; (let ((lib-stanzas (filter-stanzas :library stanzas)))
;;                 ;;   (if (not (null? lib-stanzas))
;;                 ;;       (emit-library-args fs-path lib-stanzas srcfiles out-port)))

;;                 ;; (let ((exec-stanzas (filter-stanzas 'executable stanzas)))
;;                 ;;   (if (not (null? exec-stanzas))
;;                 ;;       (begin
;;                 ;;         (emit-executable-args fs-path exec-stanzas srcfiles out-port))))

;;                 ;; (let ((execs-stanzas (filter-stanzas 'executables stanzas)))
;;                 ;;   (if (not (null? execs-stanzas))
;;                 ;;       (emit-executables-args fs-path execs-stanzas srcfiles out-port)
;;                 ;;         ))

;;                 ))))
;;       dune-pkg-tbl))
;;    dune-pkg-tbls)
;;   )
