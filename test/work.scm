(load "dune.scm")
(define pkgs (load-dune ;; "obazl/mibl/test",
             ;; "dune/stanzas/library"
             ;; "dune/tezos/lib_test"
              "d"
             ))
pkgs
(define pkg (hash-table-ref pkgs "test/a"))
pkg

(load "pkg_api.scm")
(define mnames (pkg->module-names pkg))
mnames

(hash-table-keys newtbl)
(define tpkg (hash-table-ref newtbl
                             ;; "src/lib_clic/test"
                             "src/lib_clic/test"
                             ))
tpkg



(let ((rules (cdr (assoc-in '(:dune :rule) pkg))))
  (map car rules))

(define pkg
  (let* ((_   (load "dune.scm"))
       (arg "dune/stanzas/rule/action/chdir")
       (pkgs (load-dune arg))
       (pkg (hash-table-ref pkgs arg))
       (normed-pkg (dune-pkg->mibl pkg))
       )
    normed-pkg))


  ;; (format #t "nzs: ~A\n" nzs))

  ;; modules)
  ;; pkg)


(define xpkg
  '((:ws-path "/Users/gar/obazl/mibl/test")
    (:pkg-path "dune/unit_tests/fields/modules/a")
    (:realpath "/Users/gar/obazl/mibl/test/dune/unit_tests/fields/modules/a")
    (:files "BUILD.bazel")
    (:modules (Client_proto_programs_commands
               (:mli "client_proto_programs_commands.mli")
               (:ml "client_proto_programs_commands.ml"))
              (Client_proto_contracts_commands
               (:ml "client_proto_contracts_commands.ml"))
              (Client_proto_context_commands
               (:ml "client_proto_context_commands.ml"))
              (Alpha_commands_registration
               (:ml "alpha_commands_registration.ml"))
              )
    (:dune-stanzas
     (library
         (name tezos_client_004_Pt24m4xi_commands)
       (public_name tezos-client-004-Pt24m4xi-commands)
       (libraries tezos-base tezos-stdlib-unix tezos-protocol-004-Pt24m4xi tezos-protocol-environment tezos-shell-services tezos-client-004-Pt24m4xi tezos-client-commands tezos-rpc)
       (library_flags (:standard -linkall))
       (modules (:standard (symbol "\\") alpha_commands_registration))
       (flags (:standard -open Tezos_base__TzPervasives -open Tezos_protocol_004_Pt24m4xi -open Tezos_stdlib_unix -open Tezos_shell_services -open Tezos_client_base -open Tezos_client_004_Pt24m4xi -open Tezos_client_commands -open Tezos_rpc))))
    (:dune-project (lang dune 2.0) (formatting (enabled_for ocaml)) (name tezos-client-004-Pt24m4xi-commands))
    ))
xpkg
(load "normalize.scm")
(dune-pkg->mibl xpkg)

(let* ((lib '(library (name mylib_a)
               (public_name mylib)
               (preprocess (pps ppx_jane))
               (libraries core_kernel ppx_version)))
       (lib-alist (cdr lib))
       (name (assoc 'name lib-alist))
       (pubname (assoc 'public_name lib-alist))
       (preproc (assoc 'preprocess lib-alist))
       (libs (assoc 'libraries lib-alist))
       )
  libs)
  preproc)
  pubname)
  lib-alist)

(let ((pkg (
  (define norm (normalize-pkg-tbl newtbl))
  )

(define palist (cdr '("src/lib_clic/unix" (:pkg-path "src/lib_clic/unix") (:dune-stanzas (library (name tezos_clic_unix) (public_name tezos-clic.unix) (flags (:standard -open Tezos_stdlib -open Tezos_clic -open Tezos_error_monad -open Tezos_error_monad.TzLwtreslib)) (libraries tezos-clic tezos-stdlib-unix tezos-error-monad tezos-lwt-result-stdlib))) (:files "BUILD.bazel") (:modules (Scriptable (:mli "scriptable.mli") (:ml "scriptable.ml"))))))

(assoc-val :pkg-path palist)

(cdr e)
norm

;; (load-dune "proto_000_Ps9mPmXa")
;; (load-dune "lib_clic")

(begin
(define opam-srcs ".opam/4.14.0/.opam-switch/sources")
(define pkg-paths
  ;; '("src" "vendors")
  '("src/lib_clic")
  ;; '("src/lib_clic/test")
  ;; '("src/lib_clic/unix")
  ;; '("test/tezos/lib_clic")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define executables-tbl '())
(define stdlib-tbl '())
(define dune-pkg-tbls-alist '())
)

(define opam-tbl (make-hash-table))
(load "dune.scm")
(define dune-pkg-tbls ;; alist: srcdir -> pkg-tbl
               (dune-fold-dirs
                pkg-paths
                ;;(list "src" "vendors")
                ))
dune-pkg-tbls

(define norm (normalize-pkg-tbl dune-pkg-tbls))
norm


(set! dune-pkg-tbls-alist dune-pkg-tbls)

dune-pkg-tbls-alist

;; (length opam-tbl)
;; (hash-table-entries opam-tbl)
;; (opam-tbl 'lwt)
;; (for-each (lambda (kv)
;;             (format #t "~A\n" kv))
;;           opam-tbl)


dune-pkg-tbls

(hash-table-keys
 dune-pkg-tbls-alist)
 (cadr (assoc (car pkg-paths) dune-pkg-tbls-alist)))
(hash-table-keys (cadr (assoc (car pkg-paths) dune-pkg-tbls-alist)))

;; ;; (type-of (cadr (assoc "src" dune-pkg-tbls-alist))) ;; hash-table
;; (length (cadr (assoc "src" dune-pkg-tbls-alist)))  ;; 512
;; (hash-table-entries (cadr (assoc "src" dune-pkg-tbls-alist)))  ;; 209
;; (hash-table-keys (cadr (assoc "src" dune-pkg-tbls-alist)))

;; (length private-name->public-name)
;; (hash-table-entries private-name->public-name)
;; (for-each (lambda (nm)
;;             (format #t "~A\n" nm))
;;           private-name->public-name)
;; (private-name->public-name 'tezos_stdlib)

(define (get-pkg pkgs-alist dir pkg-path)
  (let ((pkg (cadr (assoc dir dune-pkg-tbls-alist))))
    ;;(format #t "~A\n" pkg)))))
    (pkg pkg-path)))

(get-pkg dune-pkg-tbls-alist
         ;; "vendors"
         ;; "vendors/statz/lib_test"
         ;; "src"
         (car pkg-paths)
         ;; "src/proto_genesis/lib_client"

         ;; "src/openapi"
         ;; "src/lib_base"
         ;; "src/proto_004_Pt24m4xi/lib_protocol"
         ;; "src/lib_protocol_environment/sigs"
         ;; "src/lib_protocol_environment/s_packer"
         ;; "src/lib_client_commands"

         ;; one 'executables' stanza with one name:
         ;; "src/lib_clic/test"

         ;; one 'executables', two 'names' and 'public_names',
         ;; libraries and flags, but no modules:
         ;; "src/bin_client"

         ;;;; single 'executable', no flds
         ;; "src/lib_client_base/gen" ;; (executable (name bip39_generator))

         ;;;; two 'executable' stanzas, w/libraries, modules, flags flds
         ;; "src/lib_client_base/test"

         ;;;; multiple executable stanzas, with public_name:
         ;;;; eg (name main_byte) (public_name tezos-protocol-compiler-byte)
         ;;"src/lib_protocol_compiler"
         ;; "src/bin_node"
         ;;"src/openapi"
         ;; "src/lib_client_base"
         ;; "src/lib_client_commands"
         ;; "src/proto_008_PtEdo2Zk/lib_protocol"
         ;; "src/lib_context"
         ;; "src/lib_shell"
         ;; "src/lib_crypto"
         ;; "src/lib_crypto/test"
         ;; "src/lib_protocol_compiler/test"
         ;; "src/lib_stdlib"  ;; ppx
         ;; "src/lib_lwt_result_stdlib"
         ;; "src/proto_alpha/lib_client"  ;; ppx
         ;; "src/lib_hacl_glue"
         ;; "src/bin_client"
         ;; "src/lib_base"
         ;; "src/lib_store/legacy_store"
         ;; "src/bin_attacker"
         ;; "src/lib_protocol_environment/sigs"
         ;; "src/lib_sapling/bindings"
         ;; "src/lib_clic/test"
         ;; "src/proto_004_Pt24m4xi/lib_protocol"

         ;; "opam_srcs/uri/lib_sexp"
         ;; "opam_srcs/tar/mirage"
         "stdlib/templates"
         )

(let* ((pkg (get-pkg dune-pkg-tbls-alist "src"
                     "src/lib_protocol_environment/sigs"
                     ;; "src/proto_004_Pt24m4xi/lib_protocol"
                     ))
       (stanzas (cadr (assoc-in '(:stanzas) pkg))))
  (for-each (lambda (stanza)
              (if (equal? :with-stdout-to (car stanza))
                  (begin
                    (format #t "stanza: ~A\n" stanza)
                    (let ((deps (assoc-in '(:cmd :deps) (cdr stanza))))
                    ;; (let ((deps (assoc :filedeps (cdr stanza))))
                      (for-each (lambda (dep)
                                  (format #t "dep: ~A\n" dep))
                                (cadr deps)))
                    )))
            stanzas)
  '())

;;   (for-each (lambda (f) (format #t "~A\n" f)) (cadr x)))

installation-table

(installation-table
 ;;'libexec:tezos-protocol-compiler:dune_protocol
 ;;'bin:tezos-node:tezos-sandboxed-node.sh
 ;;'lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL
 ;;'libexec:tezos-protocol-compiler:dune_protocol.template
 ;; 'libexec:tezos-protocol-compiler:replace
 ;;'libexec:tezos-protocol-compiler:final_protocol_versions
 ;; 'libexec:tezos-protocol-environment-packer:s_packer
 ;; 'bin:tezos-protocol-compiler.tezos-protocol-packer
 ;; 'bin:tezos-embedded-protocol-packer
 ;; 'lib:tezos-tooling:lint.sh
 ;; 'exe:test_clic.exe
 ;;'exe:../config/discover.exe ;; vendors/ocaml-lmdb/src/dune
 ;; 'libexec:tezos-protocol-compiler:dune_protocol.template
 ;; 'libexec:tezos-protocol-compiler:replace
 'lib:tezos-protocol-demo-noops:raw/TEZOS_PROTOCOL
 )

;; (for-each (lambda (kv)
;;             (format #t "~A => ~A\n"
;;                     (car kv) (cdr kv))
;;             )
;;           installation-table)

;; (assoc-in '(:stanzas :library :opts :raw :standard)
;;           (get-pkg dune-pkg-tbls-alist "src" "src/lib_stdlib"))

;; (hash-table-entries stdlib-tbl)
;; (length stdlib-tbl)
;; (stdlib-tbl 'ocp-ocamlres)
;; (stdlib-tbl 'bin:ocp-ocamlres)
(stdlib-tbl ':ocamlc)
(stdlib-tbl ':camlinternalFormatBasics.cmi)

;; (for-each (lambda (e)
;;             (format #t "~A\n" e))
;;           stdlib-tbl)

(begin
  (load "dune.scm")
  (let ((x (define names-tbl
             (libdeps->names-ht dune-pkg-tbls-alist))))
    '()))

;;names-tbl
(length names-tbl)
;; (hash-table-entries names-tbl)
;;(cadr (assoc :public (names-tbl 'tezos_stdlib)))

;;(names-tbl 'libexec:tezos-protocol-environment-packer:s_packer)

;; (for-each (lambda (nm)
;;             (format #t "~A\n" nm))
;;           names-tbl)

(begin
  (load "alist.scm")
  (load "codept.scm")
  (load "dune.scm")
  (load "lookup_tables.scm")
  ;; (codept->local-modules codept-sexp)
  (let ((x (define ns-tbl ;; values: full description
             (dune->ns-tbl dune-pkg-tbls-alist) ;; codept-sexp)
             ;;(codept->modules-table codept-sexp)
             )))
    '()))
ns-tbl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((opam-dirs (directory->list opam-srcs)))
  (for-each (lambda (dir)
              (cond
                ((equal? dir "."))
                ((equal? dir ".."))
                (else
                 (let* ((bn-dot (string-index dir (lambda (ch) (char=? ch #\.))))
                        (bn (string-take dir bn-dot))
                        ;; (_ (format #t "~A\n" bn))
                        (cmd
                         (string-append "ln -s ../" opam-srcs "/" dir
                                        " opam_srcs/" bn)))
                   (format #t "~A\n" cmd)
                   (system cmd)
                   ))))
            opam-dirs))

;; used once in starlark.scm:stanza-deps
;; (define private-name->public-name (make-hash-table))

;; map dune executable names to bazel labels
;; constructed but not used?
;; (define public-exe->label (make-hash-table))

;; map 'install' stanza stuff to bazel labels
;; (define installation-table (make-hash-table))

;; stdlib-tbl does not depend on dunfiles or srcfiles
(begin
  (load "codept.scm")
  (let-values (((stdlib-tbl executables-tbl) (make-stdlib-tbl)))
    (format #t "len execs: ~A\n" (length executables-tbl))
    (format #t "len stdlib: ~A\n" (length stdlib-tbl))
    '()))

(hash-table-entries stdlib-tbl)
(hash-table-keys stdlib-tbl)
(for-each (lambda (k)
            (format #t "~A\n" k))
          stdlib-tbl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (length ns-tbl)
;; (hash-table-entries ns-tbl)
;; (ns-tbl 'tezos-crypto)
;; (ns-tbl 'libexec:tezos-protocol-environment-packer:s_packer)
;; (names-tbl 'tezos-crypto)
;; (names-tbl 'tezos_baking_008_PtEdo2Zk)
;; (names-tbl 'Tezos_baking_008_PtEdo2Zk)
;; (names-tbl 'tezos-baking-008-PtEdo2Zk)
;; (names-tbl 'Tezos_stdlib_unix)

(begin
  (load "dune.scm")
  (load "codept.scm")
  (let ((outp (begin  ;; (outp (open-output-string))
                (if (not (file-exists? ".obazl.d/tmp"))
                    (system "mkdir -p .obazl.d/tmp"))
                (open-output-file ".obazl.d/tmp/codept.args"))))
    (dune-emit-codept-args dune-pkg-tbls-alist outp)
    (close-output-port outp))
  '())

;; (for-each (lambda (f)
;;             (format #t "~A\n" f))
;;           emitted-files)
;; (emitted-files "src/openapi/openapi.mli")
;; (emitted-files "src/lib_crypto/test/test_blake2b.ml")

(begin
  (load "codept.scm")
  (system "codept -expand-deps -sexp -k -args .obazl.d/tmp/codept.args > .obazl.d/tmp/codept.deps 2> .obazl.d/tmp/codept.log")
  99)

(begin
  (load "codept.scm")
  (let ((x (define codept-sexp
           (read-codept-depsfile ".obazl.d/tmp/codept.deps"))))
  '()))

;; (length (codept->file-deps codept-sexp))
;; codept-sexp

(begin
  (load "alist.scm")
  (load "codept.scm")
  (load "dune.scm")
  (load "lookup_tables.scm")
  ;; (codept->local-modules codept-sexp)
  (let ((x (define filedeps-tbl
             (dune->filedeps-tbl codept-sexp)
             ;;(codept->modules-table codept-sexp)
             )))
    '()))

;;filedeps-tbl
;;(length filedeps-tbl)
;; (hash-table-entries filedeps-tbl)
;; (filedeps-tbl "src/lib_stdlib/memory.mli")
;; (filedeps-tbl "src/proto_009_PsFLoren/lib_protocol/alpha_context.ml")
;; (filedeps-tbl "src/lib_stdlib/test/assert.ml")
;; (filedeps-tbl "src/openapi/openapi.mli")
;; (filedeps-tbl "src/lib_crypto/blake2B.ml")

;; or use (load ".obazl.d/opam_resolver.scm")?
;; opam pkgname syntax: dots disallowd.
;; <pkgname>         ::= (") <ident> (")
;; <ident>         ::= { <identchar> }* <letter> { <identchar> }*
;; <identchar>     ::= <letter> | <digit>  | "_" | "-"

(begin
  ;; (load "string.scm")
  ;; (load "utils.scm")
  ;; (load "pp.scm")
  (load "opam.scm")
  ;;(define opam-resolver (make-opam-resolver))
  (opam-exports->tezos-exports))

;; (length opam-resolver)
;; (hash-table-entries opam-resolver)
;; (for-each (lambda (opam)
;;             (let ((v (system (format #f "opam var ~S:version" opam) #t)))
;;               (format #t "~A: ~A\n" opam v)))
;;           (sort! (hash-table-keys opam-resolver) sym<?))

(opam-resolver 'camlzip)
(opam-resolver 'mtime.clock.os)

;; (set! (*s7* 'profile) 1)

(begin
  (load "dune.scm")
  (load "codept.scm")
  (load "starlark.scm")
  ;; pass both dune-pkg-tbls-alist and codept-sexp (or deps-pkgs-tbl)
  (starlark-emit-build-files dune-pkg-tbls-alist)
  '())

;;(show-profile)
(clear-profile)

;;(format #t "~W" opam-tbl)

(ns-tbl 'tezos-client-base)
(names-tbl 'Tezos_crypto)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns-tbl 'tezos-shell)
(ns-tbl 'Distributed_db)

(ns-tbl 'Test)
(ns-tbl 'Tezos_client_base)
(ns-tbl 'tezos-client-base)
(ns-tbl 'Block_header)

(ns-tbl 'tezos-store)
(names-tbl 'Tezos_store)
(names-tbl 'tezos-store)

(names-tbl 'Tezos_context_encoding)

;; bin_node/ tezos-node executable modules:
(ns-tbl 'tezos-node)
(ns-tbl 'Node_config_file)

tezos-protocol-compiler
(ns-tbl 'tezos-protocol-compiler-byte)
(ns-tbl 'Main_byte)

(ns-tbl 'Registerer)
(ns-tbl 'Json)

;; lib_crypto
(ns-tbl 'tezos-crypto)
(ns-tbl 'Base58)
(ns-tbl 'Data_encoding)

;; (for-each (lambda (k)
;;             (format #t "~A\n" k))
;;           (sort! (hash-table-keys ns-tbl)
;;                  sym<?))

;; aggregators with indirect or no modules
(names-tbl 'tezos-protocol-environment-sigs)
(ns-tbl 'Tezos_protocol_environment_sigs)


;;(alist? dune-pkg-tbls-alist)
;;(assoc "src" dune-pkg-tbls-alist)
;; (assoc-in '(:stanzas :executable :deps :contingent)
;;           (get-pkg dune-pkg-tbls-alist "src" "src/bin_client"))

(for-each (lambda (x)
            (format #t "~A\n" x))
          public-exe->label)

(hash-table-keys public-exe->label)

(public-exe->label
 ;;'src/openapi/rpc_openapi
 ;;'openapi/rpc_openapi
 ;;'rpc_openapi.bc
 ;;'test_mockup_wallet.exe
 ;;'test_mockup_wallet.bc
 ;;'test_mockup_wallet
 ;;'tezos-embedded-protocol-packer
 ;;'test_clic.exe
 ;;'gen/bip39_generator.exe  ;; lib_client_base
 ;; 'bip39_generator
 ;;'tezos-baker-008-PtEdo2Zk
 ;;'main_baker_008_PtEdo2Zk
 ;;'main_baker_alpha
 ;;'tezos-client-genesis
 ;;'tezos-protocol-compiler.tezos-protocol-packer.exe
 'tezos-protocol-compiler
 ;;'replace.exe
 )

(public-exe->label 'tezos-protocol-compiler)

;; initialize names lookup table. maps names to names and label, e.g.
;; private-name to public-name, public-name to bazel target label.
;; used by e.g. lib 'select' LHS resolution.

(length names-tbl)

(names-tbl 'tezos_protocol_compiler_byte)
(names-tbl 'main_baker_008_PtEdo2Zk)
(names-tbl 'RPC-directory)
(ns-tbl 'Block_hash)
(ns-tbl 'tezos-node)

(names-tbl 'tezos-base)
(names-tbl 'tezos_base)
(names-tbl 'Tezos_base)

(names-tbl 'block-validator)

(names-tbl 'tezos-client-genesis)

;; (hash-table-keys dune-pkg-tbls-alist)
;; (hash-table-entries (car dune-pkg-tbls-alist))
;; (length (hash-table-keys deps-pkgs-tbl))
;; (length deps-pkgs-tbl)


(ns-tbl 'Tezos_client_base)

;; aggregators with indirect or no modules
(names-tbl 'tezos-protocol-environment-sigs)
(ns-tbl 'tezos-protocol-environment-sigs)
;;'ezos_protocol_environment_sigs)

(public-name->module-name 'tezos-protocol-environment-packer)
(ns-tbl 'tezos_protocol_environment_packer)

(names-tbl 'tezos-base)
(ns-tbl 'Block_header)

(ns-tbl 'Main_native)
(ns-tbl 'Tezos_protocol_environment_sigs)
(ns-tbl 'tezos-protocol-environment-sigs)
(assoc-in '(:opts :flags) (ns-tbl 'Tezos_stdlib))
(assoc-in '(:opts :raw) (ns-tbl 'Tezos_stdlib))
(assoc-in '(:submodules :direct) (ns-tbl 'Tezos_stdlib))

(assoc-in '(:submodules :direct) (ns-tbl 'Tezos_client_base))
(assoc-in '(:submodules :indirect) (ns-tbl 'Tezos_client_base))
(assoc :opts (ns-tbl 'Tezos_client_commands))

(ns-tbl 'Tezos_crypto)
(assoc :opts (ns-tbl 'Tezos_crypto))

(ns-tbl 'Tezos_proxy)

(ns-tbl 'Tezos_protocol_environment_alpha)

(cdr (assoc-in '(:name :public) (ns-tbl 'Lmdb)))

(length ns-tbl)
(hash-table-entries ns-tbl)
(hash-table-keys ns-tbl)
(for-each (lambda (entry)
            (format #t "~A\n" entry))
          (sort! (map (lambda (x) (symbol->string x))
                      (hash-table-keys ns-tbl))
                 string<?))

;; (begin
;;   (load "codept.scm")
;;   (load "opam.scm")
;;   (let ((x (define opam-tbl (codept->opam-table codept-sexp))))
;;     '())
;;   )

(update-opam-table 'zarith)
opam-tbl

(length opam-tbl)
(hash-table-entries opam-tbl)

(hash-table-keys opam-tbl)
(opam-tbl 'data-encoding)
(opam-tbl 'zarith)
(opam-tbl 'tezos-protocol-alpha)

(opam-tbl 'resto-cohttp-server)

;; Orphaned modules. e.g. Deterministic_nonce
;; src/bin_signer/socket_daemon.ml deps: (Deterministic_nonce)
;; Deterministic_nonce is embedded in lib_signer_services:Signer_messages
;; ( (file src/lib_signer_services/signer_messages.ml) (deps
;; ((TzEndian) (Signature) (Data_encoding) (Bytes))) )

(ns-tbl 'Deterministic_nonce)

(hash-table-entries stdlib-tbl)
(length stdlib-tbl)
(stdlib-tbl 'String)

(assoc :submodules (ns-tbl 'Tezos_client_base))

(ns-tbl 'test_mockup_wallet)

(ns-tbl 'tezos-base)
(assoc :opts (ns-tbl 'Tezos_crypto))

(ns-tbl 'Tezos_proxy)
(ns-tbl 'Tezos_client_commands)

;;(public-name->module-name 'tezos-p2p)
(ns-tbl 'Main_native)
(ns-tbl 'Tezos_protocol_environment_packer)

(ns-tbl 'tezos-protocol-environment-sigs)

(public-name->module-name 'tezos-protocol-environment-sigs)
(public-name->module-name 'tezos-client-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin
  (load "codept.scm")
  (codept-srcfile->depslist
   ;; "src/bin_client/client_protocols_commands.mli"
   ;; "vendors/statz/lib/grid.ml"
   "src/lib_base/block_header.ml"
   codept-sexp)) ;;  (deps ((Client_commands)))

(ns-tbl 'Tezos_crypto)

(length dune-pkg-tbls-alist)

dune-crypto

(ns-tbl 'Tezos_error_monad) ;;.TzLwtreslib)

(ns-tbl 'Zplus)

(ns-tbl 'Tezos_lwt_result_stdlib)

(ns-tbl 'Tezos_hacl_glue)

(ns-tbl 'Tezos_base)

(ns-tbl 'TzLwtreslib)

(length (codept->local-modules codept-sexp))

(length ns-tbl)
(hash-table-entries ns-tbl)

(ns-tbl 'Block_header)

(ns-tbl 'Operation_list_list_hash)

(assoc :ml (ns-tbl 'Block_header))

(ns-tbl 'Tezos_base)
(ns-tbl 'Light_consensus)

(ns-tbl 'Tezos_proxy)
(public-name->module-name 'tezos-rpc)
(ns-tbl 'Tezos_p2p_services)
(ns-tbl 'Tezos_base)

(ns-tbl 'RPC_context)

;; resolve-libdep module: Light_consensus, dep: (Uri)
;;   ns: Tezos_proxy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin
  (load "dune.scm")
  (load "codept.scm")
  (load "starlark.scm")
  ;; pass both dune-pkg-tbls-alist and codept-sexp (or deps-pkgs-tbl)
  (starlark-elaborate-pkg-tbls dune-pkg-tbls-alist ns-tbl)
  '())

;; (begin
;;   (load "codept.scm")
;;   (let ((x (define deps-pkgs-tbl (codept-sexp->deps-pkgs-tbl codept-sexp))))
;;     '()))

;; (begin
;;   (load "codept.scm")
;;   (let ((x (define ns-tbl (codept-sexp->ns-tbl codept-sexp))))
;;     '()))

;; (hash-table-entries (car dune-pkg-tbls-alist))
;; (hash-table-entries (cadr dune-pkg-tbls-alist))
;; (length (hash-table-keys (car dune-pkg-tbls-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((x (define dune-lib
           ((car dune-pkg-tbls-alist) "src/proto_009_PsFLoren/lib_delegate"))))
  '())
dune-lib

(let ((x (define dune-packer
           ((car dune-pkg-tbls-alist) "src/lib_protocol_environment/s_packer"))))
  '())
dune-packer
(assoc-in '(:stanzas library :empty) (list dune-packer))

(let ((x (define dune-proxy
           ((car dune-pkg-tbls-alist) "src/lib_proxy"))))
  '())
dune-proxy

dune-mockup

(let ((x (define dune-mockup
           ((car dune-pkg-tbls-alist) "src/lib_mockup"))))
  '())

(let ((x (define dune-alpha
           ((car dune-pkg-tbls-alist) "src/proto_alpha/lib_parameters"))))
  '())
dune-alpha


;; (for-each (lambda (pkg)
;;             (display (format #f "~A" (hash-table-entries pkg))) (newline))
;;           dune-pkg-tbls-alist)

(let ((x (define dune-protocol_environment
           ((cadr dune-pkg-tbls-alist) "src/lib_protocol_environment"))))
  '())
dune-protocol_environment

(cdr ((car dune-pkg-tbls-alist) "src/bin_attacker"))
((cadr dune-pkg-tbls-alist) "vendors/statz/lib")

(begin
  (load "diagnostics.scm")
  (print-multiple-lib-stanzas dune-pkg-tbls-alist)
  '())

(let* ((dune-pkg-tbl (car dune-pkg-tbls-alist))
       (ks (hash-table-keys dune-pkg-tbl))
       (pkg (dune-pkg-tbl "src/lib_protocol_compiler"))
       (stdlib (dune-pkg-tbl "src/lib_stdlib"))
       (stanzas-alist (cdr (assoc :stanzas pkg))))
  ;; stdlib)
  pkg)
  (assoc 'ocamllex stanzas-alist))

(begin
  (load "dune.scm")
  (load "codept.scm")
  (let ((outp (begin  ;; (outp (open-output-string))
                (if (not (file-exists? ".obazl.d/tmp"))
                    (system "mkdir -p .obazl.d/tmp"))
                (open-output-file ".obazl.d/tmp/codept.args"))))
    (dune-emit-codept-args dune-pkg-tbls-alist outp)
    (close-output-port outp))
  '())

(begin
  (load "codept.scm")
  (system "codept -expand-deps -sexp -k -args .obazl.d/tmp/codept.args > .obazl.d/tmp/codept.deps 2> .obazl.d/tmp/codept.log"))

(let ((x (define codept-sexp
           (read-codept-depsfile ".obazl.d/tmp/codept.deps"))))
  '())

(begin
  (load "codept.scm")
  (let ((x (define deps-pkgs-tbl (codept-sexp->deps-pkgs-tbl codept-sexp))))
    '()))

deps-pkgs-tbl

(let ((x (define codept-lib-base (deps-pkgs-tbl "src/lib_base"))))
'())
codept-lib-base
dune-lib-base

lib-base

(let ((x (define codept-statz (deps-pkgs-tbl "vendors/statz/lib"))))
  '())
codept-statz

;; (length lib-base)

;; (hash-table-keys deps-pkgs-tbl)
;; (hash-table-entries deps-pkgs-tbl)
;; (length (hash-table-keys deps-pkgs-tbl))
;; (length deps-pkgs-tbl)

;; (car dune-pkg-tbls-alist) ;; pkg tbl for one root dir

dune-openapi
codept-lib-base
dune-lib-base

;; (begin
;;   (load "emit.scm")
;;   (emit-buildfiles dune-pkg-tbls-alist ".obazl.d/codept.deps"))

(dunefile? "a.dune")

(dump-stanzas (car pkgs-tbls))

srcfiles

(srcfiles "./lib_test")

(hash-table-keys (car srcfiles))

(load "camlark.scm")

(length (codept->modules-table codept-sexp))

(codept->file-deps codept-sexp)

;; (let* ((locals (codept->local-modules codept-sexp))
;;        (module (nth 200 locals)))
;;   module)
;;  (assoc 'ml module))



(hash-table-keys (cadr (assoc :modules (deps-pkgs-tbl "src/lib_base"))))

(hash-table-ref (cadr (assoc :modules (pkgs-tbl "lib_re")))
                'Uri_re)

(hash-table-ref pkgs-tbl "config")


(codept->version codept-sexp)

(codept->file-deps codept-sexp)

(codept->local-modules codept-sexp)

(codept->unknown-modules codept-sexp)

(load "utils.scm")

(define x (system "opam var boo:installed" #t))
x
(display x) (newline))

(file-exists? "/Users/gar/.opam/4.10/lib/yojson/META")

(hash-table-keys depsmap)

(hash-table-ref depsmap "lib_re")

(hash-table-keys (cadr (depsmap "lib_re")))

(hash-table-ref (cadr (depsmap "lib_re")) 'Uri_legacy)

(camlark-handler ".obazl.d/codept.deps")

(import-dunefile "lib_re")

(assoc 'libraries (cdr x))

;; (define e '((a 1) (b 2) (c 3)))
;; (assoc 'a e)


;; (define h1 #h(:a 1))
;; h1
;; (hash-table-set! h1 :b 2)
;; (hash-table-set! h1 :c 3)

;; #{(:a 1 :b 2)}#

;; (display #h(:a 1))
;; (newline)
;; (display #h(:a 1 :b "str")) (newline)

;; (load "json.scm")

;; (s7->json x)

;; (define j (json->s7 "{\"title\": \"Person\", \"type\": \"object\"}"))
;; j

;; (s7->json j)

;; (display
;;  (with-output-to-string
;;    (lambda ()
;;      (s7->json ;; j)
;;       (json->s7 "{\"title\": \"Person\", \"type\": \"object\"}")
;;      ))))
;; (newline)

;; (write (json->s7 "{\"test\":\"a\\\\b\"}"))

;; (display (with-output-to-string (lambda () (s7->json (vector 1 2)))))

(define ht (hash-table 'aa 1 'bb 2))

;; (display (with-output-to-string
;;            (lambda ()
;;              (s7->json ht))))

;;               (vector 1 2)))))

;; (write (with-output-to-string (lambda () (s7->json (vector "asdf" #i(1 2))))))

;; (display
;;  (with-output-to-string
;;    (lambda ()
;;      (s7->json (json->s7 "{\"title\": \"Person\", \"type\": \"object\"}")))))


(load "srfi.scm")
(any number? l1)

;;(load "r7rs.scm")

(let-values (((a b) (values 1 2)))
  (+ a b))

(define l1 '(1 2 3 4 5))
(define l2 '(c d  2))

(find-if even? l1)

(begin
  (load "alist.scm")
  (let ((als '((a 1)
               (b ((b1 11) (:x ((x1 y1) (x2 y2) (x3 y3)))
                   (b1 21) (b2 12)))
               (a 2)
               (c 3))))
    (alist-update-in! als '(b :x x2)
                      (lambda (old)
                        (if (null? old)
                            '(:test)
                            (list (list :foo
                                  (cadr old))))))
als))

    ;; (alist-delete 'c als)))
    ;; (assoc-in+ '(b :x) als)))

(begin
  (load "alist.scm")
  (rassoc 1 '((a 1) (b 2) (c 3)) equal? cadr))

;; (substring "Tezos_base__TzPervasives" 4)

;; (load "string.scm")
;; (string-contains "Tezos_base__TzPervasives" "__")

;; (string-prefix "Tezos_base__TzPervasives" "__")

(string-take  "Tezos_base__TzPervasives" 7)

(load "string.scm")

(string-split "this    is a test" #\space)

(load "alist.scm")

(let ((lst '((a 1) (b 2) (c 3) (d 4) (e 5))))
  (alist-delete '(b) lst))
