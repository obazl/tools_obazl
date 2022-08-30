(define (-profile->opts opts-alist)
  (format #t "~A: ~A~%" (ublue "-profile->opts") opts-alist)
  (let ((compile-opts (if-let ((copts (assoc-val :compile-opts opts-alist)))
                              (assoc-val :flags copts)
                              #f))
        (ocamlc-opts (if-let ((copts (assoc-val :ocamlc-opts opts-alist)))
                             (begin
                                '("-ocamlcfoo" "-ocamlcbar"))
                              #f))
        (ocamlopt-opts (if-let ((copts (assoc-val :ocamlopt-opts opts-alist)))
                             (begin
                                '("-ocamloptfoo" "-ocamloptbar"))
                              #f))
        (archive-opts (if-let ((aopts (assoc-val :archive-opts opts-alist)))
                              (begin
                                '("-afoo" "-abar"))
                              #f))
        (link-opts (if-let ((aopts (assoc-val :link-opts opts-alist)))
                           (begin
                             '("-lfoo" "-lbar"))
                           #f)))
    (format #t "~A: ~A~%" (blue "compile-opts") compile-opts)
    (format #t "~A: ~A~%" (blue "archive-opts") link-opts)
    (format #t "~A: ~A~%" (blue "link-opts") link-opts)
    ;; (error 'STOP "STOP profiles")
    (values compile-opts ocamlc-opts ocamlopt-opts
            archive-opts link-opts)))

(define (instructions profiles)
  (string-join
   `(""
     " To enable these toolchain profiles, add the following 'register_toolchains'"
     " lines (without the leading '#') to WORKSPACE.bazel, *before* the call to bootstrap()."
     " For example:"
     ""
     ,(format #f "~{register_toolchains(\"//bzl/profiles:~A\")~^~%#~}~%#" profiles)
     "load(\"@coswitch//:BOOTSTRAP.bzl\", \"bootstrap\")"
     "bootstrap()"
     )
   :delim "\n#"))

(define (-env->profile-names env)
  (let ((names '()))
    (for-each (lambda (profile)
                (let ((name (keyword->symbol (car profile))))
                  (if (eq? name '_) (set! names (cons "default" names)))
                  ;; (if (assoc-val :compile-opts (cdr profile))
                  ;;     (set! names (cons (format #f "~A" name) names)))
                  (if (assoc-val :ocamlc-opts (cdr profile))
                      (set! names (cons (format #f "vm_~A" name) names)))
                  (if (assoc-val :ocamlopt-opts (cdr profile))
                      (set! names (cons (format #f "sys_~A" name) names)))))
              env)
    names))

;; profiles
;; dune user-defined profile properties may not be mappable to e.g. --compilation_mode
;; but dev and release seem to be standard, mapping to fastbuild and opt

(define (emit-profiles ws pkg)
  (format #t "~A: ~A\n" (ublue "emit-profiles") pkg)
  (let* ((pkg-path (car (assoc-val :pkg-path pkg)))
         (dunefile (assoc :dune pkg)))
    (if dunefile
        (let* ((stanzas (cdr dunefile))
               (_ (format #t "~A: ~A~%" (uwhite "stanzas") stanzas))
               (env (assoc-val :env stanzas))
               (_ (format #t "~A: ~A~%" (uwhite "env") env))
               (profiles-dir (format #f "~A/bzl/profiles" pkg-path))
               (_ (system (format #f "mkdir -p ~A" profiles-dir)))
               (build-file (format #f "~A/BUILD.bazel" profiles-dir))
               (_ (format #t "~A: ~A~%" (uwhite "build-file") build-file))

               (outp
                (catch #t
                       (lambda ()
                         (open-output-file build-file))
                       (lambda args
                         (format #t "OPEN ERROR")
                         (error 'STOP "open profiles error"))
                       ))
               )
          (format outp "load(\"@rules_ocaml//toolchain:profiles.bzl\",~%")
          (format outp "     \"toolchain_profile_selector\", \"ocaml_profile\")")
          (newline outp)

          (format outp (instructions (-env->profile-names env)))

          (newline outp)
          (newline outp)

          ;; NB: in the (env ...) stanza,
          ;; "The first form (<profile> <settings>) that corresponds to
          ;; the selected build profile will be used to modify the
          ;; environment in this directory. You can use _ to match any
          ;; build profile."

          ;; Which implies that _ can be used to override any profile?
          ;; e.g. you define (env (dev...)) and then --profile=dev
          ;; ok, then you do (env (_ ...) (dev...)); now --profile-dev
          ;; uses the _ settings. moreover, the default profile is dev,
          ;; does that mean the _ will take effect?
          ;; indeed with _ you can make up any profile name and it will match.

          ;; IOW, this is a SNAFU. Assumption: wildcard _ only used in
          ;; isolation, not with other profile definitions.

          (format #t "~A~%" (blue "processing profiles"))
          (for-each
           (lambda (profile)
             (format #t "~A: ~A~%" (ublue "profile") profile)
             (let ((name (if (eq? :_ (car profile))
                             "default"
                             (keyword->symbol (car profile)))))
               (let-values (((compile-opts ocamlc-opts ocamlopt-opts
                              archive-opts link-opts)
                             (-profile->opts (cdr profile))))

                 ;;TODO: if name is dev, use target_settings = [\":fastbuild_mode\"]
                 ;;TODO: if name is release, use target_settings = [\":opt_mode\"]

                 (if (eq? :_ (car profile))
                     (begin
                       (format outp "##################~%")
                       (format outp "toolchain_profile_selector(~%")
                       (format outp "    name          = \"~A\",~%" name)
                       (format outp "    profile       = \":~A_profile\",~%" name)
                       (format outp ")~%")
                       (newline outp)
                       (newline outp)

                       (format outp "ocaml_profile(~%")
                       (format outp "    name         = \"~A_profile\",~%" name)
                       (if compile-opts
                           (begin
                             (format outp "    compile_opts = [~%")
                             (format outp "~{        \"~A\"~^,~%~}~%" compile-opts)
                             (format outp "    ]~%")))
                       (if archive-opts
                           (begin
                             (format outp "    archive_opts = [~%")
                             (format outp "~{        \"~A\"~^,~%~}~%" archive-opts)
                             (format outp "    ]~%")))
                       (if link-opts
                           (begin
                             (format outp "    link_opts    = [~%")
                             (format outp "~{        \"~A\"~^,~%~}~%" link-opts)
                             (format outp "    ]~%")))
                       (format outp ")~%")
                       (newline outp)))

                 (if ocamlc-opts
                     (begin
                       (format outp "##################~%")
                       (format outp "toolchain_profile_selector(~%")
                       (format outp "    name                   = \"vm_~A\",~%" name)
                       (format outp "    profile                = \":vm_~A_profile\",~%" name)
                       (format outp "    target_compatible_with = [\"@ocaml//host/target:vm\"],~%")
                       (if (eq? name 'release)
                           (format outp "    target_settings        = [\":opt_mode\"]~%"))
                       (format outp ")~%")
                       (newline outp)
                       (newline outp)

                       (format outp "ocaml_profile(~%")
                       (format outp "    name         = \"vm_~A_profile\",~%" name)
                       (if compile-opts
                           (begin
                             (format outp "    compile_opts = [~%")
                             (format outp "~{        ~S~^,~%~}~%"
                                     (concatenate compile-opts ocamlc-opts))
                             (format outp "    ]~%")))
                       (if archive-opts
                           (begin
                             (format outp "    archive_opts = [~%")
                             (format outp "~{        ~S~^,~%~}~%" archive-opts)
                             (format outp "    ]~%")))
                       (if link-opts
                           (begin
                             (format outp "    link_opts    = [~%")
                             (format outp "~{        ~S~^,~%~}~%" link-opts)
                             (format outp "    ]~%")))
                       (format outp ")~%")
                       (newline outp)
                       ))

                 (if ocamlopt-opts
                     (begin
                       (format outp "##################~%")
                       (format outp "toolchain_profile_selector(~%")
                       (format outp "    name                   = \"sys_~A\",~%" name)
                       (format outp "    profile                = \":sys_~A_profile\",~%" name)
                       (format outp "    target_compatible_with = [\"@ocaml//host/target:sys\"],~%")
                       (if (eq? name 'release)
                           (format outp "    target_settings        = [\":opt_mode\"]~%"))
                       (format outp ")~%")
                       (newline outp)
                       (newline outp)

                       (format outp "ocaml_profile(~%")
                       (format outp "    name         = \"sys_~A_profile\",~%" name)
                       (if compile-opts
                           (begin
                             (format outp "    compile_opts = [~%")
                             (format outp "~{        ~S~^,~%~}~%"
                                     (concatenate compile-opts ocamlopt-opts))
                             (format outp "    ]~%")))
                       (if archive-opts
                           (begin
                             (format outp "    archive_opts = [~%")
                             (format outp "~{        ~S~^,~%~}~%" archive-opts)
                             (format outp "    ]~%")))
                       (if link-opts
                           (begin
                             (format outp "    link_opts    = [~%")
                             (format outp "~{        ~S~^,~%~}~%" link-opts)
                             (format outp "    ]~%")))
                       (format outp ")~%")
                       (newline outp)
                       ))

                 )))
           env)
          (newline outp)

          ;; emit bld settings for standard compilation_modes
          (format outp "################################~%")
          (format outp "config_setting(~%")
          (format outp "    name = \"dbg_mode\",~%")
          (format outp "    values = {\"compilation_mode\": \"dbg\"},~%")
          (format outp ")~%")
          (newline outp)

          (format outp "config_setting(~%")
          (format outp "    name = \"fastbuild_mode\",~%")
          (format outp "    values = {\"compilation_mode\": \"fastbuild\"},~%")
          (format outp ")~%")
          (newline outp)

          (format outp "config_setting(~%")
          (format outp "    name = \"opt_mode\",~%")
          (format outp "    values = {\"compilation_mode\": \"opt\"},~%")
          (format outp ")~%")
          (newline outp)

          ;; other config_settings
          ;; (format outp "config_setting(~%")
          ;; (format outp "    name = \"~A\"~%," name)
          ;; (format outp "    flag_values = {\"//bzl:foo\": \"bar\"},~%")
          ;; (format outp ")~%")
          ;; (newline outp)

          (close-output-port outp)))))

