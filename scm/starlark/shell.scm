(define (emit-shell-cmd outp tool
                        with-stdout?
                        srcs
                        deps
                        action
                        outputs
                        pkg-path
                        stanza)
  (if *mibl-debug-s7*
      (begin
        (format #t "~A: ~A~%" (ublue "emit-shell-cmd") action)
        (format #t "~A: ~A~%" (uwhite "non-bash tool") tool)
        (format #t "~A: ~A~%" (uwhite "srcs") srcs)
        (format #t "~A: ~A~%" (uwhite "deps") deps)
        (format #t "~A: ~A~%" (uwhite "xdeps") (dissoc '(::tools) deps))
        (format #t "~A: ~A~%" (uwhite "outputs") outputs)))

  (format outp "    cmd   = \" \".join([\n")

  ;;FIXME: dune seems to cd to the dir containing the executable and
  ;;run from there, so tools may assume that dep filepaths are
  ;;relative to that dir. to accomodate this in Bazel we need to copy
  ;;or symlink deps to the pkg dir.  Example: jsoo/compiler/tests-compiler
  ;; rule to generate dune.inc.gen.
  ;;TODO: insert cp or ln before the tool cmd.

  ;; if action takes no args, but we have deps, cp deps to cwd

  (for-each
   (lambda (cmd)
     (if *mibl-debug-s7*
         (format #t "~A: ~A~%" (magenta "PROCESSING cmd") cmd))
     (if (eq? :cmd (car cmd))
         (let-values
             (((tool-dep? tool xargs)
               (derive-cmd pkg-path cmd deps outputs)))

           (if *mibl-debug-s7*
               (begin
                 ;; (format #t "~A:\n" (red "derived cmd"))
                 (format #t "~A: ~A~%" (magenta "tool-dep?")
                         tool-dep?)
                 (format #t "~A: ~A~%" (magenta "tool") tool)
                 (format #t "~A: ~A~%" (magenta "XARGS") xargs)
                 ;; (error 'X "STOP shell")
                 ))

           ;; cmd   = " ".join([
           ;;     "$(execpath //compiler/bin-js_of_ocaml:js_of_ocaml)",
           ;;     "check-runtime",
           ;;     "$(SRCS)",
           ;;     "> $@"
           ;; ]),

           (if (null? xargs)
               (if (truthy? srcs) ;; deps)
                   (format outp "~{        \"cp $(locations ~A) . ;\"~^,~%~},~%" srcs)))

           (if tool-dep?
               ;; HACK: $(location ...)
               (format outp "        \"$(execpath ~A)\",\n" tool)
               (format outp "        \"~A\",\n" tool))

           ;; (for-each (lambda (arg)
           ;;             (format outp "~A" arg))
           ;;           xargs)
           ;; (format outp "        \"$(SRCS)\",\n")
           (format outp "~{        \"~A\",~^\n~}\n" xargs)
           (if (equal? 'diff tool)
                 (format outp "        \"> $@\"\n"))
           )
         ;; else
         (if (eq? :stdout (car cmd))
             (format outp "        \"> $@\"\n")
             (error 'unknown-cmd
                    (format #f "unknown cmd: ~A" cmd)))
         ))
   action)
  (if *mibl-debug-s7*
      (format #t "~A~%" (red "emitted cmd attrib")))
  (format outp "    ]),\n"))
