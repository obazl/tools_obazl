(define (emit-shell-cmd outp tool
                        with-stdout?
                        deps
                        action
                        outputs
                        pkg-path
                        stanza)
  (format #t "~A: ~A~%" (ublue "emit-shell-cmd") action)
  (format #t "~A: ~A~%" (uwhite "non-bash tool") tool)

  (format outp "    cmd   = \" \".join([\n")

  (for-each
   (lambda (cmd)
     (format #t "~A: ~A~%" (magenta "PROCESSING cmd") cmd)
     (if (eq? :cmd (car cmd))
         (let-values
             (((tool-dep? tool xargs)
               (derive-cmd pkg-path cmd deps outputs)))
           ;; (format #t "~A:\n" (red "derived cmd"))
           (format #t "~A: ~A~%" (magenta "tool-dep?")
                   tool-dep?)
           (format #t "~A: ~A~%" (magenta "tool") tool)
           (format #t "~A: ~A~%" (magenta "XARGS") xargs)
           ;; (error 'X "STOP shell")

           ;; cmd   = " ".join([
           ;;     "$(execpath //compiler/bin-js_of_ocaml:js_of_ocaml)",
           ;;     "check-runtime",
           ;;     "$(SRCS)",
           ;;     "> $@"
           ;; ]),


           (if tool-dep?
               ;; HACK: $(location ...)
               (format outp "        \"$(execpath ~A)\",\n" tool)
               (format outp "        \"~A\",\n" tool))

           ;; (for-each (lambda (arg)
           ;;             (format outp "~A" arg))
           ;;           xargs)
           ;; (format outp "        \"$(SRCS)\",\n")
           (format outp "~{        \"~A\",~^\n~}\n" xargs)
           )
         ;; else
         (if (eq? :stdout (car cmd))
             (format outp "        \"> $@\"\n")
             (error 'unknown-cmd
                    (format #f "unknown cmd: ~A" cmd)))
         ))
   action)
  (format #t "~A~%" (red "emitted cmd attrib"))
  (format outp "    ]),\n"))
