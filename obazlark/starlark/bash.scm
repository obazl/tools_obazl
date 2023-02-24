(define (-expand-bash-tool tool pkg-path stanza)
  (if *debugging*
      (begin
        (format #t "~A: ~A~%" (ublue "-expand-bash-tool") tool)
        (format #t "~A: ~A~%" (uwhite "pkg-path") pkg-path)
        (format #t "~A: ~A~%" (uwhite "stanza") stanza)))

  (let ((tool (if (string? tool) tool (format #f "~A" tool))))
    (if (member (string->symbol tool) shell-tools)
        (begin
          (if *debugging*
              (format #t "~A: ~A~%" (red "FOO") tool))
          #f)
        (let* ((key (string->keyword tool)))
          (let* ((deps (assoc-val :deps stanza))
                 (_ (if *debugging*
                        (format #t "~A: ~A~%" (yellow "searching deps") deps)))
                 (match (find-if (lambda (dep)
                                   ;; (format #t "~A: ~A~%" (yellow "dep") dep)
                                   (eq? key (car dep)))
                                 deps)))
            (if *debugging* (format #t "~A: ~A~%" (yellow "match") match))
            (if match
                (let* ((lbl (cdr match))
                       (pkg (assoc-val :pkg lbl))
                       ;; (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                       (pkg (if (equal? pkg-path pkg) "" pkg))
                       (tgt (if-let ((t (assoc-val :tgt lbl)))
                                    (format #f "//~A:~A" pkg t)
                                    (error 'fixme "bash tool has :tgts"))))
                  (if *debugging* (format #t "~A: ~A~%" (yellow "RESOLVED") tgt))
                  tgt)
                ;; (format #t "~A~%" tool)
                ))))))

(define (-expand-bash-arg arg pkg-path stanza)
  (if *debugging*
      (begin
        (format #t "~A: ~A~%" (blue "-expand-bash-arg") arg)
        (format #t "~A: ~A~%" (white "pkg-path") pkg-path)
        (format #t "~A: ~A~%" (white "stanza") stanza)))
  (let* ((key (string->keyword arg)))
    ;; search :deps
    (let* ((deps (assoc-val :deps stanza))
           ;; (_ (format #t "~A: ~A~%" (yellow "deps") deps))
           (match (find-if (lambda (dep)
                           ;; (format #t "~A: ~A~%" (yellow "dep") dep)
                           (eq? key (car dep)))
                         deps)))
      (if *debugging* (format #t "~A: ~A~%" (yellow "match") match))
      (if match
          (let* ((lbl (cdr match))
                 (pkg (assoc-val :pkg lbl))
                 ;; (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                 (pkg (if (equal? pkg-path pkg) "" pkg))
                 (tgt (if-let ((t (assoc-val :tgt lbl)))
                              (format #f "$(rootpath //~A:~A)" pkg t)
                              (if-let ((t (assoc-val :tgts lbl)))
                                      (format #f "$(rootpaths ~A:~A)" pkg t)
                                      (error 'fixme "lbl missing tgt/tgs")))))
            (if *debugging* (format #t "~A: ~A~%" (yellow "RESOLVED") tgt))
            tgt)
          arg))))

;; handle all pct-vars: %{deps}, %{target}, etc.
;; also filename literals
(define (-expand-bash-args args pkg-path stanza)
  (if *debugging* (format #t "~A: ~A~%" (ublue "-expand-bash-args") args))
  ;; args is either a string (the tool), a list of one string, or list of strings & syms
  ;; (bash "...cmd...")
  ;; or (run bash "foo.sh" bar ...)
  (if (null? args)
      (begin
        (values '(:FOO) '(:BAR)) ;; expanded-tool expanded-args)
        )
      (let* ((pct-regex "%{([^}]+)}")
             (rgx (regex.make))
             (rx (regcomp rgx pct-regex REG_EXTENDED)))
        (let* ((arg-list
                (cond
                 ((string? args) (list args))
                 ((symbol? args) (list (format #f "~A" args)))
                 (else (if (and (string? (car args)) (null? (cdr args)))
                           (string-tokenize (car args) char-set:graphic)
                           args)))))
          (if *debugging* (format #t "~A: ~A~%" (red "arg-list") arg-list))

          ;; FIXME: treat first arg as tool

          (let* ((expanded-tool
                  (-expand-bash-tool (car arg-list) pkg-path stanza))
                 (_ (if *debugging*
                        (format #t "~A: ~A~%" (yellow "expanded TOOL") expanded-tool)))
                 (expanded-args
                  (map (lambda (arg)
                         (if *debugging*
                             (format #t "~%~A: ~A (~A)~%" (red "expanding ARG") arg (type-of arg)))
                         (let* ((arg (format #f "~A" arg))
                                (match-ct 2)
                                (res (regexec rgx arg match-ct 0))
                                (_ (if *debugging*
                                       (format #t "~A: ~A~%"
                                           (red "regex result") res))))
                           (if (int-vector? res)
                               (let* ((rm-so (int-vector-ref res 0))
                                      (rm-eo (int-vector-ref res 1))
                                      (sub-so (int-vector-ref res 2))
                                      (sub-eo (int-vector-ref res 3))
                                      (pfx  (substring arg 0 rm-so))
                                      (sfx  (substring arg rm-eo))
                                      (match (substring arg sub-so sub-eo))
                                      (replacement (-resolve-match match pkg-path stanza)))
                                 ;; (format #t "~A: ~A~%" (red "sub_so") sub-so)
                                 ;; (format #t "~A: ~A~%" (red "sub_eo") sub-eo)
                                 ;; (format #t "~A: ~A~%" (red "rm_so") rm-so)
                                 ;; (format #t "~A: ~A~%" (red "rm_eo") rm-eo)
                                 ;; (format #t "~A: ~A~%" (red "match") match)
                                 ;; (format #t "~A: ~A~%" (red "pfx") pfx)
                                 ;; (format #t "~A: ~A~%" (red "sfx") sfx)
                                 (format #f "~A~A~A" pfx replacement sfx))
                               (-expand-bash-arg arg pkg-path stanza))))
                       (cdr arg-list))))
            (regfree rgx)
            (let* ((expanded-args
                    (if-let ((stdout (assoc-in '(:actions :stdout) stanza)))
                            (begin
                              (if *debugging*
                                  (format #t "~A: ~A~%" (yellow "stdout") stdout))
                              (append expanded-args (list "> $@")))
                            (let ((outs (-expand-outputs pkg-path stanza)))
                              (append expanded-args (list (format #f "cp ~A $@" outs)))))))
              (if *debugging*
                  (format #t "~A: ~A~%" (red "EXPANDED ARGS") expanded-args))
              (values expanded-tool expanded-args)))))))

(define (emit-bash-cmd outp with-stdout? outs pkg-path stanza)
  (if *debugging*
      (format #t "~A: ~A~%" (blue "emit-bash-cmd") stanza))
  (let* ((args (assoc-in '(:actions :cmd :args) stanza))
         (args (cdr args))
         (_ (if *debugging*
                (format #t "~A: ~A~%" (yellow "bash args") args)))
         )
    (let-values (((tool parsed-args)
                  (-expand-bash-args args pkg-path stanza)))
      (if *debugging*
          (begin
            (format #t "~A: ~A~%" (yellow "tool") tool)
            (format #t "~A: ~S~%" (yellow "parsed args") parsed-args)))

      (format outp "    cmd_bash   = \" \".join([\n")
      (format outp "        \"$(execpath ~A)\",~%" tool)
      (format outp "        \"$(SRCS);\",~%")
      ;; (format outp "~{        ~S~^,~%~}~%" parsed-args)
      (if with-stdout?
          (format outp "        \"> $@\",\n")
          (format outp "        \"cp ~{~A ~} $@\",\n" outs))
      (format outp "    ]),\n")
      (if tool
          (format outp "    tools   = [~S]\n" tool))
      )))

(define (emit-bash-srcs outp srcs pkg-path stanza)
  (if *debugging*
      (format #t "~A: ~A~%" (ublue "emit-bash-srcs") srcs))
  ;; remove bash tool from srcs

  (let* ((args (assoc-in '(:actions :cmd :args) stanza))
         (args (if (equal? args '(:args)) '() (cadr args))))
    (if (truthy? args)
        (let-values (((tool parsed-args)
                      (-expand-bash-args args pkg-path stanza)))
          (if *debugging*
              (format #t "~A: ~A~%" (yellow "TOOL") tool))
          (let ((srcs (remove tool srcs)))
            (if *debugging* (format #t "~A: ~A~%" (red "srcs") srcs))
            (format outp "    srcs  = [\n")
            (format outp "~{        \"~A\"~^,\n~}\n" srcs)
            (format outp "    ],\n")))
        (begin
          (if *debugging* (format #t "~A: ~A~%" (red "srcs") srcs))
          (format outp "    srcs  = [\n")
          (format outp "~{        \"~A\"~^,\n~}\n" srcs)
          (format outp "    ],\n")))
    ))

