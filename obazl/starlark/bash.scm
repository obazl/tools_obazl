(define (-expand-bash-tool tool pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-expand-bash-tool") tool)
  (format #t "~A: ~A~%" (blue "pkg-path") pkg-path)
  (format #t "~A: ~A~%" (blue "stanza") stanza)

  (let ((tool (if (string? tool) tool (format #f "~A" tool))))
    (if (member (string->symbol tool) shell-tools)
        #f
        (let* ((key (string->keyword tool)))
          ;; search :deps
          (let* ((deps (assoc-val :deps stanza))
                 (_ (format #t "~A: ~A~%" (yellow "searching") deps))
                 (match (find-if (lambda (dep)
                                   ;; (format #t "~A: ~A~%" (yellow "dep") dep)
                                   (eq? key (car dep)))
                                 deps)))
            (format #t "~A: ~A~%" (yellow "match") match)
            (if match
                (let* ((lbl (cdr match))
                       (pkg (assoc-val :pkg lbl))
                       ;; (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                       (pkg (if (equal? pkg-path pkg) "" pkg))
                       (tgt (if-let ((t (assoc-val :tgt lbl)))
                                    (format #f "~A:~A" pkg t)
                                    (error 'fixme "bash tool has :tgts"))))
                  (format #t "~A: ~A~%" (yellow "RESOLVED") tgt)
                  tgt)
                tool))))))

(define (-expand-bash-arg arg pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-expand-bash-arg") arg)
  (format #t "~A: ~A~%" (white "pkg-path") pkg-path)
  (format #t "~A: ~A~%" (white "stanza") stanza)
  (let* ((key (string->keyword arg)))
    ;; search :deps
    (let* ((deps (assoc-val :deps stanza))
           ;; (_ (format #t "~A: ~A~%" (yellow "deps") deps))
           (match (find-if (lambda (dep)
                           ;; (format #t "~A: ~A~%" (yellow "dep") dep)
                           (eq? key (car dep)))
                         deps)))
      (format #t "~A: ~A~%" (yellow "match") match)
      (if match
          (let* ((lbl (cdr match))
                 (pkg (assoc-val :pkg lbl))
                 ;; (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                 (pkg (if (equal? pkg-path pkg) "" pkg))
                 (tgt (if-let ((t (assoc-val :tgt lbl)))
                              (format #f "$(location ~A:~A)" pkg t)
                              (if-let ((t (assoc-val :tgts lbl)))
                                      (format #f "$(locations ~A:~A)" pkg t)
                                      (error 'fixme "lbl missing tgt/tgs")))))
            (format #t "~A: ~A~%" (yellow "RESOLVED") tgt)
            tgt)
          arg))))

;; handle all pct-vars: %{deps}, %{target}, etc.
;; also filename literals
(define (-expand-bash-args args pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-expand-bash-args") args)
  ;; args is either a string (the tool), a list of one string, or list of strings & syms
  ;; (bash "...cmd...")
  ;; or (run bash "foo.sh" bar ...)
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
      (format #t "~A: ~A~%" (red "arg-list") arg-list)

      ;; FIXME: treat first arg as tool

      (let* ((expanded-tool
             (-expand-bash-tool (car arg-list) pkg-path stanza))
            (_ (format #t "~A: ~A~%" (yellow "expanded TOOL") expanded-tool))
            (expanded-args
             (map (lambda (arg)
                    (format #t "~%~A: ~A (~A)~%" (red "expanding ARG") arg (type-of arg))
                    (let* ((arg (format #f "~A" arg))
                           (match-ct 2)
                           (res (regexec rgx arg match-ct 0))
                           (_ (format #t "~A: ~A~%"
                                      (red "regex result") res)))
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
                  arg-list)))
        (regfree rgx)
        (let* ((expanded-args
                (if-let ((stdout (assoc-in '(:actions :stdout) stanza)))
                        (begin
                          (format #t "~A: ~A~%" (yellow "stdout") stdout)
                          (append expanded-args (list "> $@")))
                        (let ((outs (-expand-outputs pkg-path stanza)))
                          (append expanded-args (list (format #f "cp ~A $@" outs)))))))
          (format #t "~A: ~A~%" (red "EXPANDED ARGS") expanded-args)
          (values expanded-tool expanded-args))))))

(define (-emit-bash-cmd outp pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-emit-bash-cmd") stanza)
  (let* ((args (assoc-in '(:actions :cmd :args) stanza))
         (args (cdr args))
         (_ (format #t "~A: ~A~%" (yellow "bash args") args))
         )
    (let-values (((tool parsed-args)
                  (-expand-bash-args args pkg-path stanza)))
      (format #t "~A: ~A~%" (yellow "tool") tool)
      (format #t "~A: ~S~%" (yellow "parsed args") parsed-args)

      (format outp "    cmd_bash   = \" \".join([\n")
      (format outp "~{        ~S~^,~%~}~%" parsed-args)
      ;; (if stdout
      ;;     (format outp "        \"> $@\",\n"))
      (format outp "    ]),\n")
      (if tool
          (format outp "    tools   = [~S]\n" tool))
      )))

(define (-emit-bash-srcs outp srcs pkg-path stanza)
  (format #t "~A: ~A~%" (blue "-emit-bash-srcs") srcs)
  ;; remove bash tool from srcs

  (let* ((args (assoc-in '(:actions :cmd :args) stanza))
         (args (cadr args)))
    (let-values (((tool parsed-args)
                  (-expand-bash-args args pkg-path stanza)))
      (format #t "~A: ~A~%" (yellow "TOOL") tool)
      (let ((srcs (remove tool srcs)))

        (format #t "~A: ~A~%" (red "srcs") srcs)
        (format outp "    srcs  = [\n")
        (format outp "~{        \"~A\"~^,\n~}\n" srcs)
        (format outp "    ],\n")))))

