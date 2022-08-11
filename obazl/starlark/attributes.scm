(define (-outputs->outs-attr pkg-path outputs)
  (format #t "~A: ~A\n" (blue "-outputs->outs-attr") outputs)
  (let* ((outs (map (lambda (out-tlbl)
                      (format #t "out-tlbl: ~A\n" out-tlbl)
                      (let* ((tag (car out-tlbl))
                             ;; (_ (format #t "~A: ~A~%" (yellow "tag") tag))
                             (label (cdr out-tlbl))
                             ;; (_ (format #t "~A: ~A~%" (yellow "label") label))
                             (pkg (assoc-val :pkg label))
                             ;; (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                             (tgt (assoc-val :tgt label))
                             ;; (_ (format #t "~A: ~A~%" (yellow "tgt") tgt))
                             )
                        (if (equal? pkg pkg-path)
                            tgt
                            ;; should not happen?
                            (format #f "~A:~A" pkg tgt))))
                        outputs))
         (outs (fold (lambda (src accum)
                       (if (string? src) (append accum (list src))
                           (append accum src)))
                     '() outs)))
    (format #t "OUTPUTS: ~A\n" outs)
    outs))

(define (deps->srcs-attr pkg-path deps)
  (format #t "~A: ~A\n" (ublue "deps->srcs-attr") deps)
  ;; deps is a list of alists; key :maps to list of (:pkg :file) pairs
  ;; key :_ (anonymous) may map to multiple pairs
  ;; other keys are dune 'variables', each mapping to one (:pkg :file) pair

  (if deps
      (let* ((srcs (map (lambda (dep)
                          (format #t "dep: ~A~%" dep)
                          (if (eq? ::tools (car dep))
                              (begin) ;; skip, it goes in tools attr
                              (let* ((tag (car dep))
                                     (_ (format #t "~A: ~A~%" (yellow "tag") tag))
                                     (label (cdr dep))
                                     (_ (format #t "~A: ~A~%" (yellow "label") label)))
                                (if (alist? label)
                                    (let* ((pkg (cdr (assoc :pkg label)))
                                           (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                                           (tgt (cadr label))
                                           (_ (format #t "~A: ~A~%" (yellow "tgt") tgt))
                                           (tgt-tag (car tgt))
                                           (_ (format #t "~A: ~A~%" (yellow "tgt-tag") tgt-tag))
                                           (tgt (cdr tgt))
                                           (_ (format #t "~A: ~A~%" (yellow "tgt") tgt))
                                           )
                                      (case tgt-tag
                                        ((:tgt)
                                         (if (equal? pkg-path pkg)
                                             (format #f "~A" tgt)
                                             (format #f "//~A:~A" pkg tgt)))

                                        ((:tgts)
                                         (if (equal? pkg-path pkg)
                                             (format #f "~A" tgt)
                                             (format #f "//~A~A" pkg tgt)))

                                        ((:fg)
                                         (if (equal? pkg-path pkg)
                                             (format #f "~A" tgt)
                                             (format #f "//~A:*~A*" pkg tgt)))

                                        (else
                                         (error 'fixme "unrecognized target value"))))
                                    ;; else not a (:pkg ) (:tgt ) dep
                                    (begin
                                      (case (car label)
                                        ((::import)
                                         (format #t "~A: ~A~%" (uyellow "::import") tag)
                                         (format #f "~A" tag))
                                        ((::pkg)
                                         (format #t "~A: ~A~%" (uyellow "::pkg") tag)
                                         (format #f "~A" tag))
                                        (else
                                         (error 'fixme "XXXXXXXXXXXXXXXX"))))
                                      )
                                    )))
                        deps))
             (_ (format #t "~A: ~A~%" (uwhite "prelim srcs") srcs))
             ;; srcs list may contain mix of strings and sublists
             (srcs (fold (lambda (src accum)
                           (cond
                            ((string? src) (append accum (list src)))
                            ((symbol? src) (append accum (list src)))
                            (else (append accum src))))
                         '() srcs)))
        (format #t "SRCS ATTR: ~A\n" srcs)
        srcs)
      #f))
