
(define (deps->srcs-attr pkg-path deps)
  (format #t "~A: ~A\n" (blue "deps->srcs-attr") deps)
  ;; deps is a list of alists; key :maps to list of (:pkg :file) pairs
  ;; key :_ (anonymous) may map to multiple pairs
  ;; other keys are dune 'variables', each mapping to one (:pkg :file) pair

  (if deps
      (let* ((srcs (map (lambda (tagged-label)
                          (format #t "tagged-label: ~A~%" tagged-label)
                          (let* ((tag (car tagged-label))
                                 (_ (format #t "~A: ~A~%" (yellow "tag") tag))
                                 (label (cdr tagged-label))
                                 (_ (format #t "~A: ~A~%" (yellow "label") label))
                                 (pkg (cdr (assoc :pkg label)))
                                 (_ (format #t "~A: ~A~%" (yellow "pkg") pkg))
                                 (tgt (cadr label))
                                 (_ (format #t "~A: ~A~%" (yellow "tgt") tgt))
                                 (tgt-tag (car tgt))
                                 (_ (format #t "~A: ~A~%" (yellow "tgt-tag") tgt-tag))
                                 (tgt (cdr tgt))
                                 (_ (format #t "~A: ~A~%" (yellow "tgt") tgt)))
                            (case tgt-tag
                              ((:tgt)
                               (if (equal? pkg-path pkg)
                                   (format #f ":~A" tgt)
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
                               (error 'fixme "unrecognized target value")))))
                        deps))
             ;; srcs list may contain mix of strings and sublists
             (srcs (fold (lambda (src accum)
                           (if (string? src) (append accum (list src))
                               (append accum src)))
                         '() srcs)))
        (format #t "SRCS ATTR: ~A\n" srcs)
        srcs)
      #f))
