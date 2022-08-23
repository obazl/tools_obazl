(define (pkg-namespaced? dune-pkg-tbl)
  (any (lambda (s)
         (if (assoc-in '(:library :namespaced) (list s)) #t #f))
       (cadr (assoc :stanzas (cdr dune-pkg-tbl)))))


;; dune always makes an archive from 'library' stanza,
;; but may or may not namespace it, depending on 'unwrapped' fld.
(define (pkg-has-archive? dune-pkg-tbl)
  (assoc-in '(:stanzas :library) (cdr dune-pkg-tbl)))

;; unwrapped 'library' stanza
(define (pkg-has-library? dune-pkg-tbl)
  (if-let ((libs (assoc-in+ '(:stanzas :library) (cdr dune-pkg-tbl))))
          (any (lambda (lib)
                 (if-let ((wrapped (assoc 'wrapped (cdr lib))))
                         ;; FIXME: handle empty '(wrapped)'
                         (if (equal? 'false  (cadr wrapped))
                             #t
                             #f)
                         #f))
               libs)
          #f))

(define (pkg-has-signature? dune-pkg-tbl)
  (if-let ((srcs (assoc-in '(:srcfiles :ocaml :static) (cdr dune-pkg-tbl))))
          (any (lambda (srcfile)
                   (string-suffix? ".mli" srcfile))
                 (cadr srcs))
          #f))

(define (dunefile? f)
  (display (format #f "DUNEFILE? ~A" f)) (newline)
  (or (string=? "dune" f)
      (any (lambda (sfx) (string-suffix? sfx f))
            dunefile-ext-list)))

