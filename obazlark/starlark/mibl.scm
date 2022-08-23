;; starlark/mibl.scm

;; routines for converting mibl model to starlark model

(define mibl->starlark
  (let ((+documentation+ "convert mibl data model to starlark data model")
        (+signature+ '(mibl-starlark pkgs-list)))
    (lambda (pkgs)
      (let ((slark
             (map (lambda (pkg)
                    (format #t "~A" (assoc-val :pkg-path pkg)))
                  pkgs)))))))

