;; starlark/mibl.scm

;; routines for converting mibl model to starlark model

(define mibl->starlark
  (let ((+documentation+ "convert mibl data model to starlark data model")
        (+signature+ '(mibl-starlark pkgs-list)))
    (if *mibl-debugging*
        (format #t "~A: ~A~%" (ublue "mibl->starlark")))
    (lambda (pkgs)
      (let ((slark
             (map (lambda (pkg)
                    (if *mibl-debugging*
                        (format #t "~A" (assoc-val :pkg-path pkg))))
                  pkgs)))))))
