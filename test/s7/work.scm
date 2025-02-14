


(for-each (lambda (ws)
            (let* ((ws-name (car ws))
                   (ws-mibl (cdr kv))
                  (pkg-path (car (assoc-val :pkg-path pkg)))
                  (outpath (string-append ws-root \"/\" pkg-path \"/DUNEFILE.mibl\")))
              (call-with-output-file outpath
                 (lambda (p)
                    (mibl-pretty-print pkg p)))))
          *mibl-project*)
