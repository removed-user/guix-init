  (package
    (inherit static-guile)
    (name (string-append (package-name static-guile) "-stripped"))
    (build-system trivial-build-system)
    (arguments
     ;; The end result should depend on nothing but itself.
     (list #:allowed-references '("out")
           #:modules '((guix build utils))
           #:builder
           #~(let ((version #$(version-major+minor (package-version static-guile))))
               (use-modules (guix build utils))

               (let* ((in     #$static-guile)
                      (out    #$output)
                      (guile1 (string-append in "/bin/guile"))
                      (guile2 (string-append out "/bin/guile")))
                 (mkdir-p (string-append out "/share/guile/" version))
                 (copy-recursively (string-append in "/share/guile/" version)
                                   (string-append out "/share/guile/" version))

                 (mkdir-p (string-append out "/lib/guile/" version "/ccache"))
                 (copy-recursively (string-append in "/lib/guile/" version "/ccache")
                                   (string-append out "/lib/guile/" version "/ccache"))

                 (mkdir (string-append out "/bin"))
                 (copy-file guile1 guile2)

                 ;; Optionally remove additional directories.
                 (for-each (lambda (directory)
                             (delete-file-recursively
                              (string-append out "/" directory)))
                           '#$directories-to-remove)

                 ;; Verify that the relocated Guile works.
                 #$@(if (%current-target-system)
                        '()
                        '((invoke guile2 "--version")))

                 ;; Strip store references.
                 (remove-store-references guile2)

                 ;; Verify that the stripped Guile works.  If it aborts, it could be
                 ;; that it tries to open iconv descriptors and fails because libc's
                 ;; iconv data isn't available (see `guile-default-utf8.patch'.)
                 #$@(if (%current-target-system)
                        '()
                        '((invoke guile2 "--version")))))))
    (outputs '("out"))
    (synopsis "Minimal statically-linked and relocatable Guile")))
