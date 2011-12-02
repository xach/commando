;;;; package.lisp

(defpackage #:commando
  (:use #:cl)
  (:shadowing-import-from #:sb-ext
                          #:run-program
                          #:process-exit-code
                          #:process-output)
  (:shadowing-import-from #:sb-ext
                          #:native-namestring)
  (:shadowing-import-from #:sb-posix
                          #:chdir)
  (:shadowing-import-from #:alexandria
                          #:flatten)
  (:export #:run
           #:with-run-output
           #:with-posix-cwd
           #:with-binary-run-output
           #:without-run-output
           #:run-output-lines
           #:*default-temporary-template*
           #:call-with-temporary-directory
           #:with-temporary-directory
           #:call-in-temporary-directory
           #:in-temporary-directory
           #:in-specific-temporary-directory
           #:copy-file))
