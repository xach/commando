;;;; commando.asd

(asdf:defsystem #:commando
  :serial t
  :depends-on (#:sb-posix
               #:alexandria)
  :components ((:file "package")
               (:file "commando")))

