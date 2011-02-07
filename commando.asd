;;;; commando.asd

(asdf:defsystem #:commando
  :serial t
  :depends-on (#:sb-posix)
  :components ((:file "package")
               (:file "commando")))

