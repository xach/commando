;;;; commando.asd

(asdf:defsystem #:commando
  :serial t
  :author "Zach Beane <xach@xach.com>"
  :version "0.0.1"
  :description "A half-baked interface for a supportive shell command
  environment."
  :depends-on (#:sb-posix
               #:alexandria)
  :components ((:file "package")
               (:file "commando")))

