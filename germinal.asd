(in-package :cl-user)
(defpackage germinal-asd
  (:use :cl :asdf))
(in-package :germinal-asd)

(defsystem germinal
  :version "1.0.0"
  :author "Jason McBrayer"
  :license "AGPL3"
  :depends-on (
               :alexandria
               :babel
               :usocket
               :usocket-server
               :bordeaux-threads
               :cl+ssl
               :str
               :trivial-mimes
               :ppath
               :osicat
               :cl-interpol
               :quri
               :cl-ppcre
               :cl-fad
               :local-time
               )
  :components ((:file "package")
               (:file "server")
               (:file "middleware")
               (:file "util")
               (:file "classes")
               )
  :description "A Gemini protocol server."
  )
