(in-package :cl-user)
(defpackage germinal-asd
  (:use :cl :asdf))
(in-package :germinal-asd)

(defsystem germinal
  :version "0.1"
  :author "Jason McBrayer"
  :license "AGPL3"
  :depends-on (
               :babel
               :usocket
               :usocket-server
               :bordeaux-threads
               :cl+ssl
               :str
               :trivial-mimes
               :osicat
               :cl-interpol
               )
  :components (
               (:file "server")
               )
  :description "A Gemini protocol server."
  )
