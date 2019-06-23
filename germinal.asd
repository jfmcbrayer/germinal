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
               :cl+ssl
               )
  :components (
               (:file "server")
               )
  :description "A Gemini protocol server."
  )
