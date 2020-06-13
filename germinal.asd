(in-package :cl-user)
(defpackage germinal-asd
  (:use :cl :asdf))
(in-package :germinal-asd)

(defsystem germinal
  :version "0.2.0"
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
               :ppath
               :osicat
               :cl-interpol
               :unix-opts
               :quri
               :cl-toml
               )
  :components (
               (:file "server")
               )
  :description "A Gemini protocol server."
  :build-pathname "germinal"
  :entry-point "germinal:start-cli"

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  )
