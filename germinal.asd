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
               :trivial-file-size
               :ppath
               :osicat
               :cl-interpol
               :unix-opts
               :quri
               :cl-toml
               :cl-ppcre
               )
  :components ((:file "package")
               (:file "server")
               (:file "util")
               (:file "classes")
               )
  :description "A Gemini protocol server."
  :build-operation "program-op"
  :build-pathname "germinal"
  :entry-point "germinal:start-cli"
  )
