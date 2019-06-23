(in-package :cl-user)

(defpackage :germinal
  (:use :cl)
  (:import-from :cl+ssl
                :make-context
                :with-global-context
                :make-ssl-server-stream
   )
  (:export :start
           :stop))

(in-package :germinal)

(defun start (&rest args &key host (port 1965))
  t)
