;;; Preamble
(in-package :cl-user)
(defpackage :germinal
  (:use :cl :cl+ssl)
  (:import-from :quri
                #:uri
                #:uri-scheme
                #:uri-host
                #:uri-path
                #:uri-userinfo
                #:url-encode
                #:url-decode)
  (:import-from :cl-toml
                #:parse-file)
  (:import-from :ppath
                #:normpath
                #:join)
  (:import-from :cl-ppcre
                #:scan
                #:scan-to-strings)
  (:export #:start
           #:start-cli
           #:make-request
           #:make-response
           #:*germinal-server-name*
           #:*germinal-root*
           #:*germinal-host*
           #:*germinal-port*
           #:*germinal-cert*
           #:*germinal-pathname-blacklist*
           #:*germinal-routes*))
