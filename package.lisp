;;; Preamble
(in-package :cl-user)
(defpackage :germinal
  (:use :cl :cl+ssl :trivial-file-size)
  (:import-from :quri
                #:uri
                #:uri-scheme
                #:uri-host
                #:uri-path
                #:url-encode
                #:url-decode)
  (:import-from :cl-toml
                #:parse-file)
  (:import-from :ppath
                #:normpath
                #:join)
  (:export #:start
           #:start-cli))
