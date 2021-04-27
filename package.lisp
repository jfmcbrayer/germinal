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
  (:import-from :ppath
                #:normpath
                #:join)
  (:import-from :cl-ppcre
                #:scan
                #:scan-to-strings)
  (:import-from :uax-15
                #:normalize)
  (:export #:start
           #:start-cli
           #:make-request
           #:make-response
           #:path-blacklisted-p
           #:gemini-serve-file-or-directory
           #:*germinal-server-name*
           #:*germinal-root*
           #:*germinal-host*
           #:*germinal-port*
           #:*germinal-cert*
           #:*germinal-cert-key*
           #:*germinal-pathname-blacklist*
           #:*germinal-routes*
           #:*germinal-middleware*
           #:request-url
           #:request-pathinfo
           #:request-params
           #:request-client-key
           #:request-client-addres
           #:response-status
           #:response-meta
           #:response-body))
