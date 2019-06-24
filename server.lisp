(in-package :cl-user)

(defpackage :germinal
  (:use :cl)
  (:import-from :cl+ssl
                #:make-context
                #:with-global-context
                #:make-ssl-server-stream
   )
  (:import-from :usocket
                #:socket-listen
                #:socket-accept
                #:socket-close
                #:socket-server)
  (:export #:start)
  )

(in-package :germinal)

;; Initially define an echo server so I can learn to do this.
(defun echo-handler (stream)
  (loop
    (when (listen stream)
      (let ((line (read-line stream nil)))
        (write-line line stream)
        (force-output stream)))))

(defun tls-echo-handler (stream)
  (echo-handler (cl+ssl:make-ssl-server-stream stream
                                               :external-format '(:utf-8 :eol-style :lf)
                                               :certificate "cert.pem"
                                               :key "key.pem")))

(defun start (&key (host "127.0.0.1") (port 1965))
  (usocket:socket-server host port #'tls-echo-handler))

(defun read-line-crlf (stream &optional eof-error-p)
  (let ((s (make-string-output-stream)))
    (loop
      for empty = t then nil
      for c = (read-char stream eof-error-p nil)
      while (and c (not (eql c #\return)))
      do
         (unless (eql c #\newline)
           (write-char c s))
      finally
         (return
           (if empty nil (get-output-stream-string s))))))
