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
  (:import-from :str
                #:split
                #:join
                #:concat
                #:starts-with-p)
  (:import-from :babel
                #:octets-to-string
                #:string-to-octets)
  (:import-from :mimes
                #:*mime-db*
                #:mime)
  (:export #:start)
  )

(in-package :germinal)

;; Initially define an echo server so I can learn to do this.
(defun echo-handler (stream)
  (handler-case
      (loop
        (when (listen stream)
          (let ((line (read-line stream nil)))
            (write-line line stream)
            (force-output stream))))
    (error ()
      (close stream))))

(defun tls-echo-handler (stream)
  (echo-handler (cl+ssl:make-ssl-server-stream stream
                                               :external-format '(:utf-8)
                                               :certificate "cert.pem"
                                               :key "key.pem")))

(defun start (&key (host "127.0.0.1") (port 1965))
  (usocket:socket-server host port #'gemini-handler ()
                         :multi-threading t
                         :element-type '(unsigned-byte 8)))

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

(defun gemini-handler (stream)
  (let* ((tls-stream (cl+ssl:make-ssl-server-stream stream
                                                    :external-format '(:utf-8)
                                                    :certificate "cert.pem"
                                                    :key "key.pem"))
         (request (read-line-crlf tls-stream))
         (response (gemini-serve-file request)))
    (write-sequence (str:concat (nth 0 response) '(#\return #\newline))
                    tls-stream)
    (force-output tls-stream)
    (write-sequence (nth 1 response) tls-stream)
    (force-output tls-stream)))

(defun get-trivial-response-for-gemini-url (request)
  (let ((status "2	text/gemini; charset=utf-8")
        (body (str:concat "This is a gemini response for " request)))
    (list status body)))

(defun gemini-serve-file (request)
  (let* ((path (if (str:starts-with-p "/" request) (str:s-rest request) request))
         (path (str:replace-all "../" "" path)))
    (if (probe-file path)
        (let* ((mime-type (mimes:mime path))
               (status (str:concat "2	" mime-type))
               (body (alexandria:read-file-into-string path)))
          (list status body))
        (list (str:concat "4	Path not found " path) ""))))

