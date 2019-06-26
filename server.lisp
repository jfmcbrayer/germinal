(in-package :cl-user)

(defpackage :germinal
  (:use :cl)
  (:import-from :cl+ssl
                #:make-ssl-server-stream)
  (:import-from :usocket
                #:socket-server)
  (:import-from :str
                #:split
                #:join
                #:concat
                #:starts-with-p
                #:ends-with-p)
  (:import-from :babel
                #:octets-to-string
                #:string-to-octets)
  (:import-from :mimes
                #:*mime-db*
                #:mime)
  (:import-from :osicat
                #:file-permissions)
  (:export #:start)
  )

(in-package :germinal)
(interpol:enable-interpol-syntax)

(defvar *germinal-root* "/var/gemini")

(defun start (&key (host "127.0.0.1") (port 1965))
  ;; update mime types
  (setf (gethash "org" mimes:*mime-db*) "text/org-mode")
  (setf (gethash "gmi" mimes:*mime-db*) "text/gemini")
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
         (response (gemini-serve-file-or-directory request)))
    (write-sequence (str:concat (nth 0 response) '(#\return #\newline))
                    tls-stream)
    (force-output tls-stream)
    (write-sequence (nth 1 response) tls-stream)
    (force-output tls-stream)))


(defun gemini-serve-file-or-directory (request-path)
  (let* ((path (if (str:starts-with-p "/" request-path)
                   (str:s-rest request-path)
                   request-path))
         (path (str:replace-all "../" "" path))
         (path (str:concat *germinal-root* "/" path))
         (path-kind (osicat:file-kind path :follow-symlinks t)))
    (cond
      ((eq :directory path-kind) (gemini-serve-directory path) )
      ((eq :regular-file path-kind) (gemini-serve-file path))
      (t (list "4	Not Found" "")))))

(defun gemini-serve-file (path)
  (if (not (member :other-read (osicat:file-permissions path)))
      (list "2	text/plain" "Permission denied")
      (let* ((mime-type (mimes:mime path))
             (status (str:concat "2	" mime-type))
             (body (alexandria:read-file-into-string path)))
        (list status body))))

(defun gemini-serve-directory (path)
   (if (probe-file (str:concat path "index.gmi"))
     (gemini-serve-file (str:concat path "/index.gmi"))
     (gemini-generate-directory-list path)))

(defun gemini-generate-directory-list (path)
  (list "5	Not implemented" "Directory list: not implemented"))
