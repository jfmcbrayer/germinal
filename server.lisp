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
(defvar *germinal-host* "0.0.0.0")
(defvar *germinal-port* 1965)

(defun start (&key (host *germinal-host*) (port *germinal-port*))
  ;; update mime types
  (setf (gethash "org" mimes:*mime-db*) "text/org-mode")
  (setf (gethash "gmi" mimes:*mime-db*) "text/gemini")
  (write-line #?"Listening on ${*germinal-host*} port ${*germinal-port*}")
  (force-output)
  (usocket:socket-server host port #'gemini-handler ()
                         :multi-threading t
                         :element-type '(unsigned-byte 8)))

(defun read-line-crlf (stream &optional eof-error-p)
  (let ((s (make-string-output-stream)))
    (loop
      for empty = t then nil
      for c = (read-byte stream eof-error-p nil)
      while (and c (not (eql (code-char c) #\return)))
      do
         (unless (eql (code-char c) #\newline)
           (write-char (code-char c) s))
      finally
         (return
           (if empty nil (get-output-stream-string s))))))

(defun gemini-handler (stream)
  (let* ((tls-stream
           (cl+ssl:make-ssl-server-stream stream
                                          :certificate "cert.pem"
                                          :key "key.pem"))
         (request (read-line-crlf tls-stream))
         (response (gemini-serve-file-or-directory request)))
    (write-sequence
     (babel:string-to-octets (str:concat (nth 0 response) '(#\return #\newline)))
                    tls-stream)
    (force-output tls-stream)
    (write-sequence (nth 1 response) tls-stream)
    (force-output tls-stream)))


(defun gemini-serve-file-or-directory (request)
  (handler-case 
      (let* ((path (if (str:starts-with-p "/" request)
                       (str:s-rest request)
                       request))
             (path (str:replace-all "../" "" path))
             (path (str:concat *germinal-root* "/" path))
             (path-kind (osicat:file-kind path :follow-symlinks t)))
        (if (not (member :other-read (osicat:file-permissions path)))
            (list "4	Not Found" "") ;; In lieu of a permission-denied status
            (cond
              ((eq :directory path-kind) (gemini-serve-directory path))
              ((eq :regular-file path-kind) (gemini-serve-file path))
              (t (list "4	Not Found" "")))))
    (osicat-posix:enoent () (list "4	Not Found" ""))))
    ;(error () (list "5	Internal server error" "Internal server error"))))

(defun gemini-serve-file (path)
  (list "2	text/plain" "Permission denied")
  (let* ((mime-type (mimes:mime path))
         (status (str:concat "2	" mime-type))
         (body (alexandria:read-file-into-byte-vector path)))
    (list status body)))

(defun gemini-serve-directory (path)
   (if (probe-file (str:concat path "/index.gmi"))
     (gemini-serve-file (str:concat path "/index.gmi"))
     (gemini-generate-directory-list path)))

(defun gemini-generate-directory-list (path)
  (let* ((subdirectories (map 'list #'linkify
                              (uiop:subdirectories (str:concat path "/"))))
         (files (map 'list #'linkify
                     (uiop:directory-files (str:concat path "/"))))
         (status "2	text/gemini")
         (body (make-string-output-stream)))
    (write-sequence #?"# Directory listing for ${(de-prefix path)}\n\n"
                    body)
    (write-sequence #?"## Subdirectories\n" body)
    (write-sequence
     (let ((cl-interpol:*list-delimiter* #\Newline))
       #?"@{subdirectories}\n\n")
     body)
    (write-sequence #?"## Files\n" body)
    (write-sequence
     (let ((cl-interpol:*list-delimiter* #\Newline))
       #?"@{files}\n\n")
     body)
    (list status (babel:string-to-octets (get-output-stream-string body)
                                         :encoding :utf-8))))

(defun linkify (path &optional text)
  (let ((path-name (de-prefix(namestring path))))
    (if text
        #?"[$(text)|$(path-name)]"
        #?"[$(path-name)|$(path-name)]"
        )))

(defun de-prefix (path &optional (prefix *germinal-root*))
  (str:replace-all prefix "" path))
