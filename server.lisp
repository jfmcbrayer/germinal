;;;; The Germinal server. This is pretty much self-contained.

;;; Preamble
(in-package :cl-user)
(defpackage :germinal
  (:use :cl :cl+ssl)
  (:export #:start
           #:start-cli))

;;; Package-level things
(in-package :germinal)
(interpol:enable-interpol-syntax)

(defvar *germinal-root* "/var/gemini")
(defvar *germinal-host* "0.0.0.0")
(defvar *germinal-port* 1965)

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")
  (:name :root
   :description "Path to the root of the directory tree to serve. Default /var/gemini."
   :short #\r
   :long "root"
   :arg-parser #'identity)
  (:name :port
   :description "Port number to listen on. Default 1965."
   :short #\p
   :long "port"
   :arg-parser #'parse-integer) ;; <- takes an argument
  (:name :host
   :description "Hostname or IP address to bind to. Default 0.0.0.0"
   :short #\i
   :long "host"
   :arg-parser #'identity))

;;; Entry functions

(defun start (&key (host *germinal-host*) (port *germinal-port*))
  "Start the germinal server, listening to HOST and PORT."
  ;; update mime types
  (setf (gethash "org" mimes:*mime-db*) "text/org-mode")
  (setf (gethash "gmi" mimes:*mime-db*) "text/gemini")
  (write-line #?"Listening on ${host} port ${port}")
  (force-output)
  (usocket:socket-server host port #'gemini-handler ()
                         :multi-threading t
                         :element-type '(unsigned-byte 8)))

(defun start-cli ()
  "Start the germinal server, taking config from the environment or command-line."
  (get-config-env)
  (get-config-args)
  (start))

;;; Internal functions
(defun get-config-env ()
  "Get the configuration from the environment"
  (let ((germinal-root (osicat:environment-variable "GERMINAL_ROOT"))
        (germinal-host (osicat:environment-variable "GERMINAL_HOST"))
        (germinal-port (osicat:environment-variable "GERMINAL_PORT")))
    (if germinal-root (setq *germinal-root* germinal-root))
    (if germinal-host (setq *germinal-host* germinal-host))
    (if germinal-port (setq *germinal-port* (parse-integer germinal-port)))))

(defun get-config-args ()
  "Get the configuration from the command-line"
  (multiple-value-bind (options free-args)
      (handler-case (opts:get-opts)
        (error ()
          (opts:describe)
          (opts:exit)))
    (if (getf options :help)
              (progn
                (opts:describe
                 :prefix "Germinal, a gemini server.  Usage:"
                 :args "[keywords]")
                (opts:exit)))
    (if (getf options :root )
        (setq *germinal-root* (getf options :root)))
    (if (getf options :host)
        (setq *germinal-host* (getf options :host)))
    (if (getf options :port)
        (setq *germinal-port* (getf options :port)))))

(defun read-line-crlf (stream &optional eof-error-p)
  "Read a CRLF-terminated line from a binary stream and return a string"
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
  "The main Gemini request handler. Sets up TLS and sets up request and response"
  (let* ((tls-stream
           (make-ssl-server-stream stream
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
  "Given a gemini request (string), try to respond by serving a file or directory listing."
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
  "Given an accessible file path, serve it as a gemini response"
  (let* ((mime-type (mimes:mime path))
         (status (str:concat "2	" mime-type))
         (body (alexandria:read-file-into-byte-vector path)))
    (list status body)))

(defun gemini-serve-directory (path)
  "Given an accessible directory, serve either an index.gmi file or a directory listing as
a gemini response"
   (if (probe-file (str:concat path "/index.gmi"))
     (gemini-serve-file (str:concat path "/index.gmi"))
     (gemini-generate-directory-list path)))

(defun gemini-generate-directory-list (path)
  "Given an accessible directory path, generate a directory listing and serve it as a gemini response"
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
  "Format a path name with optional description as a gemini link"
  (let ((path-name (de-prefix(namestring path))))
    (if text
        #?"=> $(path-name)	$(text)"
        #?"=> $(path-name)"
        )))

(defun de-prefix (path &optional (prefix *germinal-root*))
  "Strip *germinal-root* from a pathname"
  (str:replace-all prefix "" path))
