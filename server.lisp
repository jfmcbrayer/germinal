;;;; The Germinal server. This is pretty much self-contained.

;;; Package-level things
(in-package :germinal)
(interpol:enable-interpol-syntax)

(defvar *germinal-server-name* "localhost")
(defvar *germinal-root* "/var/gemini")
(defvar *germinal-host* "0.0.0.0")
(defvar *germinal-port* 1965)
(defvar *germinal-cert* "/etc/germinal/cert.pem")
(defvar *germinal-cert-key* "/etc/germinal/key.pem")
(defvar *germinal-config-file* "/etc/germinal/config.toml")
(defvar *germinal-pathname-blacklist* '(".git" ".git/"))

(defvar *germinal-tls-context* nil "Variable used to store global TLS context")

(defvar *germinal-routes*
  '(("/hello/?.*" . hello-world-view)
    (".*" . gemini-serve-file-or-directory))
  "Alist associating regular expressions to match paths against with functions
  to call to handle them. Routes are matched in order, so put the most specific
  routes at the top, and the least-specific at the bottom. Each function must
  take a request object as its first argument and return a response object. Make
  sure you refer to the function in such a way that it can be found from the
  package you call `germinal-start' from.")

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")
  (:name :config
   :description "Path to the configuration file to use. Default /etc/germinal/config.toml"
   :short #\f
   :long "config"
   :arg-parser #'identity)
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
   :arg-parser #'identity)
  (:name :cert
   :description "Path to the TLS server certificate to use"
   :short #\c
   :long "cert"
   :arg-parser #'identity)
  (:name :key
   :description "Path to the private key file for the TLS server certificate"
   :short #\k
   :long "key"
   :arg-parser #'identity))

(define-condition gemini-error (error)
  ((error-type :initarg :error-type
               :reader gemini-error-type)
   (error-message :initarg :error-message
                  :reader gemini-error-message)))

;;; Entry functions

(defun start (&key (host *germinal-host*) (port *germinal-port*) (background nil))
  "Start the germinal server, listening to HOST and PORT."
  ;; update mime types
  (setf (gethash "org" mimes:*mime-db*) "text/org-mode")
  (setf (gethash "gmi" mimes:*mime-db*) "text/gemini")
  (write-line #?"Listening on ${host} port ${port}")
  (force-output)
  (setq *germinal-tls-context* (make-context :disabled-protocols
                                              (list +ssl-op-no-sslv2+ +ssl-op-no-sslv3+
                                                    +ssl-op-no-tlsv1+ +ssl-op-no-tlsv1-1+
                                                    +ssl-op-no-tlsv1-2+)))
  (with-global-context (*germinal-tls-context* :auto-free-p (not background))
     (usocket:socket-server host port #'gemini-handler ()
                           :multi-threading t
                           :element-type '(unsigned-byte 8)
                           :in-new-thread background)))

(defun start-cli ()
  "Start the germinal server, taking config from the environment or command-line."
  (if (getf (opts:get-opts) :help)
      (progn
        (opts:describe
         :prefix "Germinal, a gemini server.  Usage:"
         :args "[keywords]")
        (opts:exit)))
  (get-config-file)
  (get-config-env)
  (get-config-args)
  (start))

;;; Internal functions
(defun get-config-env ()
  "Get the configuration from the environment"
  (let ((germinal-root (osicat:environment-variable "GERMINAL_ROOT"))
        (germinal-host (osicat:environment-variable "GERMINAL_HOST"))
        (germinal-port (osicat:environment-variable "GERMINAL_PORT"))
        (germinal-cert (osicat:environment-variable "GERMINAL_CERT"))
        (germinal-cert-key (osicat:environment-variable "GERMINAL_CERT_KEY"))
        (germinal-config-file (osicat:environment-variable "GERMINAL_CONFIG")))
    (if germinal-root (setq *germinal-root* germinal-root))
    (if germinal-host (setq *germinal-host* germinal-host))
    (if germinal-port (setq *germinal-port* (parse-integer germinal-port)))
    (if germinal-cert (setq *germinal-cert* germinal-cert))
    (if germinal-cert-key (setq *germinal-cert-key* germinal-cert-key))
    (if germinal-config-file (setq *germinal-config-file* germinal-config-file))))

(defun get-config-args ()
  "Get the configuration from the command-line"
  (multiple-value-bind (options free-args)
      (handler-case (opts:get-opts)
        (error ()
          (opts:describe)
          (opts:exit)))
    (if (getf options :root )
        (setq *germinal-root* (getf options :root)))
    (if (getf options :host)
        (setq *germinal-host* (getf options :host)))
    (if (getf options :port)
        (setq *germinal-port* (getf options :port)))
    (if (getf options :cert)
        (setq *germinal-cert* (getf options :cert)))
    (if (getf options :key)
        (setq *germinal-cert-key* (getf options :key)))
    (if (getf options :config)
        (setq *germinal-config-file* (getf options :config)))))

(defun get-config-file-path ()
  "Use command-line, environment, or default to find config file"
  (let ((env-config (osicat:environment-variable "GERMINAL_CONFIG"))
        (opts-config (handler-case
                         (getf (opts:get-opts) :config)
                       (error () nil))))
    (cond
      (opts-config opts-config)
      (env-config env-config)
      (t *germinal-config-file*))))

(defun get-config-file ()
  "Set config vars based on contents of config file"
  (let* ((config (cl-toml:parse-file (get-config-file-path)))
         (core (gethash "core" config)))
    (when core
      (when (gethash "server-name" core) (setq *germinal-server-name*
                                               (gethash "server-name" core)))
      (when (gethash "root" core) (setq *germinal-root* (gethash "root" core)))
      (when (gethash "host" core) (setq *germinal-host*
                                        (gethash "host" core)))
      (when (gethash "port" core) (setq *germinal-port* (gethash "port" core)))
      (when (gethash "cert" core) (setq *germinal-cert*
                                        (gethash "cert" core)))
      (when (gethash "key" core) (setq *germinal-cert-key*
                                        (gethash "key" core))))
    config))

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

(defun resolve-route (request)
  "Take a request object as argument, and return the function for handling the
route."
  (loop for route in *germinal-routes*
        when (scan (car route) (uri-path (url request)))
          return (symbol-function(cdr route))))

(defun gemini-handler (stream)
  "The main Gemini request handler. Sets up TLS and sets up request and response"
  (handler-case
      (let* ((tls-stream (make-ssl-server-stream stream
                                                 :certificate *germinal-cert*
                                                 :key *germinal-cert-key*))
             (request (make-request (read-line-crlf tls-stream)))
             (response (funcall (resolve-route request) request)))
        (write-response response tls-stream)
        (close tls-stream))
    (error (c) (format *error-output* "gemini-handler error: ~A~%" c))))

(defun hello-world-view (request)
  "A `Hello World' view function."
  (make-response 20 "text/plain" (str:concat "# Hello, world!" '(#\newline))))

(defun gemini-serve-file-or-directory (request)
  "Given a gemini request (string), try to respond by serving a file or directory listing."
  (handler-case 
      (let* ((path (get-path-for-url (url request)))
             (path-kind (osicat:file-kind path :follow-symlinks t)))
        (if (or (not (member :other-read (osicat:file-permissions path)))
                (member (pathname-name path) *germinal-pathname-blacklist*
                        :test #'string-equal)
                (not (string-starts-with-p path *germinal-root*)))
            (make-response 51 "Not Found") ;; In lieu of a permission-denied status
            (cond
              ((eq :directory path-kind) (gemini-serve-directory path))
              ((eq :regular-file path-kind) (gemini-serve-file path))
              (t (make-response 51 "Not Found")))))
    (osicat-posix:enoent () (make-response 51 "Not Found"))
    (gemini-error (err) (make-response (gemini-error-type err)
                                       (gemini-error-message err)))
    (error () (make-response 40 "Internal server error"))))

(defun get-path-for-url (url)
  "Get file path based on URL (a quri object)"
  (if (uri-userinfo url)
        (error 'gemini-error :error-type 59 :error-message "Bad Request"))
    (normpath (join *germinal-root*
                    (string-left-trim "/" (url-decode (uri-path url))))))

(defun gemini-serve-file (path)
  "Given an accessible file path, serve it as a gemini response"
  (let* ((mime-type (mimes:mime path))
         (body (alexandria:read-file-into-byte-vector path)))
    (make-response 20 mime-type body)))

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
    (make-response 20 "text/gemini"
                   (babel:string-to-octets (get-output-stream-string body)
                                           :encoding :utf-8))))

(defun linkify (path &optional text)
  "Format a path name with optional description as a gemini link"
  (let* ((path-name (de-prefix(namestring path)))
         (encoded-path-name (url-encode path-name))
         (file-size (file-size-human-readable
                     (file-size-in-octets path))))
    (if text
        #?"=> $(encoded-path-name)	$(text) ($(file-size))"
        #?"=> $(encoded-path-name)  $(path-name) ($(file-size))")))

