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
(defvar *germinal-pathname-blacklist* '(".git")
  "List of files and directories to exclude. 
   Relative files/directories are excluded in all subdirectories of *germinal-root*.
   Absolute paths are excluded at exactly this path.")
(defvar *germinal-middleware* '(basic-logging))

(defvar *germinal-tls-context* nil "Variable used to store global TLS context")

(defvar *germinal-routes*
  '(("/hello/(.*)/?" . hello-world-view)
    ("/hello/?" . hello-world-view)
    (".*" . gemini-serve-file-or-directory))
  "Alist associating regular expressions to match paths against with functions
  to call to handle them. Routes are matched in order, so put the most specific
  routes at the top, and the least-specific at the bottom. Each function must
  take a request object as its first argument and return a response object. Make
  sure you refer to the function in such a way that it can be found from the
  package you call `germinal-start' from.")

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
  (write-line #?";; Germinal listening on ${host} port ${port}")
  (force-output)
  (setq *germinal-tls-context*
        (make-context :disabled-protocols
                      (list +ssl-op-no-sslv2+ +ssl-op-no-sslv3+
                            +ssl-op-no-tlsv1+ +ssl-op-no-tlsv1-1+
                            +ssl-op-no-tlsv1-2+)))
  (with-global-context (*germinal-tls-context* :auto-free-p (not background))
     (usocket:socket-server host port #'gemini-handler ()
                           :multi-threading t
                           :element-type '(unsigned-byte 8)
                           :in-new-thread background)))


;;; Internal functions
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
        when (scan (car route) (uri-path (request-url request)))
          return (symbol-function(cdr route))))

(defun serve-route (request)
  "Take a request object as argument, and apply the function for handling the
route to the request and any positional args from the route."
  (loop for route in *germinal-routes*
        when (scan (car route) (uri-path (request-url request)))
          return (apply (symbol-function (car (cdr route)))
                        request
                        (route-args route request))))

(defun route-args (route request)
  (vector-to-list (nth 1 (multiple-value-list
                          (scan-to-strings (car route)
                                           (uri-path (request-url request)))))))

(defun serve-route-with-middleware (request)
  "Take a request object as argument, and apply the chain of handlers in
*germinal-middleware* in order, with serve-route as the last handler."
  (funcall (middleware-chain *germinal-middleware*) request))

(defun gemini-handler (stream)
  "The main Gemini request handler. Sets up TLS and sets up request and response"
  (handler-case
      (let* ((tls-stream (make-ssl-server-stream stream
                                                 :certificate *germinal-cert*
                                                 :key *germinal-cert-key*))
             (request (make-request (normalize (read-line-crlf tls-stream) :nfc)
                                    (cl+ssl:ssl-stream-x509-certificate tls-stream)
                                    usocket:*remote-host*))
             (response (serve-route-with-middleware request)))
        (write-response response tls-stream)
        (close tls-stream))
    (error (c) (format *error-output* "gemini-handler error: ~A~%" c))))

(defun hello-world-view (request &optional name &rest junk)
  "A `Hello World' view function."
  (let ((name (if (str:emptyp name) "World" name)))
    (make-response 20 "text/gemini" (str:concat #?"# Hello, $(name)!" (string #\Newline)))))

(defun path-blacklisted-p (path &optional (blacklist *germinal-pathname-blacklist*))
  "Return t if the path matches something in the pathname blacklist."
   (loop for pattern in blacklist
         when (path-components-contain-p path pattern)
           return t))

(defun path-components-contain-p (path pattern)
  (cond
    ((cl-fad:pathname-root-p path) nil)
    ((pathname-match-p (cl-fad:pathname-as-file path) pattern) t)
    (t (path-components-contain-p (cl-fad:pathname-parent-directory
                           (cl-fad:pathname-as-directory path)) pattern))
  ))

(defun gemini-serve-file-or-directory (request &rest junk)
  "Given a gemini request (string), try to respond by serving a file or directory listing."
  (declare (ignore junk))
  (handler-case 
      (let* ((path (get-path-for-url (request-url request)))
             (path-kind (osicat:file-kind path :follow-symlinks t)))
        (if (or (not (member :other-read (osicat:file-permissions path)))
                (path-blacklisted-p path)
                (not (str:starts-with-p *germinal-root* path)))
            (make-response 51 "Not Found") ;; In lieu of a permission-denied status
            (cond
              ((eq :directory path-kind) (gemini-serve-directory path))
              ((eq :regular-file path-kind) (gemini-serve-file path))
              (t (make-response 51 "Not Found")))))
    (osicat-posix:enoent () (make-response 51 "Not Found"))
    (gemini-error (err) (make-response (gemini-error-type err)
                                       (gemini-error-message err)))
    (error (c)
      (format *error-output* "gemini-serve-file-or-directory error: ~A~%" c)
      (make-response 40 "Internal server error"))))

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
                              (remove-if #'path-blacklisted-p 
                                         (remove-if-not #'cl-fad:directory-exists-p
                                                        (cl-fad:list-directory path)))))
         (files (map 'list #'linkify
                     (remove-if #'path-blacklisted-p 
                                (remove-if #'cl-fad:directory-exists-p
                                           (cl-fad:list-directory path)))
                      ))
         (body (make-string-output-stream)))
    (write-sequence #?"# Directory listing for ${(de-prefix path)}/\n\n"
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

(defun url-encode-path (path)
  "Url-encode a path, leaving slashes intact."
  (let* ((dir-parts (pathname-directory path))
         (abs-or-rel (car dir-parts))
         (path-components (append
                           (cdr dir-parts)
                           (list (file-namestring path))))
         (url-encoded-path-components (mapcar #'url-encode path-components))
         (encoded-path (str:join "/" url-encoded-path-components)))
    (if (eq :absolute abs-or-rel)
        (str:concat "/" encoded-path)
        encoded-path)))

(defun linkify (path &optional text)
  "Format a path name with optional description as a gemini link"
  (let* ((path-name (de-prefix (namestring path)))
         (encoded-path-name (url-encode-path path-name))
         (file-size (file-size-human-readable
                     (file-size-in-octets path))))
    (if text
        #?"=> $(encoded-path-name)	$(text) ($(file-size))"
        #?"=> $(encoded-path-name)  $(path-name) ($(file-size))")))

