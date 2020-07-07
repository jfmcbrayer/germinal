(in-package :germinal)

(defclass request ()
  ((url :initarg :url :accessor url)
   (path-info :initarg :path-info :accessor path-info)
   (params :initarg :params :accessor params)
   (client-key :initarg :client-key :accessor client-key)
   (client-address :initarg :cliient-address :accessor client-address)))

(defclass response ()
  ((status :initarg :status :accessor status)
   (meta :initarg :meta :accessor meta)
   (body :initarg :body :accessor body :initform "")))

(defun make-request (url &optional client-key client-address)
  (let* ((parsed-url (uri url))
         (path-info (uri-path parsed-url))
         (params (quri:uri-query-params parsed-url))
         )
    (make-instance 'request :url parsed-url :path-info path-info :params params
                            :client-key client-key :client-address client-address)))

(defun make-response (status meta &optional body)
  (make-instance 'response :status (write-to-string status) :meta meta :body body))

(defmethod print-response ((stream stream) (response response))
  (write-sequence (babel:string-to-octets
                   (str:concat (status response) " " (meta response) '(#\return #\newline)))
                  stream))
