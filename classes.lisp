(in-package :germinal)
(interpol:enable-interpol-syntax)

(defclass request ()
  ((url :initarg :url :accessor request-url)
   (path-info :initarg :path-info :accessor request-pathinfo)
   (params :initarg :params :accessor request-params)
   (client-key :initarg :client-key :accessor request-client-key)
   (client-address :initarg :client-address :accessor request-client-address)))

(defclass response ()
  ((status :initarg :status :accessor response-status)
   (meta :initarg :meta :accessor response-meta)
   (body :initarg :body :accessor response-body :initform "")))

(defun make-request (url &optional client-key client-address)
  (let* ((parsed-url (uri url))
         (params (car (car (quri:uri-query-params parsed-url)))))
    (if (not (uri-path parsed-url))
        (setf (uri-path parsed-url) "/" ))
    (make-instance 'request :url parsed-url :path-info (uri-path parsed-url)
                            :params params
                            :client-key client-key
                            :client-address client-address)))

(defun make-response (status meta &optional body)
  (make-instance 'response :status (write-to-string status) :meta meta :body body))

(defmethod write-response ((the-response response) (the-stream stream))
  (write-sequence (babel:string-to-octets
                   (str:concat (response-status the-response)
                               " "
                               (response-meta the-response)
                               (coerce '(#\return #\newline) 'string)) :encoding :utf-8)
                  the-stream)
  (if (response-body the-response)
      (if (stringp (response-body the-response))
          (let ((the-body (if (str:ends-with-p #?"\n" (response-body the-response))
                              (response-body the-response)
                              #?"$((response-body the-response))\n")))
            (write-sequence (babel:string-to-octets the-body
                                                    :encoding :utf-8) the-stream))
      (write-sequence (response-body the-response) the-stream)))
  (force-output the-stream))

