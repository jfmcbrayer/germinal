(in-package :germinal)
(interpol:enable-interpol-syntax)

(defclass request ()
  ((url :initarg :url :accessor url)
   (path-info :initarg :path-info :accessor path-info)
   (params :initarg :params :accessor params)
   (client-key :initarg :client-key :accessor client-key)
   (client-address :initarg :client-address :accessor client-address)))

(defclass response ()
  ((status :initarg :status :accessor status)
   (meta :initarg :meta :accessor meta)
   (body :initarg :body :accessor body :initform "")))

(defun make-request (url &optional client-key client-address)
  (let* ((parsed-url (uri url))
         (params (quri:uri-query-params parsed-url)))
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
                   (str:concat (status the-response)
                               " "
                               (meta the-response)
                               '(#\return #\newline)) :encoding :utf-8)
                  the-stream)
  (if (body the-response)
      (if (stringp (body the-response))
          (let ((the-body (if (str:ends-with-p #?"\n" (body the-response))
                              (body the-response)
                              #?"$((body the-response))\n")))
            (write-sequence (babel:string-to-octets the-body
                                                    :encoding :utf-8) the-stream))
      (write-sequence (body the-response) the-stream)))
  (force-output the-stream))

