(in-package :germinal)

(defun basic-logging (handler)
  "Middleware that logs requests to *standard-output*."
  (lambda (request)
    (let ((response (funcall handler request)))
      (format
       *standard-output*
       "[~A] \"~A\" ~A ~A ~%"
       (local-time:now)
       (path-info request)
       (status response)
       (meta response))
      response)))

(defun validate-server-name (handler)
  "Middleware that ensures the requested host matches *germinal-server-name*."
  (lambda (request)
    (let* ((url (url request))
           (host (uri-host url)))
      (if (not (scan *germinal-server-name* host))
          (make-response 51 "Not Found")
          (funcall handler request)))))

(defun gemini-app (request)
  (serve-route request))

(defun middleware-chain (middlewares)
  (reduce
   #'funcall
   (remove-if
    #'null
    middlewares)
   :initial-value #'gemini-app
   :from-end t))
