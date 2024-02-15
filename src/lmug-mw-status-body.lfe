(defmodule lmug-mw-status-body
  (doc "Set the body as to the value of the status code.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty list for options."
  (wrap handler '()))

(defun wrap (handler _opts)
  "Middleware that sets the body to be the same as the status code."
  (lambda (req)
    (let ((resp (funcall handler req)))
      (http.response:set-body resp (mref resp 'status)))))
