(defmodule lmug-mw-content-type
  (doc "Set content type based upon file extension in URL.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty map for options."
  (wrap handler #m()))

(defun wrap (handler _opts)
  "Middleware that adds a content type based upon the file extension in the URL."
  (lambda (req)
    (let ((resp (funcall handler req)))
      (lmug-response:add-content-type-from-ext resp (mref req 'url)))))
