(defmodule lmug-mw-request-id
  (doc "Add a request ID.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty map for options."
  (wrap handler #m()))

(defun wrap (handler _opts)
  "Middleware that adds a request ID for use in tracing, etc."
  (lambda (req)
    (let* ((id (integer_to_binary (trunc (* (rand:uniform) (math:pow 2 128)))))
           (resp (funcall handler (http.request:add-header req #"X-Request-ID" id))))
      (http.response:add-header resp
                                #"X-Request-ID"
                                id))))
