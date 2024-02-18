(defmodule lmug-mw-resource
  (doc "Serve resource from the filesystem.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun wrap (handler)
  "The same as #'wrap/2 but with the default options map."
  (wrap handler #m()))

(defun wrap (handler opts)
  "Middleware that can load resources from on-disk instead of a handler.

  For the possible options available, see
  * `lmug-filesystem:default-response-opts/0`."
  (lambda (req)
    ;; TODO: implement
    (funcall handler req)))

(defun default-opts ()
  #m(doc-root #"."
     allow-symlinks? false
     prefer-handler? false))

(defun prefer-resource ()
  'tbd)

(defun prefer-handler ()
  ;; TODO: implement
  'not-implemented)