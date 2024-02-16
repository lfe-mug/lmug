(defmodule lmug-mw-identity
  (doc "Pass-through/no-op middleware.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty map for options."
  (wrap handler #m()))

(defun wrap (handler _opts)
  "Middleware that does absolutely nothing at all."
  handler)
