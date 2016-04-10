(defmodule lmug-mw-identity
  (doc "Pass-through/no-op middleware.")
  (behaviour lmug-mw)
  (export all))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty list for options."
  (wrap handler '()))

(defun wrap (handler _)
  "Middleware that does absolutely nothing at all."
  handler)
