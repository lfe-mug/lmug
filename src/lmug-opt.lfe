(defmodule lmug-opt
  (doc "Helper functions for parsing options.")
  (export all))

(defun get (opts key)
  (get opts key ()))

(defun get (opts key default)
  (case (proplists:get_value key opts)
    ('undefined default)
    (value value)))
