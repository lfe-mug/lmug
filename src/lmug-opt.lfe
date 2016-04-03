(defmodule lmug-opt
  (doc "Helper functions for parsing options.")
  (export all))

(defun get (opts key)
  (case (proplists:get_value key opts)
    ('undefined '())
    (value value)))
