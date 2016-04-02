(defmodule lmug-opt
  (export all))

(defun get (opts key)
  (case (proplists:get_value key opts)
    ('undefined '())
    (value value)))
