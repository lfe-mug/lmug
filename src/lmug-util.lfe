(defmodule lmug-util
  (doc "lmug utility functions.")
  (export all))

(include-lib "inets/include/httpd.hrl")

(defun normalize-method
  ((method) (when (is_list method))
   (list_to_atom (string:to_lower method)))
  ((method)
   method))
