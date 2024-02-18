(defmodule lmug-util
  (doc "lmug utility functions.")
  (export all))

(include-lib "inets/include/httpd.hrl")

(defun normalize-method
  ((method) (when (is_list method))
   (list_to_atom (string:to_lower method)))
  ((method)
   method))

(defun bin->str (bin)
  (bin->str-with-default bin ""))

(defun bin->str-with-default
  ((bin def) (when (is_binary bin))
   (bin->str-with-default (binary_to_list bin) def))
  ((bin def) (when (is_binary def))
   (bin->str-with-default bin (binary_to_list def)))
  (("" def)
   def)
  ((str _)
   str))