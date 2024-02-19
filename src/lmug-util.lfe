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

(defun filename-ext
  "Returns a content type based upon the file extension of a filename or filepath."
  ((url) (when (is_binary url))
   (filename-ext (binary_to_list url)))
  ((url) (when (is_list url))
   (case (string:split (filename:extension url) ".")
     ('("" "") (lmug:default-content-type))
     (`("" ,ext) (lmug:ext->content-type ext))
     (_ (lmug:default-content-type)))))
