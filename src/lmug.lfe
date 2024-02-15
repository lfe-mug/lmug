(defmodule lmug
  (doc "General lmug functions and aliases for commonly-used functions
       in other modules.")
  (export all))

(defun app ()
  ""
  (lambda (_)
    (http.response:new)))

(defun body
  "The lmug library and adapters standardise on binary data for HTTP bodies."
  ((b) (when (is_binary b))
   b)
  ((b) (when (is_list b))
   (binary_to_list b)))

(defun headers
  "The lmug library and adapters standardise on a map of binary key/value pairs
  for HTTP headers."
  ((h) (when (is_map h))
   h)
  ((h) (when (is_list h))
   (http.header:list->map h)))

(defun method
  "The lmug library and adapters standardise on lower-case atoms for HTTP
  methods."
  ((m) (when (is_list m))
   (list_to_atom (string:to_lower m)))
  ((m) (when (is_atom m))
   (method (atom_to_list m)))
  ((m) (when (is_binary m))
   (method (binary_to_list m))))
