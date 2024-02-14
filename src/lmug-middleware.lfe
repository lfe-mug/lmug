(defmodule lmug-middleware
  (export all))

(defun behaviour_info
  (('callbacks)
   '(#(wrap 1) #(wrap 2)))
  ((_)
   'undefined))
