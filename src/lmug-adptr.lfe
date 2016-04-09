(defmodule lmug-adptr
  (export all))

(defun behaviour_info
  (('callbacks)
    '(#(->request 1) #(->request 2)
      #(response-> 1) #(response-> 2)
      #(handler-> 1) #(handler-> 2)))
  ((_)
    'undefined))
