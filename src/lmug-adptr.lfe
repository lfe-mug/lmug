(defmodule lmug-adptr
  (export all))

(defun behaviour_info
  (('callbacks)
    '(#(convert-request 1) #(convert-request 2)
      #(convert-response 1) #(convert-response 2)
      #(convert-handler 1) #(convert-handler 2)))
  ((_)
    'undefined))
