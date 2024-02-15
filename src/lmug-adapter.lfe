(defmodule lmug-adapter
  (export all))

(defun behaviour_info
  (('callbacks)
   '(#(convert-request 1) #(convert-request 2)
     #(convert-response 1) #(convert-response 2)
     #(call-handler 1) #(call-handler 2)))
  (('optional_callbacks)
   '(#(set-handler 1)
     #(get-handler 0)))
  ((_)
   'undefined))
