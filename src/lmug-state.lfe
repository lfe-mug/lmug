(defmodule lmug-state
  (behaviour gen_server)
  (export all))

(include-lib "logjam/include/logjam.hrl")

;;; config functions

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () '())
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))

;;; gen_server implementation

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;;; callback implementation

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_cast
  ((`#(set ,key ,value) state-data)
    `#(noreply ,(cons `#(,key ,value) state-data))))

(defun handle_call
  ((#(get all) _caller state-data)
   `#(reply ,state-data ,state-data))
  ((`#(get resource ,path) _caller state-data)
   (let ((res (maps:get path
                        (proplists:get_value 'resources state-data)
                        #m(error not-found))))
     `#(reply ,res ,state-data)))
  ((`#(get ,key) _caller state-data)
    `#(reply ,(proplists:get_value key state-data) ,state-data))
  (('stop _caller state-data)
    `#(stop shutdown ok state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (log-error "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; State API

(defun set-handler (handler)
  (gen_server:cast (server-name) `#(set handler ,handler)))

(defun get-data ()
  (gen_server:call (server-name) #(get all)))

(defun get-handler ()
  (gen_server:call (server-name) #(get handler)))

(defun call-handler (req)
  (funcall (get-handler) req))

(defun set-resources (store)
  (gen_server:cast (server-name) `#(set resources ,store)))

(defun get-resource (filepath)
  (gen_server:cast (server-name) `#(get resource ,filepath)))
