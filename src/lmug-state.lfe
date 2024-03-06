(defmodule lmug-state
  (behaviour gen_server)
  (export all))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lmug/include/const.hrl")

;;; config functions

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () #m())
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))

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
    `#(noreply ,(maps:put key value state-data))))

(defun handle_call
  ((#(get all) _caller state-data)
   `#(reply ,state-data ,state-data))
  ((`#(get resource ,path) _caller state-data)
   (let* ((ress (maps:get 'resources state-data #m()))
          (res (maps:get path
                         ress
                         #m(error not-found))))
     `#(reply ,res ,state-data)))
  ((`#(get ,key) _caller state-data)
    `#(reply ,(maps:get key state-data "") ,state-data))
  (('stop _caller state-data)
    `#(stop shutdown ok state-data))
  ((message _caller state-data)
    `#(reply ,(ERR_UNK_CMD) ,state-data)))

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

(defun call-handler (req)
  (funcall (get-handler) req))

(defun dump ()
  (gen_server:call (server-name) #(get all)))

(defun get (key)
  (gen_server:call (server-name) `#(get ,key)))

(defun get-handler ()
  (gen_server:call (server-name) #(get handler)))

(defun get-in (keys)
  (clj:get-in (dump) keys))

(defun set (key val)
  (gen_server:cast (server-name) `#(set ,key ,val)))

(defun set-handler (handler)
  (gen_server:cast (server-name) `#(set handler ,handler)))
