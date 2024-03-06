(defmodule lmug-watcher
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

(defun start (path)
  (gen_server:start (register-name)
                    (callback-module)
                    ;;(initial-state)
                    path
                    (genserver-opts)))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;;; callback implementation

(defun init (path)
  (log-debug "Got initial state: ~p" (list path))
  (fs:start_link 'lmug-fsw path)
  `#(ok ,path #(continue subscribe)))

(defun handle_call
  ((message _caller state-data)
   (let ((err (ERR_UNK_CMD)))
     (log-warn "~s" (list (mref err 'error)))
     `#(reply ,err ,state-data))))

(defun handle_cast
  ((_ state-data)
   (let ((err (ERR_UNK_CMD)))
     (log-warn "~s" (list (mref err 'error)))
     `#(noreply ,state-data))))

(defun handle_continue
  (('subscribe state)
   (log-debug "Subscribing ~p (~p) to file system events ..." (list (MODULE) (self)))
   (fs:subscribe 'lmug-fsw (self))
   `#(noreply ,state))
  ((message state-data)
   (let ((err (ERR_UNK_CMD)))
     (log-warn "~s" (list (mref err 'error)))
     `#(noreply ,state-data))))

(defun handle_info
  ((`#(,_pid #(fs file_event) #(,file-name ,event-types)) state-data)
   (log-debug "Got fs event of type(s): ~p on file: ~s" (list event-types file-name))
   (handle-fs file-name event-types)
   `#(noreply ,state-data))
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (log-error "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((msg state-data)
   (let ((err (ERR_UNK_CMD)))
     (log-warn "~s" (list (mref err 'error)))
     (log-debug "message: ~p" (list msg))
     `#(noreply ,state-data))))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; Watcher API

(defun handle-fs (file-path types)
  (log-debug "dispatching on types: ~p" (list types))
  (cond
   ((lists:member 'created types) (handle-create file-path))
   ((lists:member 'inodemetamod types) (handle-modify file-path))
   ((lists:member 'modified types) (handle-modify file-path))
   ((lists:member 'removed types) (handle-delete file-path))
   ((lists:member 'renamed types) (handle-rename file-path))
   ))

(defun handle-create (file-path)
  (log-info "handling file creation of ~s" (list file-path))
  'tbd)

(defun handle-delete (file-path)
  (log-info "handling file deletion of ~s" (list file-path))
  'tbd)

(defun handle-modify (file-path)
  (log-info "handling file modification of ~s" (list file-path))
  'tbd)

(defun handle-rename (file-path)
  (log-info "handling file renaming of ~s" (list file-path))
  'tbd)

