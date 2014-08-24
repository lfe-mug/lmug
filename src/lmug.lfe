(defmodule lmug
  (import
    (from proplists
      (delete 2)
      (get_value 2)
      (is_defined 2)))
  (export all))

(defun host->tuple (host)
  (let ((`#(ok ,tuple) (inet:getaddr host 'inet)))
    tuple))

(defun add-listen (options)
  (++ options
      `(#(listen ,(host->tuple (get_value 'host options))))))

(defun rename-key (old-key new-key old-options)
  (let ((options (++ old-options
                     `(#(,new-key ,(get_value old-key old-options))))))
    (delete old-key options)))

(defun add-default (key val options)
  (cond
    ((is_defined key options)
      options)
    ('true
      (++ options `(#(,key ,val))))))


