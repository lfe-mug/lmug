(defmodule lmug
  (import
    (from proplists
      (delete 2)
      (get_value 2)
      (is_defined 2)))
  (export all))

(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(defun host->tuple (host)
  (let ((`#(ok ,tuple) (inet:getaddr host 'inet)))
    tuple))

(defun add-listen (options)
  (++ options
      `(#(listen ,(host->tuple (get_value 'host options))))))

(defun add-default (key val options)
  (cond
    ((is_defined key options)
      options)
    ('true
      (++ options `(#(,key ,val))))))


