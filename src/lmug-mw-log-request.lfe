(defmodule lmug-mw-log-request
  (doc "Middlware for logging the request and some of the response.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty map for options."
  (wrap handler #m()))

(defun wrap
  "Middleware that logs an HTTP request at the given log level."
  ((handler `#m(log-level ,log-level))
   (lambda (req)
     (let* ((user-agent (maps:get #"user-agent" req #""))
           (referrer (maps:get #"referrer" req (maps:get #"referer" req #"")))
           (resp (funcall handler req))
           (status (mref resp 'status)))
       (lmug-log:request req user-agent referrer status log-level)))))
