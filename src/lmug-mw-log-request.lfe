(defmodule lmug-mw-log-request
  (doc "Middlware for logging the request and some of the response.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(include-lib "logjam/include/logjam.hrl")

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty map for options."
  (wrap handler #m(log-level debug)))

(defun wrap
  "Middleware that logs an HTTP request at the given log level.

  Note that this middleware will return 200 if no status has been set in the
  respose; as such, it is important that this be placed near or at the end of
  a middleware chain/definition."
  ((handler opts)
   (lambda (req)
     (let* ((log-level (maps:get 'log-level opts 'debug))
            (resp (funcall handler req))
            (status (maps:get 'status resp (http.status:ok)))
            (body-size (size (maps:get 'body resp #""))))
       (lmug-log:request req status body-size log-level)
       resp))))
