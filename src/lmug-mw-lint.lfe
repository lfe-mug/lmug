(defmodule lmug-mw-lint
  (doc "Middleware that checks lmug requests and responses for correctness.")
  (behaviour lmug-mw)
  (export (wrap 1) (wrap 2))
  (import (from lmug-lint (check-request 1) (check-response 1))))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty list for options."
  (wrap handler '()))

;; Ported from ring.middleware.lint
(defun wrap (handler _)
  "Wrap a handler to validate incoming requests and outgoing responses
  according to the current lmug specification. An exception is raised if either
  the request or response is invalid."
  (lambda (req)
    (check-request req)
    (let ((resp (funcall handler req)))
      (check-response resp)
      resp)))
