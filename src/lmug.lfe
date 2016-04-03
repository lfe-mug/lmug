;;;; This module is really only intended to be used when slurping from the
;;;; LFE REPL.
(defmodule lmug
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(defun response ()
  "Create a default response suitable for use as an lmug handler. As this
  function takes no arguments, it cannot be used in the middle of a
  middleware chain, only at the beginning.

  Returns a function which expects an lmug request as the only argument."
  (response 'undefined '()))

(defun response (_handler)
  "Create a default response suitable for use as an lmug handler. This
  function takes one argument (though it is ignored), and as such it can be
  used in the middle of a middleware chain.

  Note that the intended use for this function is to override the response
  generated in the lmug middleware chain up until that point. As such, you
  should almost never need to use this function directly.

  Returns a function which expects an lmug request as the only argument."
  (response _handler '()))

(defun response (_handler opts)
  "Create a default response suitable for use as an lmug handler. This
  function takes two arguments (the first, the handler, is ignored). The second
  argument is for options (proplist).

  Allowed options are:

  - ``status``: HTTP response status number (e.g., 200)
  - ``headers``: HTTP response headers
  - ``body``: HTTP response body

  This function may be used in the middle of a middleware chain. Note, however,
  that the intended use for this function is to override the response
  generated in the lmug middleware chain up until that point. As such, you
  should almost never need to use this function directly.

  Returns a function which expects an lmug request as the only argument."
  (lambda (_req)
    (make-response
      status (lmug-opt:get opts 'status 200)
      headers (lmug-opt:get opts 'headers '())
      body (lmug-opt:get opts 'body ""))))
