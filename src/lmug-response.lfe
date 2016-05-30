(defmodule lmug-response
  (doc "Manipulating `response`s.")
  (export all))

(include-lib "lmug/include/response.lfe")

(defun response ()
  "Create a default response suitable for use as an lmug handler. As this
  function takes no arguments, it cannot be used in the middle of a
  middleware chain, only at the beginning.

  Returns a function which expects an lmug request as the only argument."
  (response 'undefined ()))

(defun response (_handler)
  "Create a default response suitable for use as an lmug handler. This
  function takes one argument (though it is ignored), and as such it can be
  used in the middle of a middleware chain.

  Note that the intended use for this function is to override the response
  generated in the lmug middleware chain up until that point. As such, you
  should almost never need to use this function directly.

  Returns a function which expects an lmug request as the only argument."
  (response _handler ()))

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
      headers (lmug-opt:get opts 'headers ())
      body (lmug-opt:get opts 'body #""))))

(defun header (resp header-key header-value)
  "Returns an updated lmug response with the specified header added."
  (set-response-headers
    resp
    (cons `#(,header-key ,header-value)
          (response-headers resp))))

(defun get-header
  "Looks up a header in a lmug response (or request) case insensitively,
  returning the value of the header, or nil if not present."
  ((resp #"")
    #"")
  ((resp header-key) (when (is_atom header-key))
    (get-header resp (atom_to_binary header-key 'latin1)))
  ((resp header-key)
    (match-header (response-headers resp) header-key)))

(defun content-type (resp content-type)
  "Returns an updated lmug response with a Content-Type header
  corresponding to the given content-type."
  (header resp #"Content-Type" content-type))

(defun match-header (headers header-key)
  "Return the first header that matches the given header-key."
  (case (match-headers headers header-key)
    (() #"")
    (headers (car headers))))

(defun match-headers (headers sought-key)
  "Return all the headers that match the given header-key."
  (lists:filtermap
    (match-lambda ((`#(,key ,value))
      (case (re:run sought-key key '(caseless #(capture none list)))
        ('nomatch 'false)
        ('match `#(true ,value)))))
    headers))
