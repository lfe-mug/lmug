(defmodule lmug-request
  (doc "Manipulating `request`s.")
  (export all))

(include-lib "lmug/include/request.lfe")

(defun request ()
  "Create a default request suitable for use by lmug handlers.

  Returns an lmug ``request`` record."
  (request '()))

(defun request (opts)
  "Create a default request suitable for use by lmug handlers. The
  argument is a proplist (empty list is allowed) used for setting
  record fields.

  Allowed proplist options are:

  XXX: Fill in the description values

  - ``server-port``:
  - ``server-name``:
  - ``remote-addr``:
  - ``uri``:
  - ``path``:
  - ``query-string``:
  - ``query-params``:
  - ``form-params``:
  - ``params``:
  - ``scheme``:
  - ``method``:
  - ``ssl-client-cert``:
  - ``headers``:
  - ``body``:
  - ``orig``:
  - ``mw-data``:

  Returns an lmug ``request`` record."
  (make-request
    server-port (lmug-opt:get opts 'server-port 8080)
    server-name (lmug-opt:get opts 'server-name #"")
    remote-addr (lmug-opt:get opts 'remote-addr 'undefined)
    uri (lmug-opt:get opts 'uri #"")
    path (lmug-opt:get opts 'path '())
    query-string (lmug-opt:get opts 'query-string #"")
    query-params (lmug-opt:get opts 'query-params '())
    form-params (lmug-opt:get opts 'form-params '())
    params (lmug-opt:get opts 'params '())
    scheme (lmug-opt:get opts 'scheme 'http)
    method (lmug-opt:get opts 'method 'get)
    ssl-client-cert (lmug-opt:get opts 'ssl-client-cert 'undefined)
    headers (list* `#(#"Content-Type"
                      ,(lmug-opt:get opts 'content-type #"unknown/type"))
                   ;; XXX: Don't add if 'undefined.
                   ;; `#(#"Content-Length"
                   ;;    ,(lmug-opt:get opts 'content-length 'undefined))
                   ;;; XXX: Don't add if 'undefined.
                   ;; `#(#"Content-Encoding"
                   ;;    ,(lmug-opt:get opts
                   ;;       'content-encoding
                   ;;       'unknown-content-encoding))
                   (lmug-opt:get opts 'headers '()))
    body (lmug-opt:get opts 'body #"")
    orig (lmug-opt:get opts 'orig 'undefined)))
