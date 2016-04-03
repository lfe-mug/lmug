(defmodule lmug-request
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
  - ``query-params``:
  - ``scheme``:
  - ``method``:
  - ``content-type``:
  - ``content-length``:
  - ``content-encoding``:
  - ``ssl-client-cert``:
  - ``headers``:
  - ``body``:
  - ``orig``:

  Returns an lmug ``request`` record."
  (make-request
    server-port (lmug-opt:get opts 'server-port 8080)
    server-name (lmug-opt:get opts 'server-name "")
    remote-addr (lmug-opt:get opts 'remote-addr "")
    uri (lmug-opt:get opts 'uri "")
    path (lmug-opt:get opts 'path "")
    query-params (lmug-opt:get opts 'query-params '())
    scheme (lmug-opt:get opts 'scheme "")
    method (lmug-opt:get opts 'method 'get)
    content-type (lmug-opt:get opts 'content-type 'unknown-content-type)
    content-length (lmug-opt:get opts 'content-length 'undefined)
    content-encoding (lmug-opt:get opts
                       'content-encoding
                       'unknown-content-encoding)
    ssl-client-cert (lmug-opt:get opts
                      'ssl-client-cert
                      'unknown-ssl-client-cert)
    headers (lmug-opt:get opts 'headers '())
    body (lmug-opt:get opts 'body "")
    orig (lmug-opt:get opts 'orig "")))
