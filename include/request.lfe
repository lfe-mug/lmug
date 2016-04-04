(defrecord request
  (server-port 8080)
  (server-name "")
  (remote-addr "")
  (uri "")
  (path "")
  (query-string "")
  (query-params '())
  (form-params '())
  (params '())
  (scheme "")
  (method 'get)
  (content-type "unknown/type")
  content-length
  (content-encoding 'unknown-content-encoding)
  (ssl-client-cert 'unknown-ssl-client-cert)
  (headers '())
  (body "")
  (orig ""))

(defun loaded-request ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
