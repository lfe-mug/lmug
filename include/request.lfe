(defrecord request
  (server-port     (error #(required server-port integer)))
  (server-name     (error #(required server-name string)))
  (remote-addr     (error #(required remote-addr string)))
  (uri             (error #(required uri string)))
  (path            ())
  (query-params    ())
  (scheme          (error #(required scheme #(http https))))
  (method  'get)
  (ssl-client-cert 'unknown-ssl-client-cert)
  (headers         ())
  (body            'undefined)
  (orig            'undefined))

(defun loaded-request ()
  "This is just a dummy function for display purposes when including from the
REPL (the last function loaded has its name printed in stdout).

This function needs to be the last one in this include."
  'ok)
