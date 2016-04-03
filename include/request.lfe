(defrecord request
  (server-port     8080)
  (server-name     #"")
  (remote-addr     #"")
  (uri             #"")
  (path            ())
  (query-string    #"")
  (query-params    ())
  (scheme          'http)
  (method          'get)
  (protocol        #"HTTP/1.1")
  (ssl-client-cert 'unknown-ssl-client-cert)
  (headers         ())
  (body            'undefined)
  (orig            'undefined)
  (mw-data         ()))

(defun loaded-request ()
  "This is just a dummy function for display purposes when including from the
REPL (the last function loaded has its name printed in stdout).

This function needs to be the last one in this include."
  'ok)
