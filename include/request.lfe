(defrecord request
  (server-port     8080)
  (server-name     #"")
  (remote-addr     'undefined)
  (uri             #"")
  (path            ())
  (query-string    #"")
  (query-params    ())
  (form-params     ())
  (params          ())
  (scheme          'http)
  (method          'get)
  (protocol        #"HTTP/1.1")
  (ssl-client-cert 'undefined)
  (headers         ())
  (body            #"")
  (orig            'undefined)
  (mw-data         ()))

(defun loaded-request ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
