(defrecord response
  (status 200)
  (headers '()) ; This is a property list
  (body ""))

(defun loaded-response ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
