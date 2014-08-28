(defmodule lmug-barista-adapter
  (export all))

(defun get-default-options ()
  (orddict:from_list
      `(#(modules (mod_log mod_disk_log lmug-barista-adapter)))))

(defun get-default-handler ()
  "Given an lmug request record, return a default response."
  (lambda (x) (lmug-util:get-response x)))

(defun run ()
  "Run with the default handler and options."
  (run (get-default-handler)))

(defun run (handler)
  "Run with the default options but a specific handler."
  (run handler '()))

(defun run (handler custom-options)
  "Run with a specific handler and options."
  (let ((options (barista-options:merge
                   (get-default-options)
                   custom-options)))
    (barista:run-barista handler options)))

(defun stop (arg)
  (barista:stop-barista arg))

(defun stop (arg-1 arg-2)
  (barista:stop-barista arg-1 arg-2))

(defun do (httpd-mod-data)
  "This is the function that the Erlang/OTP httpd server calls on every request
  for each of the registered modules. In order for this to work, the barista
  HTTP server needs to be configured with #(modules (... <module name>)) at the
  end.

  Note that, in order to call the handler here, we need to set up a 'handler
  server' when we call the 'run' function. This will allow us to call the
  configured handler later (i.e., here in the 'do' function).

  This function does the following, when it is called (on each HTTH request):

   * looks up the PID for the handler loop
   * calls the middleware function that converts the Erlang/OTP httpd arg data
     to lmug request data
   * sends a message to the handler loop with converted request data
   * sets up a listener that will be called by the handler loop
   * waits to reveive data from the handler loop (the data which will have been
     produced by the handler function passed to run-barista/1 or run-barista/2)
   * converts the passed lmug request data to the format expected by
     Erlang/OTP httpd
  "
  (let ((handler-pid (whereis (barista:lmug-handler-name))))
    (! handler-pid
       (tuple (self)
              (lmug-util:httpd->lmug-request httpd-mod-data)))
    (receive
      ((tuple 'handler-output data)
        `#(proceed ,(lmug-util:lmug->httpd-response data))))))
