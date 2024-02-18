(defmodule lmug-log
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun format
  "Create an Apache-era 'combined-style' log format."
  ;; Note that the req can't be pattern-matched here, since we first need to
  ;; ensure that the required fields are present (a function head does not match
  ;; when one of the keys in the function head doesn't exist in the map).
  ((req status body-size)
   (let* ((delim " - ")
          (headers (maps:get #"headers" req #m()))
          (req (maps:merge (http.request:new #"") req))
          (method (mref req 'method))
          (headers (mref req 'headers))
          (remote-addr (mref req 'remote-addr))
          (url (mref req 'url))
          (url-parsed (mref req 'url-parsed))
          (now (calendar:system_time_to_rfc3339 (erlang:system_time 'second)))
          (method (string:to_upper (atom_to_list method)))
          (user (lmug-util:bin->str (maps:get 'user url-parsed #"")))
          (user (if (== "" user)
                  (++ user " ")
                  (++ delim user " ")))
          (referrer (lmug-util:bin->str
                     (maps:get #"referrer"
                               headers
                               (maps:get #"referer" headers remote-addr))))
          (referrer (if (== "" referrer)
                      referrer
                      (++ delim "'" referrer "'")))
          (user-agent (lmug-util:bin->str (maps:get #"user-agent" headers #"")))
          (user-agent (if (== "" user-agent)
                        user-agent
                        (++ delim "'" user-agent "'"))))
     (io_lib:format "~s~s[~s] '~s ~s' ~p ~p~s~s"
                    (list remote-addr
                          user
                          now
                          method
                          ;; TODO: let's use the cleaned _relative_ path here
                          (lists:flatten (yuri:format (yuri:clean url-parsed)))
                          status
                          body-size
                          referrer
                          user-agent)))))
  
(defun request-debug (req status body-size)
  (request req status body-size 'debug))

(defun request-info (req status body-size)
  (request req status body-size 'info))

(defun request-notice (req status body-size)
  (request req status body-size 'notice))

(defun request-warn (req status body-size)
  (request req status body-size 'warn))

(defun request-error (req status body-size)
  (request req status body-size 'error))

(defun request-critical (req status body-size)
  (request req status body-size 'critical))

(defun request-alert (req status body-size)
  (request req status body-size 'alert))

(defun request-emergency (req status body-size)
  (request req status body-size 'emergency))

(defun request
  ((req status body-size 'debug)
   (log-debug (format req status body-size)))
  ((req status body-size 'info)
   (log-info (format req status body-size)))
  ((req status body-size 'notice)
   (log-notice (format req status body-size)))
  ((req status body-size 'warn)
   (log-warn (format req status body-size)))
  ((req status body-size 'error)
   (log-error (format req status body-size)))
  ((req status body-size 'critical)
   (log-critical (format req status body-size)))
  ((req status body-size 'alert)
   (log-alert (format req status body-size)))
  ((req status body-size 'emergency)
   (log-emergency (format req status body-size))))
