(defmodule lmug-log
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun format
  ((`#m(body ,body
       method ,method
       headers ,headers
       remote-addr ,remote-addr
       url ,url
       url-parsed ,url-parsed)
       status)
   (let* ((delim " - ")
         (now (calendar:system_time_to_rfc3339 (erlang:system_time 'second)))
         (method (string:to_upper (atom_to_list method)))
         (user (lmug-util:bin->str (maps:get 'user url-parsed #"")))
         (user (if (== "" user)
                 user
                 (++ delim user " ")))
         (referrer (lmug-util:bin->str
                    (maps:get #"referrer"
                             headers
                             (maps:get #"referer" headers remote-addr))))
         (referrer (if (== "" referrer)
                     referrer
                     (++ delim referrer)))
         (user-agent (lmug-util:bin->str (maps:get #"user-agent" headers #"")))
         (user-agent (if (== "" user-agent)
                       user-agent
                       (++ delim user-agent))))
     (io_lib:format "~s~s[~s] '~s ~s' ~p ~p~s~s"
                    (list remote-addr
                          user
                          now
                          method
                          url
                          status
                          (size body)
                          referrer
                          user-agent)))))

(defun request-debug (req status)
  (request req status 'debug))

(defun request-info (req status)
  (request req status 'info))

(defun request-notice (req status)
  (request req status 'notice))

(defun request-warn (req status)
  (request req status 'warn))

(defun request-error (req status)
  (request req status 'error))

(defun request-critical (req status)
  (request req status 'critical))

(defun request-alert (req status)
  (request req status 'alert))

(defun request-emergency (req status)
  (request req status 'emergency))

(defun request
  ((req status 'debug)
   (log-debug (format req status)))
  ((req status 'info)
   (log-info (format req status)))
  ((req status 'notice)
   (log-notice (format req status)))
  ((req status 'warn)
   (log-warn (format req status)))
  ((req status 'error)
   (log-error (format req status)))
  ((req status 'critical)
   (log-critical (format req status)))
  ((req status 'alert)
   (log-alert (format req status)))
  ((req status 'emergency)
   (log-emergency (format req status))))
