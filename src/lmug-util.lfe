(defmodule lmug-util
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(defun get-lmug-version ()
  (lutil:get-app-src-version "src/lmug.app.src"))

(defun get-version ()
  (++ (lutil:get-version)
      `(#(lmug ,(get-lmug-version)))))

; -record(mod,{init_data,
;              data=[],
;              socket_type=ip_comm,
;              socket,
;              config_db,
;              method,
;              absolute_uri=[],
;              request_uri,
;              http_version,
;              request_line,
;              parsed_header=[],
;              entity_body,
;              connection}).

; (defrecord request
;   (server-port 1206)
;   (server-name "")
;   (remote-addr "")
;   (uri "")
;   query-string
;   (scheme "")
;   (request-method 'get)
;   content-type
;   content-length
;   content-encoding
;   ssl-client-cert
;   (headers '())
;   body)

(defun httpd->lmug-request (data)
  "Every web server that gets an lmug adapter needs to implement a function
  like this one which will transform that server's request data into the
  request data needed by lmug, in the record structure required by lmug (and
  defined in the lmug Spec)."
  data)

; (defrecord response
;   (status 200)
;   (headers '())
;   (body ""))

(defun get-response (data)
  "Translate an lmug request to an lmug response."
  data)

(defun lmug->httpd-response (data)
  "The data paseed is an lmug response record."
  `(#(response
     ; XXX get the status
     ; XXX get the body
     ; XXX construct the headers in the way that httpd expects them
      #(200 ,(io_lib:format
               "~p"
               (list data))))))


