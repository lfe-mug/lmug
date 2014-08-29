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

(defun normalize-http-verb
  ((verb) (when (is_list verb))
    (list_to_atom (string:to_lower verb)))
  ((verb) (when (is_atom verb))
    (normalize-http-verb (atom_to_list verb))))

(defun split-host-data (host-data)
  (string:tokens (car (string:tokens host-data "/")) ":"))

(defun get-host-data (host-data)
  (case (split-host-data host-data)
    (`(,host)
      `(,host 80))
    (`(,host ,port)
      `(,host ,(list_to_integer port)))))

(defun get-hostname
  ((`#(init_data ,_ ,hostname))
    hostname))

(defun split-query (url)
  (string:tokens url "?"))

(defun parse-query-string (url)
  (case (split-query url)
    (`(,host ,query)
      (httpd:parse_query query))
    (`(,host)
      '())))

(defun httpd->lmug-request (data)
  "Every web server that gets an lmug adapter needs to implement a function
  like this one which will transform that server's request data into the
  request data needed by lmug, in the record structure required by lmug (and
  defined in the lmug Spec)."
  (let ((`(,host ,port) (get-host-data (mod-absolute_uri data)))
        (remote-host (get-hostname (mod-init_data data)))
        (uri (mod-request_uri data))
        (body (mod-entity_body data)))
    (make-request
      server-port port
      server-name host
      remote-addr remote-host
      ;; XXX the following need to be sorted out
      uri uri ; this should have query params
      path uri ; this should be with no query params
      query-params (parse-query-string uri)
      ;; XXX figure out how to get the scheme
      scheme 'unknown-scheme
      request-method (normalize-http-verb (mod-method data))
      ;; XXX figure out how to get the content-type
      content-type 'unknown-content-type
      content-length (length body)
      ;; XXX figure out how to get the content-encoding
      content-encoding 'unknown-content-encoding
      headers (mod-parsed_header data)
      body body
      orig data)))

(defun get-response (lmug-request-data)
  "Translate an lmug request to an lmug response."
  (make-response
    status 200
    headers '(#(content-type "text/plain"))
    body (lists:flatten
           (io_lib:format "Request data: ~n~p"
                          (list lmug-request-data)))))

(defun get-body-length (body)
  (integer_to_list (length body)))

(defun lmug->httpd-response (lmug-response-data)
  "The data paseed is an lmug response record."
  (let ((body (response-body lmug-response-data)))
    ;;(lfe_io:format "body: ~n~p~n" (list body))
    `(#(response
        #(response
          ,(++
            `(#(code ,(response-status lmug-response-data))
              #(content-length ,(get-body-length body)))
            (response-headers lmug-response-data))
          ,body)))))


