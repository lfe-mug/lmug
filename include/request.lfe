(defrecord request
  (server-port 1206)
  (server-name "")
  (remote-addr "")
  (uri "")
  query-string
  (scheme "")
  (request-method 'get)
  content-type
  content-length
  content-encoding
  ssl-client-cert
  (headers '())
  body
  orig)
