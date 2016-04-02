(defrecord request
  (server-port 1206)
  (server-name "")
  (remote-addr "")
  (uri "")
  (path "")
  query-params
  (scheme "")
  (method 'get)
  (content-type 'unknown-content-type)
  content-length
  (content-encoding 'unknown-content-encoding)
  (ssl-client-cert 'unknown-ssl-client-cert)
  (headers '())
  body
  orig)
