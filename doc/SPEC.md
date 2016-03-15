# lmug Spec

*v0.0.1*

lmug is defined in terms of the following:

 * handlers
 * middleware
 * adapters
 * requests records
 * response records

These are described below.


## Handlers

lmug handlers constitute the core logic of the web application. Handlers are
implemented as LFE functions that process a given request record to generate
and return a response record.


## Middleware

lmug middleware augments the functionality of handlers by invoking them in
the process of generating responses. Typically middleware will be
implemented as a higher-order function that takes one or more handlers and
configuration options as arguments and returns a new handler with the
desired compound behavior.


## Adapters

Handlers are run via lmug adapters, which are in turn responsible for
implementing the HTTP protocol and abstracting the handlers that they run
from the details of the protocol.

Adapters are implemented as functions of two arguments:

1. a handler, and
1. an options proplist.

The options proplist provides any needed configuration to the adapter, such
as the port on which to run.

Once initialized, adapters receive HTTP requests, parse them to construct a
request record, and then invoke their handler with this request record as an
argument. Once the handler returns a response record, the adapter uses it to
construct and send an HTTP response to the client.


## Request Record

A request record is an LFE record containing at least the following keys and
corresponding values:

```
'server-port
  (Required, Integer)
  The port on which the request is being handled.
```

```
'server-name
  (Required, String)
  The resolved server name, or the server IP address.
```

```
'remote-addr
  (Required, String)
  The IP address of the client or the last proxy that sent the request.
```

```
'uri
  (Required, String)
  The request URI. Must start with "/".
```

```
'query-string
  (Optional, String)
  The query string, if present.
```

```
'scheme
  (Required, Keyword)
  The transport protocol, must be one of :http or :https.
```

```
'request-method
  (Required, Keyword)
  The HTTP request method, must be a lowercase keyword corresponding to a
  HTTP request method, such as :get or :post.
```

```
'protocol
  (Required, String)
  The protocol the request was made with, e.g. "HTTP/1.1".
```

```
'ssl-client-cert
  (Optional, X509Certificate)
  The SSL client certificate, if supplied.
```

```
'headers
  (Required, IPersistentMap)
  An LFE record of downcased header name Strings to corresponding header
  value Strings.
```

```
'body
  (Optional, InputStream)
  An InputStream for the request body, if present.
```


## Response Record

A response record is an LFE record containing at least the following keys and
corresponding values:

```
'status
  (Required, Integer)
  The HTTP status code, must be greater than or equal to 100.
```

```
'headers
  (Required, IPersistentMap)
  An LFE proplist of HTTP header names to header values. These values may be
  either Strings, in which case one name/value header will be sent in the
  HTTP response, or a seq of Strings, in which case a name/value header will
  be sent for each such String value.
```

```
'body
  (Optional, {String, ISeq, File, InputStream})
  A representation of the response body, if a response body is appropriate
  for the response's status code. The respond body is handled according to
  its type:

  String:
    Contents are sent to the client as-is.
  ISeq:
    Each element of the seq is sent to the client as a string.
  File:
    Contents at the specified location are sent to the client. The server may
    use an optimized method to send the file if such a method is available.
  InputStream:
    Contents are consumed from the stream and sent to the client. When the
    stream is exhausted, it is .close'd.
```
