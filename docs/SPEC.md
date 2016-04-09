# lmug Spec

*v0.0.2*

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
  (Required, pos_integer())
  The port on which the request is being handled.
```

```
'server-name
  (Required, binary())
  The resolved server name, or the server IP address.
```

```
'remote-addr
  (Required, binary() | 'undefined')
  The IP address of the client or the last proxy that sent the request.
```

```
'uri
  (Required, binary())
  The request URI. Must start with "/".
```

```
'path
  (Required, [binary()])
  The request path as a list of binaries. For example:
      /path/to/page.html => '(#"path" #"to" #"page.html")

  Default: ()
```

```
'query-string
  (Optional, binary())
  The query string, if present.
```

```
'query-params
  (Optional, [{binary(), any()}])
  An LFE proplist of parsed and URL decoded query parameters.
```

```
'form-params
  (Optional, [{binary(), any()}])
  Form parameters, if present.
```

```
'params
  (Optional, [{binary(), any()}])
  the union of all parameters.
```

```
'scheme
  (Required, http | https)
  The transport protocol, must be one of 'http or 'https.
```

```
'method
  (Required, 'options' | 'get' | 'head' | 'post' |
             'put' | 'delete' | 'trace'
  The HTTP request method, must be a lowercase atom corresponding to a
  HTTP request method, such as 'get or 'post.

  Default: 'get
```

```
'protocol
  (Required, binary()))
  The protocol the request was made with, e.g. "HTTP/1.1".

  Default: #"HTTP/1.1"
```

```
'ssl-client-cert
  (Optional, binary() | 'undefined')
  The SSL client certificate, if supplied.

  Default: 'unknown-ssl-client-cert
```

```
'headers
  (Required, [{binary(), binary()}])
  An LFE proplist of downcased header name binaries to corresponding header
  value binaries.

  Default: ()
```

```
'body
  (Optional, binary() | iolist())
  A binary() or iolist() representing the request body.

  Default: #""
```

```
'orig
  (Optional, 'undefined' | any())
  A representation of the original request (Cowboy, Elli, Yaws, etc)
  that has been adapted for lmug.

  Default: 'undefined
```

```
'mw-data
  (Optional, list())
  A list of any middleware-specific data.

  Default: ()
```


## Response Record

A response record is an LFE record containing at least the following keys and
corresponding values:

```
'status
  (Required, integer())
  The HTTP status code, must be greater than or equal to 100.

  Default: 200
```

```
'headers
  (Required, [{binary(), binary()}
              | {binary(), [{binary(), [{binary(), binary()},...]}]}
              | [atom() | {atom(), term()}])
  An LFE proplist of HTTP header names to header values. These values may be
  either binaries, in which case one name/value header will be sent in the
  HTTP response, or a list of binaries, in which case a name/value header will
  be sent for each such binary value.

  Default: ()
```

```
'body
  (Optional, binary() | list() | file:name() | file:io_device())
  A representation of the response body, if a response body is appropriate
  for the response's status code. The respond body is handled according to
  its type:

  binary():
    Contents are sent to the client as-is.
  list():
    Each element of the list is sent to the client as a string.
  file:name():
    Contents at the specified location are sent to the client. The server may
    use an optimized method to send the file if such a method is available.
  file:io_device():
    Contents are consumed from the stream and sent to the client. When the
    stream is exhausted, it is file:close/1'd.

    Default: ""
```
