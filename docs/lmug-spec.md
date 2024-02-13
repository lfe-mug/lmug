# lmug Specification

*v0.1.0*


lmug is an abstraction layer for building HTTP server applications in LFE.

The specification is divided into two parts; a synchronous API, and an asynchronous API. The synchronous API is simpler, but the asynchronous API can be more performant.


## 1. Synchronous API

Ring is defined in terms of handlers, middleware, adapters, request maps and response maps, each of which are described below.

## 1.1. Handlers

In the lmug view of the HTTP universe, handlers constitute the core logic of the web application. Handlers are implemented as LFE functions.

A synchronous handler takes 1 argument, a request map, and returns a response map.

```lisp
(defun handler (request) response)
```

### 1.2. Middleware

lmug middlware augment the functionality of handlers. Middleware is implemented as higher-order functions that take one or more handlers and configuration options as arguments and return a new handler with the desired additional behavior.

### 1.3. Adapters

Ring adapters are side-effectful functions that take a handler and a map of options as arguments, and when invoked start a HTTP server. To run lmud, an adaptor will need to be written for a webserver and should provide a means of:

1. converting the webserver's request data to an lmug request map
1. converting lmug's response map to the webservers response data
1. running the webserver itself

```lisp
(run-adapter handler options)
```

Once invoked, and adapter will receive HTTP requests, parse them to construct a request map, and then invoke their handler with this request map as an argument. Once the handler (and any middleware) has the final form of the transformed response map, the adapter will use this map to construct and send an HTTP response to the client.

### 1.4. Request Maps

lmug uses the [LFE HTTP library](https://github.com/lfe-http/http)'s request map to represent an HTTP request. The full structure is available [here](https://github.com/lfe-http/http/blob/main/src/http.request.lfe) and there is a textual overview [here](https://github.com/lfe-mug/lmug/blob/main/docs/core-concepts.md#requests).


### 1.5. Response Maps

lmug uses the [LFE HTTP library](https://github.com/lfe-http/http)'s response map to represent an HTTP response. The map definition is available [here](https://github.com/lfe-http/http/blob/main/src/http.response.lfe) and a brief textual overview is [here](https://github.com/lfe-mug/lmug/blob/main/docs/core-concepts.md#responses).

## 2. Asynchronous API

WARNING: This portion of the spec is a work in progress!

The asynchronous API builds upon the synchronous API. The differences
between the two APIs are described below.

### 2.1. Handlers

An asynchronous handler takes 3 arguments: a request map, a callback
function for sending a response and a callback function for raising an
exception. The response callback takes a response map as its
argument. The exception callback takes an exception as its
argument. The return value of the function is ignored.

```lisp
(lambda (request respond-fn raise-fn)
  (funcall respond-fn response))
```

```lisp
(lambda (request respond raise)
  (funcall raise-fn exception))
```

A handler function may simultaneously support synchronous and
asynchronous behavior by providing both arities (same function name) in a module.


### 2.2. Adapters

An adapter may support synchronous handlers, or asynchronous handlers,
or both. If it supports both, it should have an option to specify
which one to use at the time it is invoked.

For example:

```clojure
(run-adapter handler {:async? true})
```

## 3. Websockets

WARNING: This portion of the spec is a work in progress!

A HTTP request can be promoted into a websocket by means of an "upgrade" header.

In this situation, a Ring handler may choose to respond with a websocket response instead of a HTTP response.

### 3.1. Websocket Responses

A websocket response is a map that represents a WebSocket, and may be returned from a handler in place of a response map.

```lisp
(lambda (request)
  (lmug.websocket #'websocket-listener/1))
```

It may also be used from an asynchronous handler.

```lisp
(lambda (request respond raise)
  (respond (lmug.websocket #'websocket-listener/1)))
```

A websocket response contains the following keys. Any key not marked as
**required** may be omitted.

| Key                      | Type                    | Required |
| ------------------------ | ----------------------- | -------- |
|`:lmug.websocket/listener`|`ring.websocket/Listener`| Yes      |
|`:lmug.websocket/protocol`|`String`                 |          |

#### :lmug.websocket/listener

An event listener that satisfies the `lmug.websocket.protocols/Listener`
protocol, as described in section 3.2.

#### :lmug.websocket/protocol

An optional websocket subprotocol. Must be one of the values listed in
the `Sec-Websocket-Protocol` header on the request.

### 3.2. Websocket Listeners

A websocket listener must satisfy the
`lmug.websocket.protocols/Listener` protocol:

```clojure
(defprotocol Listener
  (on-open    [listener socket])
  (on-message [listener socket message])
  (on-pong    [listener socket data])
  (on-error   [listener socket throwable])
  (on-close   [listener socket code reason]))
```

It *may* optionally satisfy the `ring.websocket.protocols/PingListener`
protocol:

```clojure
(defprotocol PingListener
  (on-ping [listener socket data]))
```

If the `PingListener` protocol is not satisifed, the adapter *must*
default to respond to each ping message with a corresponding pong
message that has the same data.

#### on-open

Called once when the websocket is first opened. Supplies a `socket`
argument that satisfies `ring.websocket.protools/Socket`, described in
section 3.3.

#### on-message

Called when a text or binary message frame is received from the client.
The `message` argument must be a `java.lang.CharSequence` or a
`java.nio.ByteBuffer` depending on whether the message is text or binary.

#### on-ping

Called when a "ping" frame is received from the client. The `data`
argument is a `java.nio.ByteBuffer` that contains optional client
session data.

If the user implements this method, they are responsible for sending
the return "pong" that the websocket protocol expects.

#### on-pong

Called when a "pong" frame is received from the client. The `data`
argument is a `java.nio.ByteBuffer` that contains optional client
session data.

#### on-error

Called when an error occurs. This may cause the websocket to be closed.
The `throwable` argument is a `java.lang.Throwable` that was generated
by the error.

#### on-close

Called once when the websocket is closed. Guaranteed to be called, even
if an error occurs, so may be used for finalizing/cleanup logic. Takes
an integer `code` and a string `reason` as arguments.

### 3.3. Websocket Sockets

A socket must satisfy the `lmug.websocket.protocols/Socket` protocol:

```clojure
(defprotocol Socket
  (-open? [socket])
  (-send  [socket message])
  (-ping  [socket data])
  (-pong  [socket data])
  (-close [socket code reason]))
```

It *may* optionally satisfy the `lmug.websocket.protocols/AsyncSocket`
protocol:

```clojure
(defprotocol AsyncSocket
  (-send-async [socket message succeed fail]))
```

#### -open?

Returns a truthy or falsey value denoting whether the socket is
currently connected to the client.

#### -send

Sends a websocket message frame that may be a `java.lang.CharSequence`
(for text), or a `java.nio.ByteBuffer` (for binary).

#### -send-async

As above, but does not block and requires two callback functions:

- `succeed` is called with zero arguments when the send succeeds
- `fail` is called with a single `java.lang.Throwable` argument when the
  send fails

#### -ping

Sends a websocket ping frame with a `java.nio.ByteBuffer` of session
data (which may be empty).

#### -pong

Sends an unsolicited pong frame with a `java.nio.ByteBuffer` of session
data (which may be empty).

#### -close

Closes the websocket with the supplied integer code and reason string.
