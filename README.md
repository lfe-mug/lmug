# lmug

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tags][github-tags-badge]][github-tags]

*LFE HTTP Server Abstraction and Web Application Middleware Infrastructure*

[![Project Logo][logo]][logo-large]

#### Contents

* [Introduction](#introduction-)
  * [Why?](#why-)
* [Installation](#installation-)
* [Documentation](#documentation-)
* [Usage](#usage-)
  * [Simple Example](#simple-example-)
  * [Applications](#applications-)
* [More Details](#more-details-)
* [The Name?](#the-name-)

## Introduction [&#x219F;](#contents)

lmug is an LFE web applications library inspired by
[Clojure's Ring](https://github.com/ring-clojure/ring).

lmug allows web applications to be constructed of modular components
that can be shared among a variety of applications -- regardless of
which web server, web framework, or HTTP client is used. This is possible by providing
standard abstractions for:

* HTTP requests and responses for Erlang/LFE/BEAM HTTP servers and clients - these are provided by and defined in the [LFE HTTP library](https://github.com/lfe-http/http)
* Functions that transform a request into a response (handlers)
* Higher order funtions that take transformers (handlers) as arguments and return a new, wrapped transformer (these are middleware)
* Functions that start a server/listener and pass a handler, potentially handler chain of middleware (adapters)

For an overview, be sure to read the [core concepts document](docs/core-concepts.md). For more details, see the [lmug specification](docs/lmug-spec.md).

### Why? [&#x219F;](#contents)

Using lmug as the basis for your web application has a number of
benefits:

* Write your application using LFE functions and maps
* Take advantage of pre-written middleware
* More easily deploy your application across different environmentsu
* Swap out the underlying HTTP server without changing any of your code

If all goes well, lmug will be the de facto standard, the base upon which one will write web applications in LFE. Higher level frameworks could then use lmug as its HTTP foundation.

## Installation [&#x219F;](#contents)

Just add it to your ``rebar.config`` deps:

```erlang
{deps, [
    ...
    {lmug, "0.2.8}
  ]}.
```

And then do the usual:

```bash
rebar3 compile
```

## Documentation [&#x219F;](#contents)

* The [lmug spec](docs/lmug-spec.md) - based on the Clojure [Ring spec](https://github.com/ring-clojure/ring/blob/master/SPEC)

## Usage [&#x219F;](#contents)

The usage examples below are done from the REPL:

```bash
rebar3 lfe repl
```

### Simple Example [&#x219F;](#contents)

Ordinarily you would use lmug middleware in a project that was running a
supported web server and which included the lmug adaptor for that web server.
Below is an example showing something similar to what you would have in an
lmug web application: it demonstrates the use of multiple middleware modules (the
no-op/identify middleware is used as filler, to help demonstrate more middleware).
If you are familiar with Clojure's Ring library, then this will look *very*
familiar (though with a Lisp-2 flavour ...):

```lisp
lfe> (set app (clj:-> (lmug:app)
                      (lmug-mw-request-id:wrap)
                      (lmug-mw-content-type:wrap)
                      (lmug-mw-status-body:wrap)))
```

Then, to run it, simply do the following:

```lisp
lfe> (funcall app (http.request:new "http://localhost/tune.mp3"))
```

If you'd like to test middleware that requires more info in the request, you'll need to set that up. Let's take the request logger as an example. First, start up the logger:

``` lisp
lfe> (logjam:set-dev-config)
lfe> (application:ensure_all_started 'logjam)
```

Set up the app with the logger middleware:

```lisp
lfe> (set app (clj:-> (app)
                      (lmug-mw-log-request:wrap #m(log-level notice))))
```

Now let's add details to the request that the logger will pull out:

```lisp
lfe> (set req
      (http.request:new
       'get
       #"http://alice:sekr1t@localhost/tune.mp3"
       #""
       #m(#"user-agent" #"LFE REPL")))
lfe> (set req (mupd req 'remote-addr #"http://client.host"))
lfe> (funcall app req)
#M(status 200
   headers
     #M(#"Content-Type" #"audio/mpeg"
        #"X-Request-ID" #"116765989140096673677588780992994213888")
   body #"200")
```

### Applications [&#x219F;](#contents)

The example usage above shows how one can chain together, but it doesn't
illustrate real-world usage. The lmug library is meant to be used in
conjunction with other lmug libraries (e.g., middleware for converting the
body of a response to JSON) and web server adaptors (which allow you to write
a single application that is runnable on any supported web server).

The simplest lmug adaptor is for the Erlang OTP http server. Here's an
example of an lmug application running on OTP inets/httpd:

```lisp
lfe> (set app (-> (lmug:response)
                  (lmug-mw-request-id:wrap)
                  (lmug-mw-content-type:wrap)))
lfe> (set opts '(#(server_name "lmuginets")
                 #(port 5099)))
lfe> (lmug-inets:start app opts)
```

Note that `lmug-inets` is from a [separate project](https://github.com/lfe-mug/lmug-inets).

### More Details [&#x219F;](#contents)

For a closer look at LFE examples, be sure read the [usage details doc](./doc/usage-details).

## The Name? [&#x219F;](#contents)

What's with the name? Well, there was lfest ... the web app routing
party. What would be at an LFE routing party? Lots of mugs, I guess.
Full of tastey, hot LFE.

Really, though, a mug is topologically equivalent to a ring.

An lmug even more so.


[//]: ---Named-Links---

[logo]: resources/images/mugring-small-grey-3.png
[logo-large]: resources/images/mugring-large-grey-3.png
[gh-actions-badge]: https://github.com/lfe-mug/lmug/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfe-mug/lmug/actions
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[versions]: https://github.com/lfe-mug/lmug/blob/master/.github/workflows/cicd.yml
[github-tags]: https://github.com/lfe-mug/lmug/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfe-mug/lmug.svg
