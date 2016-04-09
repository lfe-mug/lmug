# lmug

[![img](https://travis-ci.org/lfe-mug/lmug.svg)](https://travis-ci.org/lfe-mug/lmug)
[![img](https://img.shields.io/github/tag/lfe-mug/lmug.svg)](https://github.com/lfe-mug/lmug/releases/latest)
[![img](https://img.shields.io/badge/erlang-%E2%89%A5R16B03-red.svg)](http://www.erlang.org/downloads)
[![img](https://img.shields.io/badge/docs-69%25-green.svg)](http://lfe-mug.github.io/lmug)
[![img](https://img.shields.io/badge/license-Apache-blue.svg)](LICENSE)

[![][lmug-logo]][lmug-logo-large]

[lmug-logo]: resources/images/mugring-small-grey-3.png
[lmug-logo-large]: resources/images/mugring-large-grey-3.png

*LFE HTTP Server Abstraction and Web Application Middleware
Infrastructure*


#### Contents

* [Introduction](#introduction-)
  * [Why?](#why-)
* [Installation](#installation-)
* [Documentation](#documentation-)
* [Usage](#usage-)
  * [Simple Example](#simple-example-)
  * [Applications](#applications-)
* [Details](#details-)
  * [Handlers](#handlers-)
  * [Requests](#requests-)
  * [Responses](#responses-)
  * [Middleware](#middleware-)
  * [Adaptors](#adaptors-)
* [The Name?](#the-name-)


## Introduction [&#x219F;](#contents)

lmug is an LFE web applications library inspired by
[Clojure's Ring](https://github.com/ring-clojure/ring) (and thus,
indirectly, by
[Python's WSGI](http://legacy.python.org/dev/peps/pep-3333/)).

lmug allows web applications to be constructed of modular components
that can be shared among a variety of applications -- regardless of
which web server or web framework is used. This is possible by providing
a standard abstraction for two things:

* Erlang/LFE/BEAM HTTP servers, and
* functions that can sit between the HTTP request and the HTTP response.

The [SPEC](docs/SPEC.md) file, copied directly from the Clojure Ring
project, provides a complete description of the lmug interface.


### Why? [&#x219F;](#contents)

Using lmug as the basis for your web application has a number of
benefits:

* Write your application using LFE functions and records
* Run your application in a auto-reloading development server
* Take advantage of pre-written middleware
* More easily deploy your application in cloud environments like Amazon
  Elastic Beanstalk and Heroku
* Swap out the underlying HTTP server without changing any of your code

If all goes well, lmug will be the de facto standard, the base upon
which one will write web applications in LFE. Higher level frameworks could
then use lmug as a platform.

Even though lmug provides only a low-level interface, it is useful to
understand how it works even if you plan to use a higher-level interface.
Without a basic understanding of lmug, you cannot write middleware, and
you may find debugging your application more difficult.


## Installation [&#x219F;](#contents)

Just add it to your ``rebar.config`` deps:

```erlang
{deps, [
    ...
    {lmug, {git, "https://github.com/lfe-mug/lmug.git", {tag, "0.0.3"}}}
  ]}.
```

And then do the usual:

```bash
$ rebar3 compile
```


## Documentation [&#x219F;](#contents)

* The [lmug spec](docs/SPEC.md) - based on the Clojure [Ring spec](https://github.com/ring-clojure/ring/blob/master/SPEC)
* The [lmug API Reference](http://lfe-mug.github.io/lmug/current/api)


## Usage [&#x219F;](#contents)

The usage examples below are done from the REPL:

```bash
$ make repl
```


### Simple Example [&#x219F;](#contents)

Ordinarily you would use lmug middleware in a project that was running a
supported web server and which included the lmug adaptor for that web server.
Below is an example showing similar to what you would have in a lmug web
application. If you are familiar with Clojure's Ring, then this will look
*very* familiar (though with a Lisp-2 flavor ...):

```lisp
(include-lib "clj/include/compose.lfe")

(defun identity-mw (handler)
  "An identity middleware handler."
  handler)

(set app (-> (lmug:response)
             (identity-handler)
             (lmug-mw-content-type:wrap)
             (identity-handler)))
```

Then, to run it, simply do the following:

```lisp
> (funcall app (lmug:request '(#(uri "http://localhost/file.json"))))
#(response 200 (#("Content-Type" "application/json")) ())
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
TBD
```

## Details [&#x219F;](#contents)

At its core, an lmug web application consists of five components:

* Handler
* Request
* Response
* Middleware
* Adaptor


### Handlers [&#x219F;](#contents)

Handlers are functions that define your web application. They take one 
argument, a record representing an HTTP request, and return a record 
representing an HTTP response.

Here's a simple example handler that takes a request and sets the
response body to be the client's IP address:

```lisp
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(defun my-ip?
  (((match-request remote-addr ip))
    (make-response status 200
                   headers '(#("Content-Type" "text/plain"))
                   body ip)))
```

This function returns a response record that lmug adaptors can then translate into HTTP server-specific responses (e.g., YAWS, Elli, Cowboy, etc.). In the above example, the response returns a plain text file that contains the IP address that was used to access the web application.

The handler function can then be converted into a web application through a variety of different methods which will be covered in the next section.


### Requests [&#x219F;](#contents)

As previously mentioned, lmug HTTP requests are represented by LFE records. Though the record fields are fixed, lmug provides a ``mw-data`` field for use by middleware, a proplist that middleware can update with its own keys and values. To see the list of supported request fields, be sure to read the [lmug spec](docs/SPEC.md)


### Responses [&#x219F;](#contents)

The lmug response record is created by a handler and contains three fields:

* ``status`` - The HTTP status code, such as 200, 302, 404 etc.
* ``headers`` - An LFE proplist of HTTP header names to header values.
* ``body`` - A representation of the response body, if a response body is appropriate for the response's status code.


### Middleware [&#x219F;](#contents)

lmug middleware are modules that implement the lmug middleware behaviour.
The lmug middleware behaviour defines two callback functions:

* ``wrap/1`` - takes a handler as an argument
* ``wrap/2``- takes a handler and a proplist of middleware-specific options
  (or any other argument a middleware author may want to pass into the wrap
  function)

The ``wrap`` callback functions are higher-level functions that add additional
functionality to handlers. The return value of the ``wrap`` functions should 
be a new handler function. For most middleware, the new handler function will 
call the original handler.

Here is a simple example:

```lisp
(defmodule content-type
  (behaviour lmug-mw)
  (export (wrap 1)))

(include-lib "clj/include/compose.lfe")

(defun wrap (handler content-type)
  (lambda (request)
    (-> request
        (handler)
        (lmug-response:content-type content-type))))
```

This middleware function adds a "Content-Type" header to every response generated by the handler.

To apply this middleware to a handler:

```lisp
(defun app ()
  (content-type:wrap another-handler "text/html"))
```

### Adaptors [&#x219F;](#contents)

lmug adaptors are what (will) allow developers to write a single web application
and then run them on multiple (supported) web servers. In a nutshell, lmug
adaptors transform:

* a web server's request into an lmug request record, 
* lmug's reponse record into a web server's response data format, and
* lmug handlers into the entry-point functions of a specific web server

Currently the following adaptors are being developed (status given in
brackets):

* Barista (an LFE web server that wraps inets http) - [ALPHA]
* OTP inets http - [IN DEVELOPMENT]
* YAWS - [IN DEVELOPMENT]
* Elli - [IN DEVELOPMENT]
* Cowboy - [NOT STARTED]


## The Name? [&#x219F;](#contents)

What's with the name? Well, there was lfest ... the web app routing
party. What would be at an LFE routing party? Lots of mugs, I guess.
Full of tastey, hot LFE.

Also, a mug is topologically equivalent to a ring. An lmug even more so.

Besides, lhorse just sounds weird.
