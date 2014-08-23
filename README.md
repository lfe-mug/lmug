# lmug

*LFE HTTP Server Abstraction and Web Application Middleware Infrastructure*

<img src="resources/images/mugring-small-grey-2.png" />

## Introduction

lmug is an LFE web applications library inspired by
[Clojure's Ring](https://github.com/ring-clojure/ring) (and thus
[Python's WSGI](http://legacy.python.org/dev/peps/pep-3333/)). By
abstracting the details of HTTP into a simple, unified
API, lmug allows web applications to be constructed of modular components
that can be shared among a variety of applications, web servers, and web
frameworks.

The [SPEC](doc/SPEC.md) file, copied directly from the Clojure Ring project,
provides a complete description of the lmug interface.


### Dependencies

This project assumes that you have [rebar](https://github.com/rebar/rebar)
installed somwhere in your ``$PATH``.

This project depends upon the following, which are installed to the ``deps``
directory of this project when you run ``make deps``:

* [LFE](https://github.com/rvirding/lfe) (Lisp Flavored Erlang; needed only
  Wto compile)
* [ltest](https://github.com/lfex/ltest)(needed only to run the unit tests)


## Installation

Just add it to your ``rebar.config`` deps:

```erlang

    {deps, [
        ...
        {lmug, ".*", {git, "git@github.com:lfex/lmug.git", "master"}}
      ]}.
```

And then do the usual:

```bash

    $ rebar get-deps
    $ rebar compile
```

## Usage

NOTE: the libraries referenced in this section have not been created yet.

The following assumes that you have lmud-yaws as a dependency in your
project.

```cl
(defmodule hello-world
  (export all))

(include-file "deps/lmug/include/response.lfe")

(defun handler (request)
  (make-response
    status 200
    headers (#("Content-Type" "text/plain"))
    body "Hello World"))

(run-yaws #'handler/1 `(#(port 8080)))
```


## lmug?

What's with the name? Well, there was lfest ... the web app routing party.
What would be at an LFE routing party? Lots of mugs, I guess. Full of tastey,
hot LFE.

Also, a mug is topologically equivalent to a ring. And lmug even more so.

Besides, lhorse just sounds weird.
