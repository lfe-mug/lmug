# lmug

*LFE HTTP Server Abstraction and Web Application Middleware Infrastructure*

## Introduction

TBD


### Dependencies

This project assumes that you have [rebar]() installed somwhere in your
``$PATH``.

This project depends upon the following, which are installed to the ``deps``
directory of this project when you run ``make deps``:

* [LFE]() (Lisp Flavored Erlang; needed only to compile)
* [lunit]()(needed only to run the unit tests)


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

Add content to me here!
