;;;; This module is really only intended to be used when slurping from the
;;;; LFE REPL.
(defmodule lmug
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

