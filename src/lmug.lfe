;;;; This module is really only intended to be used when slurping from the
;;;; LFE REPL.
(defmodule lmug
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "include/request.lfe")
(include-lib "include/response.lfe")

(defun run (handler)
  (lmug-barista-adapter:run handler))