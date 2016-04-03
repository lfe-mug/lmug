(defmodule lmug
  (doc "General lmug functions and aliases for commonly-used functions
       in other modules.")
  (export all))

(defun response ()
  "Convenience function; alias for ``#'lmug-response:response/0``."
  (lmug-response:response))

(defun response (handler)
  "Convenience function; alias for ``#'lmug-response:response/1``."
  (lmug-response:response handler))

(defun response (handler opts)
  "Convenience function; alias for ``#'lmug-response:response/2``."
  (lmug-response:response handler opts))

(defun request ()
  "Convenience function; alias for ``#'lmug-request:request/0``."
  (lmug-request:request))

(defun request (opts)
  "Convenience function; alias for ``#'lmug-request:request/1``."
  (lmug-request:request opts))
