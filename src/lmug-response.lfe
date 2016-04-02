(defmodule lmug-response
  (export all))

(include-lib "lmug/include/response.lfe")

(defun header (resp header-key header-value)
  "Returns an updated Ring response with the specified header added."
  (set-response-headers
    resp
    (cons `#(,header-key ,header-value)
          (response-headers resp))))

(defun content-type (resp content-type)
  "Returns an updated Ring response with the a Content-Type header
  corresponding to the given content-type."
  (header resp "Content-Type" content-type))
