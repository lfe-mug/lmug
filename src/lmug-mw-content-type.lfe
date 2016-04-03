(defmodule lmug-mw-content-type
  (export all))

(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty list for options."
  (wrap handler '()))

(defun wrap (handler opts)
  "Middleware that adds a content-type header to the response if one is not
  set by the handler. Uses ``#'lmug-util:ext->mime-type/1`` to guess the
  content-type from the file extension in the URI. If no content-type can be
  found, it defaults to ``application/octet-stream``.

  Accepts the following options:

  'mime-types - a proplist of filename extensions to mime-types that will be
                used in addition to the ones retiurned by
                ``lmug-util:get-default-mime-types/0``. For the proper
                formatting of the data, see the source for the
                ``get-default-mime-types`` function mentioned above."
  (lambda (req)
    (add-content-type
      req
      (funcall handler req)
      opts)))

(defun add-content-type (req resp)
  "The same as #'add-content-type/3 but with an empty list for options."
  (add-content-type req resp '()))

(defun add-content-type
  "Adds a content-type header to the response. Used by ``#'wrap/2``.
  Returns a ``response`` record."
  (((match-request uri uri) resp opts)
    (case (lmug-response:get-header resp "Content-Type")
      ("" (let* ((overrides (lmug-opt:get opts 'mime-types))
                 (mime-type (lmug-util:ext->mime-type uri overrides)))
            (lmug-response:content-type resp mime-type)))
      (_ resp))))
