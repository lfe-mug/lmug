(defmodule lmug-response
  (export
   all))

(defun add-header (resp fieldname value)
  (http.response:add-header resp fieldname value))
                                
(defun add-content-type (resp value)
  (add-header resp #"Content-Type" value))

(defun add-content-length (resp value)
  (add-header resp #"Content-Length" value))

(defun add-content-type-from-ext (resp request-url)
  (add-content-type resp (lmug-util:filename-ext request-url)))

(defun set-body (resp bytes)
  (set-body resp bytes (size bytes)))

(defun set-body (resp bytes size)
  (clj:-> resp
          (http.response:set-body bytes)
          (add-content-length size)))