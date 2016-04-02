(defmodule lmug-response
  (export all))

(include-lib "lmug/include/response.lfe")

(defun header (resp header-key header-value)
  "Returns an updated Ring response with the specified header added."
  (set-response-headers
    resp
    (cons `#(,header-key ,header-value)
          (response-headers resp))))

(defun get-header
  "Looks up a header in a Ring response (or request) case insensitively,
  returning the value of the header, or nil if not present."
  ((resp header-key) (when (is_atom header-key))
    (get-header resp (atom_to_list header-key)))
  ((resp header-key)
    (match-header (response-headers resp) header-key)))

(defun content-type (resp content-type)
  "Returns an updated Ring response with the a Content-Type header
  corresponding to the given content-type."
  (header resp "Content-Type" content-type))

(defun match-header (headers header-key)
  "Return the first header that matches the given header-key."
  (car (match-headers headers header-key)))

(defun match-headers (headers sought-key)
  "Return all the headers that match the given header-key."
  (lists:filtermap
    (match-lambda ((`#(,key ,value))
      (case (re:run sought-key key '(caseless #(capture none list)))
        ('nomatch 'false)
        ('match `#(true ,value)))))
    headers))
