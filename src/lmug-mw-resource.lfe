(defmodule lmug-mw-resource
  (doc "Serve resource from the filesystem.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun wrap (handler)
  "The same as #'wrap/2 but with the default options map."
  (wrap handler #m()))

(defun wrap (handler opts)
  "Middleware that can load resources from on-disk instead of a handler.

  For the possible options available, see
  * `lmug-filesystem:default-response-opts/0`."
  (lmug-state:start)
  (let* ((opts (maps:merge (default-opts) opts))
         (pre-load? (mref opts 'pre-load?))
         (doc-root (mref opts 'doc-root)))
    (if pre-load?
      (lmug-state:set-resources (lmug-filesystem:walk doc-root)))
    (lambda (req)
      (let* ((filepath "")
             (resp (funcall handler req))
             (res (if pre-load?
                    (lmug-state:get-resource filepath)
                    (lmug-filesystem:read-file filepath))))
        ;; TODO: support the possibility of continuing the response if not found
        ;;       (checking the handler second)
        ;; TODO: support the possibility of checkout the handler first
        (file-response resp req res)))))

(defun default-opts ()
  #m(doc-root #"."
     allow-symlinks? false
     prefer-handler? false
     pre-load? true))

;;(defun prefer-resource ()
;;  ;; TODO: implement
;;  'not-implemented)

;;(defun prefer-handler ()
;;  ;; TODO: implement
;;  'not-implemented)

(defun file-response
  ((resp req `#m(error not-found))
   ;; TODO: support default error pages
   ;; TODO: support user-provided error pages
   (maps:merge
    resp
    (http.response:new (http.status:not-found) #"")))
  ((resp req resource)
   (let ((resp (maps:merge resp (http.response:new))))
     (clj:-> resp
             (lmug-response:add-content-type-from-ext
              (mref req 'url))
             (lmug-response:set-body
              (mref resource 'content) (mref resource 'size))))))
