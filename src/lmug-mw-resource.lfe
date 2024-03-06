(defmodule lmug-mw-resource
  (doc "Serve resource from the filesystem.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2))
  ;; State data API for resource middleware
  (export
   (doc-root 0)
   (metadata 0)
   (resource 1)
   (resources 0)
   (set-resource 2)
   (set-resources 1)))

(include-lib "logjam/include/logjam.hrl")

(defun wrap (handler)
  "The same as #'wrap/2 but with the default options map."
  (wrap handler #m()))

(defun wrap (handler opts)
  "Middleware that can load resources from on-disk instead of a handler.

  For the possible options available, see
  * `lmug-filesystem:default-response-opts/0`."
  (let* ((opts (maps:merge (default-opts) opts))
         (pre-load? (mref opts 'pre-load?))
         (doc-root (mref opts 'doc-root))
         (watch? (mref opts 'watch?)))
    (init-state doc-root opts)
    (if pre-load?
      (let ((rsrs (lmug-filesystem:walk doc-root opts)))
        (set-resources rsrs)
        (if watch?
          (progn
            (log-debug "Watching '~s' ..." (list doc-root))
            (lmug-watcher:start doc-root)))))
    (lambda (req)
      (let* ((url-path (mref (mref req 'url-parsed) 'path))
             (resp (funcall handler req))
             (res (if pre-load?
                    (resource url-path)
                    (lmug-fs:read-file (abs-path url-path)))))
        ;; TODO: support the possibility of continuing the response if not found
        ;;       (checking the handler second)
        ;; TODO: support the possibility of checking the handler first
        (file-response resp req res)))))

(defun default-opts ()
  `#m(doc-root "."
      allow-symlinks? false
      prefer-handler? false
      pre-load? true
      max-files 10000
      max-file-size ,(math:pow 2 27)
      max-total-bytes ,(math:pow 2 31)
      watch? false))

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
              (mref resource 'content)
              (mref resource 'size)
              (mref resource 'mtime))))))

;;; lmug State additions for resource middleware

(defun *state-key* () (MODULE))

(defun *gen-server* () (lmug-state:server-name))

(defun default-state (doc-root opts)
  "This map represents the state data that the lmug-mw-resource adds to the
  lmug-state gen_server."
  `#m(doc-root ,(filename:absname doc-root)
      ;; the definitive set of resources, a map of URL paths to
      ;; file data; in addition to keys for each file path, there
      ;; is a key for the metadata of all the resources:
      resources #m(metadata #m()) 
      opts ,opts))

(defun abs-path (url-path)
  (lmug-fs:abs-path (doc-root) url-path))

(defun get-all ()
  (lmug-state:get (*state-key*)))

(defun init-state (doc-root opts)
  (set-all (default-state doc-root opts)))

(defun doc-root ()
  (lmug-state:get-in `(,(*state-key*) opts doc-root)))

(defun metadata ()
  (lmug-state:get-in `(,(*state-key*) resources metadata)))

(defun opts ()
  (lmug-state:get-in `(,(*state-key*) opts)))

(defun resource (url-path)
  (lmug-state:get-in `(,(*state-key*) resources ,url-path)))

(defun resources ()
  (lmug-state:get-in `(,(*state-key*) resources)))

(defun set-all (state)
  (gen_server:cast (*gen-server*) `#(set ,(*state-key*) ,state)))

(defun set-resource (url-path)
  (set-resource url-path
                (lmug-filesystem:read-file (lmug-filesystem:abs-path (doc-root)
                                                                     url-path))))

(defun set-resource (url-path resource)
  (set-resources (maps:put url-path resource (resources))))

(defun set-resources (resources)
  (set-all (maps:put 'resources resources (get-all))))

(defun url-path (file-path)
  (lmug-fs:url-path (doc-root) file-path))
