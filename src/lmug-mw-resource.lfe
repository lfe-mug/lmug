(defmodule lmug-mw-resource
  (doc "Serve resource from the filesystem.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2))
  ;; State data API for resource middleware
  (export
   (abs-path 1)
   (default-opts 0)
   (doc-root 0)
   (metadata 0)
   (resource 1)
   (resources 0)
   (rm-resource 1)
   (set-resource 1) (set-resource 2)
   (set-resources 1)
   (url-path 1)))

(include-lib "logjam/include/logjam.hrl")

(defun wrap (handler)
  "The same as #'wrap/2 but with the default options map."
  (wrap handler #m()))

(defun wrap (handler opts)
  "Middleware that can load resources from on-disk instead of a handler.

  For the possible options available, see
  * `lmug-fs:default-response-opts/0`."
  (let* ((opts (maps:merge (default-opts) opts))
         (pre-load? (mref opts 'pre-load?))
         (doc-root (mref opts 'doc-root))
         (watch? (mref opts 'watch?))
         (prefer-handler? (mref opts 'prefer-handler?)))
    (init-state doc-root opts)
    (if pre-load?
      (let ((rsrs (lmug-fs:walk doc-root opts)))
        (set-resources rsrs)
        (if watch?
          (progn
            (log-debug "Watching '~s' ..." (list doc-root))
            (lmug-watcher:start doc-root)))))
    (if prefer-handler?
      (wrap-prefer-handler handler opts)
      (wrap-prefer-resources handler opts))))

(defun wrap-prefer-resources (handler opts)
  "Generate a response for the desired resource; if it is present, serve it.
  If the resource cannot be found, execute the handler chain and return that
  result."
  (lambda (req)
    (log-debug "Preferring static resouces over handler ...")
    (case (file-request handler opts req)
      (`#m(status 404) (funcall handler req))
      (resp resp))))

(defun wrap-prefer-handler (handler opts)
  "Generate a response for the handler; if it does not result in a 404, then
  return it. If it _does_ result in a 404, try finding a static resource that
  matches the passed URL; if that exists, return a response with it. if it does
  not exist, go ahead and return the handler's original 404."
  (lambda (req)
    (log-debug "Preferring handler over static resources ...")
    (let ((handler-resp (funcall handler req)))
      (case handler-resp
        (`#m(status 404)
            (case (file-request handler opts req)
              (`#m(status 404)
                  handler-resp)
              (resource-resp resource-resp)))
        (_ handler-resp)))))

(defun default-opts ()
  `#m(doc-root "."
      allow-symlinks? false
      prefer-handler? false
      pre-load? true
      max-files 10000
      max-file-size ,(math:pow 2 27)
      max-total-bytes ,(math:pow 2 31)
      watch? false))

(defun file-request (handler opts req)
  (let* ((url-path (mref (mref req 'url-parsed) 'path))
         (pre-load? (mref opts 'pre-load?))
         (resp (funcall handler req))
         (res (if pre-load?
                (resource url-path)
                (lmug-fs:read-file (abs-path url-path)))))
    (case res
      (`#m(error not-found) (not-found resp))
      ('undefined (not-found resp))
      (_ (file-response resp req res opts)))))

(defun file-response (resp req resource opts)
  (clj:-> resp
          (maps:merge (http.response:new))
          (lmug-response:add-content-type-from-ext
           (mref req 'url))
          (lmug-response:set-body
           (mref resource 'content)
           (mref resource 'size)
           (mref resource 'mtime))))

(defun not-found (resp)
  (maps:merge
   resp
   (http.response:new (http.status:not-found) #"404 - Not Found")))

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

(defun rm-resource (url-path)
  (set-resources (maps:without `(,url-path) (resources))))

(defun set-resource (url-path)
  (let ((rsrc (lmug-fs:read-file (lmug-fs:abs-path (doc-root) url-path))))
    (case rsrc
      (`#m(error ,_) 'skipping)
      (_  (set-resource url-path rsrc)))))

(defun set-resource (url-path resource)
  (set-resources (maps:put url-path resource (resources))))

(defun set-resources (resources)
  (set-all (maps:put 'resources resources (get-all))))

(defun url-path (file-path)
  (lmug-fs:url-path (doc-root) file-path))
