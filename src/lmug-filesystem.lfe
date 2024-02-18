(defmodule lmug-filesystem
  (export
   (find-index 2)
   (find-index-any 2)
   (find-index-html 2)
   (response 1) (response 2)
   ))

(defun response (path)
  (response path #m()))

(defun response (path opts)
  (let ((opts (maps:merge (default-response-opts) opts)))
    'tbd))

(defun default-response-opts ()
  #m())

(defun find-index (doc-root path)
  (case (find-index-html doc-root path)
    (`#(error ,_) (find-index-any doc-root path))
    (file file)))

(defun find-index-html (doc-root path)
  (find-file doc-root path "index.htm*"))

(defun find-index-any (doc-root path)
  (find-file doc-root path "index.*"))

(defun find-file (doc-root path pattern)
  (case (filelib:safe_relative_path path doc-root)
    ('unsafe `#(error ,(io_lib:format "unsafe path: ~s" `(,path))))
    (safesubdir (let ((safepath (filename:join doc-root safesubdir)))
                  (case (filelib:wildcard pattern safepath)
                    ('() `#(error ,(io_lib:format
                                    "no files for pattern at: ~s"
                                    `(,safepath))))
                    (files (car files)))))))
