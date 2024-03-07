(defmodule lmug-fs-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest find-index-any
  (let ((doc-root "priv/testdata"))
    (is-equal "index.html"
              (lmug-fs:find-index-any doc-root ""))
    (is-equal "index.html"
              (lmug-fs:find-index-any doc-root "many"))
    (let ((`#(error ,msg) (lmug-fs:find-index-any doc-root "none")))
      (is-equal "no files for pattern at: priv/testdata/none"
                (lists:flatten msg)))
    (is-equal "index.json"
              (lmug-fs:find-index-any doc-root "nohtml"))))

(deftest find-index-html
  (let ((doc-root "priv/testdata"))
    (is-equal "index.html"
              (lmug-fs:find-index-html doc-root ""))
    (is-equal "index.html"
              (lmug-fs:find-index-html doc-root "many"))
    (let ((`#(error ,msg) (lmug-fs:find-index-html doc-root "none")))
      (is-equal "no files for pattern at: priv/testdata/none"
                (lists:flatten msg)))
    (let ((`#(error ,msg) (lmug-fs:find-index-html doc-root "nohtml")))
      (is-equal "no files for pattern at: priv/testdata/nohtml"
                (lists:flatten msg)))))

(deftest find-index
  (let ((doc-root "priv/testdata"))
    (is-equal "index.html"
              (lmug-fs:find-index doc-root ""))
    (is-equal "index.html"
              (lmug-fs:find-index doc-root "many"))
    (let ((`#(error ,msg) (lmug-fs:find-index doc-root "none")))
      (is-equal "no files for pattern at: priv/testdata/none"
                (lists:flatten msg)))
    (is-equal "index.json"
              (lmug-fs:find-index doc-root "nohtml"))))

(deftest read-dir
  (is-equal '(#(dir? true)
              #(file? false)
              #(symlink? false))
            (lists:sort (maps:to_list (lmug-fs:read "priv/testdata"))))
  (is-equal '(#(dir? true)
              #(file? false)
              #(symlink? false))
            (lists:sort (maps:to_list (lmug-fs:read "priv/testdata/many")))))

(deftest read-dir-no-dir
  (is-equal #m(error "Could not read file; neither diretory, file, nor symlink.")
            (lmug-fs:read "priv/testdata/no/dir")))
  
(deftest read-file
  (is-equal '(#(access read_write)
              #(content #"<html><body>Welcome to the site!</body></html>\n")
              #(dir? false)
              #(file? true)
              #(size 47)
              #(symlink? false)
              #(type regular))
            (filter-data (lmug-fs:read "priv/testdata/index.html")))
  (is-equal '(#(access read_write)
              #(content #"Some notes!\n")
              #(dir? false)
              #(file? true)
              #(size 12)
              #(symlink? false)
              #(type regular))
            (filter-data (lmug-fs:read "priv/testdata/text/notes.txt"))))

;; TODO: figure out symlinks in Erlang/macos ...
;;(deftest read-symlink-skip
;;  (is-equal '(#(access read_write)
;;                   #(content #"Some notes!\n")
;;                   #(dir? false)
;;                   #(file? true)
;;                   #(size 12)
;;                   #(symlink? true)
;;                   #(type symlink))
;;            (lists:sort
;;             (maps:to_list
;;              (maps:without '(mtime) (lmug-fs:read "priv/testdata/many/notes.txt"))))))

(deftest read-no-file
  (is-equal '(#(error "Could not read file; neither diretory, file, nor symlink."))
            (filter-data (lmug-fs:read "priv/testdata/no.file"))))

(deftest walk
  (let ((store (lmug-fs:walk "priv/testdata")))
    (is-equal '(metadata
                #"/htm/index.htm"
                #"/index.html"
                #"/many/index.html"
                #"/many/index.json"
                #"/many/index.txt"
                #"/many/notes.txt"
                #"/nohtml/index.json"
                #"/nohtml/index.txt"
                #"/none/.placeholder"
                #"/text/index.txt"
                #"/text/notes.txt")
              (lists:sort (maps:keys store)))
  (is-equal '(#(access read_write)
              #(content #"Some notes!\n")
              #(dir? false)
              #(file? true)
              #(size 12)
              #(symlink? false)
              #(type regular))
            (filter-data
             (mref store #"/text/notes.txt")))))

;;; Utility test functions

(defun filter-data (read-data)
  (lists:sort
   (maps:to_list
    (maps:without '(mtime read-time path) read-data))))