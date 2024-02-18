(defmodule lmug-filesysteme-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest find-index-any
  (let ((doc-root "priv/testdata"))
    (is-equal "index.html"
              (lmug-filesystem:find-index-any doc-root ""))
    (is-equal "index.html"
              (lmug-filesystem:find-index-any doc-root "many"))
    (let ((`#(error ,msg) (lmug-filesystem:find-index-any doc-root "none")))
      (is-equal "no files for pattern at: priv/testdata/none"
                (lists:flatten msg)))
    (is-equal "index.json"
              (lmug-filesystem:find-index-any doc-root "nohtml"))))

(deftest find-index-html
  (let ((doc-root "priv/testdata"))
    (is-equal "index.html"
              (lmug-filesystem:find-index-html doc-root ""))
    (is-equal "index.html"
              (lmug-filesystem:find-index-html doc-root "many"))
    (let ((`#(error ,msg) (lmug-filesystem:find-index-html doc-root "none")))
      (is-equal "no files for pattern at: priv/testdata/none"
                (lists:flatten msg)))
    (let ((`#(error ,msg) (lmug-filesystem:find-index-html doc-root "nohtml")))
      (is-equal "no files for pattern at: priv/testdata/nohtml"
                (lists:flatten msg)))))

(deftest find-index
  (let ((doc-root "priv/testdata"))
    (is-equal "index.html"
              (lmug-filesystem:find-index doc-root ""))
    (is-equal "index.html"
              (lmug-filesystem:find-index doc-root "many"))
    (let ((`#(error ,msg) (lmug-filesystem:find-index doc-root "none")))
      (is-equal "no files for pattern at: priv/testdata/none"
                (lists:flatten msg)))
    (is-equal "index.json"
              (lmug-filesystem:find-index doc-root "nohtml"))))
