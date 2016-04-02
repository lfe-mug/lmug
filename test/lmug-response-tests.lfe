(defmodule lmug-response-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lmug/include/response.lfe")

(deftest header
  (is-equal (make-response headers '(#("Foo-Bar" "Baz-Quux")))
            (lmug-response:header (make-response) "Foo-Bar" "Baz-Quux")))

(deftest content-type
  (is-equal (make-response headers '(#("Content-Type" "text/plain")))
            (lmug-response:content-type (make-response) "text/plain")))
