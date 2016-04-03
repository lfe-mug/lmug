(defmodule lmug-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(deftest response
  (is-equal (make-response) (funcall (lmug:response) '()))
  (is-equal (make-response) (funcall (lmug:response 'ignored) '()))
  (is-equal (make-response status 748)
            (funcall (lmug:response 'ignored `(#(status 748))) '())))
