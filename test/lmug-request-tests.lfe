(defmodule lmug-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lmug/include/request.lfe")

(deftest request
  (is-equal (make-request) (lmug-request:request))
  (is-equal (make-request path '(#"ponies"))
            (lmug-request:request '(#(path (#"ponies"))))))
