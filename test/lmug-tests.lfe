(defmodule lmug-tests
  (doc "`lmug-util` unit tests.")
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(deftest response
  (is-equal (make-response) (funcall (lmug:response) #""))
  (is-equal (make-response) (funcall (lmug:response 'ignored) #""))
  (is-equal (make-response status 748)
            (funcall (lmug:response 'ignored `(#(status 748))) #"")))

(deftest request
  (is-equal (make-request) (lmug:request))
  (is-equal (make-request path '(#"ponies")) (lmug:request `(#(path (#"ponies"))))))
