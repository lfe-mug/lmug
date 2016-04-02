(defmodule lmug-opt-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(defun test-data ()
  '(#(a 1)
    #(b 2)
    #(c 3)))

(deftest get
  (is-equal 1 (lmug-opt:get (test-data) 'a))
  (is-equal 2 (lmug-opt:get (test-data) 'b))
  (is-equal 3 (lmug-opt:get (test-data) 'c)))
