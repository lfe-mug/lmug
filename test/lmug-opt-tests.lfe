(defmodule lmug-opt-tests
  (doc "`lmug-opt` unit tests.")
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
  (is-equal 3 (lmug-opt:get (test-data) 'c))
  (is-equal '() (lmug-opt:get (test-data) 'z)))

(deftest get-with-defaults
  (is-equal 1 (lmug-opt:get (test-data) 'a 0))
  (is-equal 0 (lmug-opt:get (test-data) 'z 0))
  (is-equal 'undefined (lmug-opt:get (test-data) 'z 'undefined)))
