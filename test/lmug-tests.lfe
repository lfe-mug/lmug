(defmodule lmug-tests
  (doc "`lmug-util` unit tests.")
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest tbd
  (is-equal 1 1))
