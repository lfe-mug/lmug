(defmodule unit-lmug-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest no-op
  (is 'true))
