(defmodule unit-lmug-tests
  (behaviour lunit-unit)
  (export all)
  (import
    (from lunit
      (check-failed-assert 2)
      (check-wrong-assert-exception 2))))

(include-lib "deps/lunit/include/lunit-macros.lfe")

(deftest my-adder
  (is-equal 4 (: lmug my-adder 2 2)))
