(defmodule lmug-response-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lmug/include/response.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Test data
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun test-header-1 ()
  '(#("Foo-Bar" "Baz-Quux")))

(defun test-header-2 ()
  '(#("Content-Type" "text/plain")))

(defun test-headers ()
  (++ (test-header-1)
      (test-header-2)))

(defun test-response ()
  (make-response headers (test-headers)))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Unit tests
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(deftest header
  (is-equal (make-response headers (test-header-1))
            (lmug-response:header (make-response) "Foo-Bar" "Baz-Quux")))

(deftest content-type
  (is-equal (make-response headers (test-header-2))
            (lmug-response:content-type (make-response) "text/plain")))

(deftest get-header
  (is-equal ""
            (lmug-response:get-header (test-response) ""))
  (is-equal ""
            (lmug-response:get-header (test-response) "No-Such-Header"))
  (is-equal "Baz-Quux"
            (lmug-response:get-header (test-response) "Foo-Bar"))
  (is-equal "Baz-Quux"
            (lmug-response:get-header (test-response) "foo-bar"))
  (is-equal "Baz-Quux"
            (lmug-response:get-header (test-response) "FOO-BAR"))
  (is-equal "text/plain"
            (lmug-response:get-header (test-response) "content-type")))

(deftest match-header
  (is-equal "Baz-Quux"
            (lmug-response:match-header (test-headers) "Foo-Bar"))
  (is-equal "Baz-Quux"
            (lmug-response:match-header (test-headers) "foo-bar"))
  (is-equal "Baz-Quux"
            (lmug-response:match-header (test-headers) "FOO-BAR"))
  (is-equal "text/plain"
            (lmug-response:match-header (test-headers) "content-type")))

(deftest match-headers
  (is-equal '("Baz-Quux")
            (lmug-response:match-headers (test-headers) "Foo-Bar"))
  (is-equal '("Baz-Quux")
            (lmug-response:match-headers (test-headers) "foo-bar"))
  (is-equal '("Baz-Quux")
            (lmug-response:match-headers (test-headers) "FOO-BAR"))
  (is-equal '("text/plain")
            (lmug-response:match-headers (test-headers) "content-type")))
