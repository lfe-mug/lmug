(defmodule lmug-mw-content-type-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Test data
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun test-header-1 ()
  '(#(#"Foo-Bar" #"Baz-Quux")))

(defun test-header-2 ()
  '(#(#"Content-Type" #"text/plain")))

(defun test-headers ()
  (++ (test-header-1)
      (test-header-2)))

(defun resp-a ()
  (make-response headers (test-header-1)))

(defun resp-b ()
  (make-response headers (test-header-2)))

(defun resp-c ()
  (make-response headers (++ (test-header-1) (test-header-2))))

(defun req-1 ()
  (make-request uri #"http://localhost/file.json"))

(defun req-2 ()
  (make-request uri #"http://localhost/file."))

(defun req-3 ()
  (make-request uri #"http://localhost/file"))

(defun req-4 ()
  (make-request uri #"http://localhost/.json"))

(defun req-5 ()
  (make-request uri #".json"))

(defun req-6 ()
  (make-request uri #"json"))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Unit tests
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(deftest add-content-type-content-type-not-set
  (let ((resp (lmug-mw-content-type:add-content-type (req-1) (make-response))))
    (is-equal #"application/json" (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-2) (make-response))))
    (is-equal #"application/octet-stream"
              (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-3) (make-response))))
    (is-equal #"application/octet-stream"
              (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-4) (make-response))))
    (is-equal #"application/json" (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-5) (make-response))))
    (is-equal #"application/json" (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-6) (make-response))))
    (is-equal #"application/octet-stream"
              (lmug-response:get-header resp 'content-type))))

(deftest add-content-type-content-type-set
  (let ((resp (lmug-mw-content-type:add-content-type (req-1) (resp-c))))
    (is-equal #"text/plain" (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-2) (resp-c))))
    (is-equal #"text/plain"
              (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-3) (resp-c))))
    (is-equal #"text/plain"
              (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-4) (resp-c))))
    (is-equal #"text/plain" (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-5) (resp-c))))
    (is-equal #"text/plain" (lmug-response:get-header resp 'content-type)))
  (let ((resp (lmug-mw-content-type:add-content-type (req-6) (resp-c))))
    (is-equal #"text/plain"
              (lmug-response:get-header resp 'content-type))))

(deftest wrap
  (let ((handler (lmug-mw-content-type:wrap (lmug:response))))
    (is-equal (make-response headers '(#(#"Content-Type" #"application/json")))
              (funcall handler (req-1)))
    (is-equal (make-response headers '(#(#"Content-Type" #"application/octet-stream")))
              (funcall handler (req-2)))
    (is-equal (make-response headers '(#(#"Content-Type" #"application/octet-stream")))
              (funcall handler (req-3)))
    (is-equal (make-response headers '(#(#"Content-Type" #"application/json")))
              (funcall handler (req-4)))
    (is-equal (make-response headers '(#(#"Content-Type" #"application/json")))
              (funcall handler (req-5)))
    (is-equal (make-response headers '(#(#"Content-Type" #"application/octet-stream")))
              (funcall handler (req-6)))))
