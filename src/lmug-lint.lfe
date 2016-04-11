(defmodule lmug-lint
  (doc "TODO: WRITE ME")
  (export (check-request 1) (check-response 1)))

(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")


;;;===================================================================
;;; Clojure-inspired macros
;;;===================================================================

(defmacro every-pred preds
  `(lambda (x) (andalso ,@(lists:map (lambda (pred) `(funcall ,pred x)) preds))))

(defmacro oneof vals
  `(match-lambda ,@(lists:map (lambda (val) `((,val) 'true)) vals)))

(defmacro some-fn preds
  `(lambda (x) (orelse ,@(lists:map (lambda (pred) `(funcall ,pred x)) preds))))


;;;===================================================================
;;; Linting functions
;;;===================================================================

(defun check-request (request)
  "Validate the `request`, throwing an error on violations of the spec."
  (lint (request-server-port request)
    (every-pred #'is_integer/1 #'pos?/1)
    #"server-port :: pos_integer()")
  (lint (request-server-name request) #'is_binary/1
    #"server-name :: binary()")
  (lint (request-remote-addr request)
    (some-fn #'is_binary/1 #'undefined?/1)
    #"remote-addr :: binary() | 'undefined'")
  (lint (request-uri request) #'is_binary/1
    #"uri :: binary()")
  (lint (request-path request)
    (every-pred #'is_list/1 (every? #'is_binary/1))
    #"path :: [binary()]")
  (lint (request-query-string request) #'is_binary/1
    #"query-string :: binary()")
  (lint (request-query-params request)
    (every-pred #'is_list/1 (every? #'bin-key?/1))
    #"query-params :: [{binary(), any()}]")
  (lint (request-form-params request)
    (every-pred #'is_list/1 (every? #'bin-key?/1))
    #"form-params :: [{binary(), any()}]")
  (lint (request-scheme request)
    (oneof 'http 'https)
    #"scheme :: 'http' | 'https'")
  (lint (request-method request)
    (oneof 'options 'get 'head 'post 'put 'delete 'trace)
    #"method :: 'options' | 'get' | 'head' | 'post' | 'put' | 'delete' | 'trace'")
  (lint (request-protocol request)
    (match-lambda
      (((binary "HTTP/" (version binary)))
       (case (binary:split version #".")
         ('(#"0" #"9") 'true)
         ('(#"1" #"0") 'true)
         ('(#"1" #"1") 'true)
         ('(#"2" #"0") 'true)
         ('(#"2")      'true)
         (_            'false))))
    #"protol :: binary()")
  ;; TODO: Update this and the spec.
  (lint (request-ssl-client-cert request)
    (lambda (val) (orelse (is_binary val) (=:= 'undefined val)))
    #"ssl-client-cert :: binary() | 'undefined'")
  (let ((headers (request-headers request))
        (message #"headers :: [{binary(), binary() | [binary()]}]"))
    (lint headers #'is_list/1 message)
    (lists:foreach
      (match-lambda
        ((`#(,hname ,hval))
         (lint hname #'is_binary/1 message)
         (lint hname
           (lambda (val)
             (=:= val (bc ((<= c val)) ((string:to_lower c) integer))))
           message)
         (lint hval #'is_binary/1 message)))
      headers)
    #"form-params :: [{binary(), any()}]")
  (lint (request-body request)
    (lambda (val) (orelse (is_binary val) (is_integer (iolist_size val))))
    #"body :: binary() | iolist()")
  ;; TODO: maybe lint orig
  (lint (request-mw-data) #'is_list/1
    #"ssl-client-cert :: binary() | 'undefined'")
  'ok)

(defun check-response (response)
  "Validate the `response`, throwing an error on violations of the spec."
  (lint (response-status response)
    (lambda (val) (andalso (is_integer val) (>= val 100)))
    #"status :: integer() % Status >= 100")
  (let ((headers (response-headers response))
        (message #"headers :: [{binary(), binary()]}"))
    (lint headers #'is_list/1 message)
    (lists:foreach
      (match-lambda
        ((`#(,hname ,hval))
         (lint hname #'is_binary/1 message)
         (lint hval (some-fn #'is_binary/1 (every? #'is_binary/1)) message)))
      headers)))

(defun lint (val spec message)
  (try
    (if (funcall spec val) 'ok (error `#(lmug-lint ,message ,val)))
    (catch
      (`#(error ,(= `#(lmug-lint ,_ ,_) e) ,_) (error e))
      (`#(,_ ,_ ,_)
       ;; TODO: Add error to the thrown tuple
       (error `#(lmug-lint ,message ,val))))))


;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun bin-key? ((`#(,bin ,_)) (when (is_binary bin)) 'true))

(defun every? (pred) (lambda (list) (lists:all pred list)))

(defun pos? (val) (> val 0))

(defun undefined? (val) (=:= 'undefined val))
