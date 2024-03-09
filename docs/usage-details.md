# Usage Details

## Logging Middleware

``` lisp

```

## Resource Middleware

``` lisp
(defun my-happy-middleware (handler)
  (lambda (req)
    (let ((url-path (mref (mref req 'url-parsed) 'path))
          (resp (funcall handler req)))
      (if (== url-path #"/index.html")
       (lmug-response:set-body resp #"hey!")
       (http.response:new (http.status:not-found) #"404 - Not Found")))))

(set app (clj:-> (lmug:app)
                 (lmug-mw-request-id:wrap)
                 (lmug-mw-content-type:wrap)
                 (my-happy-middleware)
                 (lmug-mw-resource:wrap #m(doc-root "static"
                                           prefer-handler? false
                                           watch? true))
                 (lmug-mw-log-request:wrap #m(log-level notice))))

(lmug-inets:start app `#m(port 5099))
```
