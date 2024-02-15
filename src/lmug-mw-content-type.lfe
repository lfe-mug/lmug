(defmodule lmug-mw-content-type
  (doc "Set content type based upon file extension in URL.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun filename-ext (url)
  "Returns the file extension of a filename or filepath."
  (case (string:split (filename:extension url) ".")
    ('(#"" #"") (default-content-type))
    (`(#"" ,ext) (lmug:ext->content-type ext))
    (_ (default-content-type))))

(defun wrap (handler)
  "The same as #'wrap/2 but with an empty list for options."
  (wrap handler '()))

(defun wrap (handler _opts)
  "Middleware that adds a content type based upon the file extension in the URL."
  (lambda (req)
    (let ((resp (funcall handler req)))
      (http.response:add-header resp
                                #"Content-Type"
                                (filename-ext (mref req 'url))))))
