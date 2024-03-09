(defmodule lmug-tests
  (doc "`lmug-util` unit tests.")
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest auto-content-type
  (let ((app (clj:-> (lmug:app)
                     (lmug-mw-content-type:wrap))))
    (let ((`#m(status ,s headers ,h body ,b) (do-req app "http://localhost/tune.mp3")))
      (is-equal #"audio/mpeg" (mref h #"Content-Type")))
    (let ((`#m(status ,s headers ,h body ,b) (do-req app "http://localhost/movie.mp4")))
      (is-equal #"video/mp4" (mref h #"Content-Type")))
    (let ((`#m(status ,s headers ,h body ,b) (do-req app "http://localhost/file.txt")))
      (is-equal #"text/plain" (mref h #"Content-Type")))
    (let ((`#m(status ,s headers ,h body ,b) (do-req app "http://localhost/page.html")))
      (is-equal #"text/html" (mref h #"Content-Type")))
    (let ((`#m(status ,s headers ,h body ,b) (do-req app "http://localhost/page")))
      (is-equal #"text/plain" (mref h #"Content-Type")))))

(defun do-req (app url)
  (funcall app (http.request:new url)))