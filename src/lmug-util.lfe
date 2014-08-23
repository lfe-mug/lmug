(defmodule lmug-util
  (export all))

(defun get-lmug-version ()
  (lutil:get-app-src-version "src/lmug.app.src"))

(defun get-version ()
  (++ (lutil:get-version)
      `(#(lmug ,(get-lmug-version)))))
