(defmodule lmug-unit-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest ext->mime-type-different-data-types
  (is-equal "application/x-7z-compressed" (lmug-util:ext->mime-type '7z))
  (is-equal "application/x-7z-compressed" (lmug-util:ext->mime-type "7z")))

(deftest ext->mime-type-check-lookup
  (is-equal "text/plain" (lmug-util:ext->mime-type 'asc))
  (is-equal "image/bmp" (lmug-util:ext->mime-type 'bmp))
  (is-equal "text/css" (lmug-util:ext->mime-type 'css))
  (is-equal "application/octet-stream" (lmug-util:ext->mime-type 'dmg))
  (is-equal "image/gif" (lmug-util:ext->mime-type 'gif))
  (is-equal "image/jpeg" (lmug-util:ext->mime-type 'jpg))
  (is-equal "image/png" (lmug-util:ext->mime-type 'png))
  (is-equal "application/rss+xml" (lmug-util:ext->mime-type 'rss))
  (is-equal "image/tiff" (lmug-util:ext->mime-type 'tif))
  (is-equal "application/zip" (lmug-util:ext->mime-type 'zip)))

(deftest ext->mime-type-override
  (is-equal "text/asciidoc"
            (lmug-util:ext->mime-type 'asc '(#(asc "text/asciidoc")))))
