(defmodule lmug-mw-content-type
  (doc "Set content type based upon file extension in URL.")
  (behaviour lmug-middleware)
  (export
   (wrap 1) (wrap 2)))

(defun default-content-type () (http.mimetype:text/html))

(defun ext-content-types ()
  "A map of file extensions to mime-types."
  #m("7z"       "application/x-7z-compressed"
     "aac"      "audio/aac"
     "ai"       "application/postscript"
     "appcache" "text/cache-manifest"
     "asc"      "text/plain"
     "atom"     "application/atom+xml"
     "avi"      "video/x-msvideo"
     "bin"      "application/octet-stream"
     "bmp"      "image/bmp"
     "bz2"      "application/x-bzip"
     "class"    "application/octet-stream"
     "cer"      "application/pkix-cert"
     "crl"      "application/pkix-crl"
     "crt"      "application/x-x509-ca-cert"
     "css"      "text/css"
     "csv"      "text/csv"
     "deb"      "application/x-deb"
     "dart"     "application/dart"
     "dll"      "application/octet-stream"
     "dmg"      "application/octet-stream"
     "dms"      "application/octet-stream"
     "doc"      "application/msword"
     "dvi"      "application/x-dvi"
     "edn"      "application/edn"
     "eot"      "application/vnd.ms-fontobject"
     "eps"      "application/postscript"
     "etx"      "text/x-setext"
     "exe"      "application/octet-stream"
     "flv"      "video/x-flv"
     "flac"     "audio/flac"
     "gif"      "image/gif"
     "gz"       "application/gzip"
     "htm"      "text/html"
     "html"     "text/html"
     "ico"      "image/x-icon"
     "iso"      "application/x-iso9660-image"
     "jar"      "application/java-archive"
     "jpe"      "image/jpeg"
     "jpeg"     "image/jpeg"
     "jpg"      "image/jpeg"
     "js"       "text/javascript"
     "json"     "application/json"
     "lha"      "application/octet-stream"
     "lzh"      "application/octet-stream"
     "mov"      "video/quicktime"
     "m3u8"     "application/x-mpegurl"
     "m4v"      "video/mp4"
     "mjs"      "text/javascript"
     "mp3"      "audio/mpeg"
     "mp4"      "video/mp4"
     "mpd"      "application/dash+xml"
     "mpe"      "video/mpeg"
     "mpeg"     "video/mpeg"
     "mpg"      "video/mpeg"
     "oga"      "audio/ogg"
     "ogg"      "audio/ogg"
     "ogv"      "video/ogg"
     "pbm"      "image/x-portable-bitmap"
     "pdf"      "application/pdf"
     "pgm"      "image/x-portable-graymap"
     "png"      "image/png"
     "pnm"      "image/x-portable-anymap"
     "ppm"      "image/x-portable-pixmap"
     "ppt"      "application/vnd.ms-powerpoint"
     "ps"       "application/postscript"
     "qt"       "video/quicktime"
     "rar"      "application/x-rar-compressed"
     "ras"      "image/x-cmu-raster"
     "rb"       "text/plain"
     "rd"       "text/plain"
     "rss"      "application/rss+xml"
     "rtf"      "application/rtf"
     "sgm"      "text/sgml"
     "sgml"     "text/sgml"
     "svg"      "image/svg+xml"
     "swf"      "application/x-shockwave-flash"
     "tar"      "application/x-tar"
     "tif"      "image/tiff"
     "tiff"     "image/tiff"
     "ts"       "video/mp2t"
     "ttf"      "font/ttf"
     "txt"      "text/plain"
     "wasm"     "application/wasm"
     "webm"     "video/webm"
     "webp"     "image/webp"
     "wmv"      "video/x-ms-wmv"
     "woff"     "font/woff"
     "woff2"    "font/woff2"
     "xbm"      "image/x-xbitmap"
     "xls"      "application/vnd.ms-excel"
     "xlsx"     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
     "xml"      "text/xml"
     "xpm"      "image/x-xpixmap"
     "xwd"      "image/x-xwindowdump"
     "zip"      "application/zip"))

(defun filename-ext (url)
  "Returns the file extension of a filename or filepath."
  (case (string:split (filename:extension url) ".")
    ('(#"" #"") (default-content-type))
    (`(#"" ,ext) (maps:get (string:to_lower (binary_to_list ext))
                           (ext-content-types)
                           (default-content-type)))
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
