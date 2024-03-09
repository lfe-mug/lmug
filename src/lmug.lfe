(defmodule lmug
  (doc "General lmug functions and aliases for commonly-used functions
       in other modules.")
  (export all))

(defun app ()
  ""
  ;; TODO: make this configurable
  (logjam:set-dev-config)
  (application:ensure_all_started 'logjam)
  (lmug-state:start)
  (lambda (_)
    (http.response:new)))

(defun body
  "The lmug library and adapters standardise on binary data for HTTP bodies."
  ((b) (when (is_binary b))
   b)
  ((b) (when (is_list b))
   (list_to_binary b)))

(defun headers
  "The lmug library and adapters standardise on a map of binary key/value pairs
  for HTTP headers."
  ((h) (when (is_map h))
   h)
  ((h) (when (is_list h))
   (http.header:list->map h)))

(defun method
  "The lmug library and adapters standardise on lower-case atoms for HTTP
  methods."
  ((m) (when (is_list m))
   (list_to_atom (string:to_lower m)))
  ((m) (when (is_atom m))
   (method (atom_to_list m)))
  ((m) (when (is_binary m))
   (method (binary_to_list m))))

(defun default-content-type () (http.mimetype:text/plain))

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

(defun content-types->proplist ()
  (maps:to_list (ext-content-types)))

(defun ext->content-type
  ((ext) (when (is_binary ext))
   (ext->content-type (binary_to_list ext)))
  ((ext) (when (is_atom ext))
   (ext->content-type (atom_to_list ext)))
  ((ext) (when (is_list ext))
   (maps:get (string:to_lower ext)
             (ext-content-types)
             (default-content-type))))
