(defmodule lmug-util
  (doc "lmug utility functions.")
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(defun get-lmug-version ()
  (lutil:get-app-src-version "src/lmug.app.src"))

(defun get-version ()
  (++ (lutil:get-version)
      `(#(lmug ,(get-lmug-version)))))

(defun http-verbs ()
  "The list of allowed HTTP verbs. See doc/spec.md for more info."
  '(get head post put delete trace options connect patch))

(defun normalize-http-verb
  ((verb) (when (is_binary verb))
   (normalize-http-verb (binary_to_list verb)))
  ((verb) (when (is_list verb))
   (validate-http-verb (list_to_atom (string:to_lower verb))))
  ((verb) (when (is_atom verb))
   (normalize-http-verb (atom_to_list verb))))

(defun validate-http-verb (verb)
  "If a given verb is valid, return it, otherwise throw an error."
  (if (lists:member verb (http-verbs)) verb (error `#(invalid-verb ,verb))))

(defun split-host-data (host-data)
  (binary:split (car (filename:split host-data)) #":"))

(defun get-host-data (host-data)
  (case (split-host-data host-data)
    (`(,host)
      `(,host 80))
    (`(,host ,port)
      `(,host ,(list_to_integer port)))))

(defun get-hostname
  ((`#(init_data ,_ ,hostname))
    hostname))

(defun split-query (url)
  (binary:split url #"?"))

(defun parse-query-string (url)
  (case (split-query url)
    (`(,host ,query)
     (lc ((<- `#(,k ,v) (httpd:parse_query query)))
       `#(,(->binary k) ,(->binary v))))
    (`(,host)
     ())))

(defun httpd->lmug-request (data)
  "Every web server that gets an lmug adapter needs to implement a function
  like this one which will transform that server's request data into the
  request data needed by lmug, in the record structure required by lmug (and
  defined in the lmug Spec)."
  (let ((`(,host ,port) (get-host-data (mod-absolute_uri data)))
        (remote-host (get-hostname (mod-init_data data)))
        (uri (->binary (mod-request_uri data)))
        (body (mod-entity_body data)))
    (make-request
      server-port port
      server-name host
      remote-addr remote-host
      ;; XXX the following need to be sorted out
      uri uri  ; this should have query params
      path uri ; this should be with no query params
      query-params (parse-query-string uri)
      ;; XXX figure out how to get the scheme
      scheme 'unknown-scheme
      method (normalize-http-verb (mod-method data))
      headers (mod-parsed_header data)
      body body
      orig data)))

(defun get-response (lmug-request-data)
  "Translate an lmug request to an lmug response."
  (let ((body (iolist_to_binary (io_lib:format "Request data: ~n~p"
                                  (list lmug-request-data)))))
    (make-response
      status  200
      headers '(#(content-type #"text/plain"))
      body    body)))

(defun get-body-length (body)
  (integer_to_list (length body)))

(defun lmug->httpd-response (lmug-response-data)
  "The data paseed is an lmug response record."
  (let ((body (response-body lmug-response-data)))
    ;;(lfe_io:format "body: ~n~p~n" (list body))
    `(#(response
        #(.response
          ,(++
            `(#(code ,(response-status lmug-response-data))
              #(.content-length ,(get-body-length body)))
            (response-headers lmug-response-data))
          ,body)))))

(defun ext->mime-type (ext)
  "The same as #'ext->mime-type/2 but with an empty list for optional
  mime-types."
  (ext->mime-type ext ()))

(defun ext->mime-type
  "Get the mimetype from the filename extension. Takes an optional proplist of
  extensions to mimetypes that overrides values in the default-mime-types map."
  ((ext override-mime-types) (when (is_atom ext))
    (proplists:get_value ext (++ override-mime-types
                                 (get-default-mime-types))))
  ((ext override-mime-types) (when (is_binary ext))
    (ext->mime-type (binary_to_atom (filename:extension ext) 'latin1)
                    override-mime-types)))

(defun get-default-mime-types ()
  "Get a proplist of default mime-types."
  '(#(||        #"application/octet-stream")
    #(|.|       #"application/octet-stream")
    #(.7z       #"application/x-7z-compressed")
    #(.aac      #"audio/aac")
    #(.ai       #"application/postscript")
    #(.appcache #"text/cache-manifest")
    #(.asc      #"text/plain")
    #(.atom     #"application/atom+xml")
    #(.avi      #"video/x-msvideo")
    #(.bin      #"application/octet-stream")
    #(.bmp      #"image/bmp")
    #(.bz2      #"application/x-bzip")
    #(.class    #"application/octet-stream")
    #(.cer      #"application/pkix-cert")
    #(.crl      #"application/pkix-crl")
    #(.crt      #"application/x-x509-ca-cert")
    #(.css      #"text/css")
    #(.csv      #"text/csv")
    #(.deb      #"application/x-deb")
    #(.dart     #"application/dart")
    #(.dll      #"application/octet-stream")
    #(.dmg      #"application/octet-stream")
    #(.dms      #"application/octet-stream")
    #(.doc      #"application/msword")
    #(.dvi      #"application/x-dvi")
    #(.edn      #"application/edn")
    #(.eot      #"application/vnd.ms-fontobject")
    #(.eps      #"application/postscript")
    #(.etx      #"text/x-setext")
    #(.exe      #"application/octet-stream")
    #(.flv      #"video/x-flv")
    #(.flac     #"audio/flac")
    #(.gif      #"image/gif")
    #(.gz       #"application/gzip")
    #(.htm      #"text/html")
    #(.html     #"text/html")
    #(.ico      #"image/x-icon")
    #(.iso      #"application/x-iso9660-image")
    #(.jar      #"application/java-archive")
    #(.jpe      #"image/jpeg")
    #(.jpeg     #"image/jpeg")
    #(.jpg      #"image/jpeg")
    #(.js       #"text/javascript")
    #(.json     #"application/json")
    #(.lha      #"application/octet-stream")
    #(.lzh      #"application/octet-stream")
    #(.mov      #"video/quicktime")
    #(.m4v      #"video/mp4")
    #(.mp3      #"audio/mpeg")
    #(.mp4      #"video/mp4")
    #(.mpe      #"video/mpeg")
    #(.mpeg     #"video/mpeg")
    #(.mpg      #"video/mpeg")
    #(.oga      #"audio/ogg")
    #(.ogg      #"audio/ogg")
    #(.ogv      #"video/ogg")
    #(.pbm      #"image/x-portable-bitmap")
    #(.pdf      #"application/pdf")
    #(.pgm      #"image/x-portable-graymap")
    #(.png      #"image/png")
    #(.pnm      #"image/x-portable-anymap")
    #(.ppm      #"image/x-portable-pixmap")
    #(.ppt      #"application/vnd.ms-powerpoint")
    #(.ps       #"application/postscript")
    #(.qt       #"video/quicktime")
    #(.rar      #"application/x-rar-compressed")
    #(.ras      #"image/x-cmu-raster")
    #(.rb       #"text/plain")
    #(.rd       #"text/plain")
    #(.rss      #"application/rss+xml")
    #(.rtf      #"application/rtf")
    #(.sgm      #"text/sgml")
    #(.sgml     #"text/sgml")
    #(.svg      #"image/svg+xml")
    #(.swf      #"application/x-shockwave-flash")
    #(.tar      #"application/x-tar")
    #(.tif      #"image/tiff")
    #(.tiff     #"image/tiff")
    #(.ttf      #"application/x-font-ttf")
    #(.txt      #"text/plain")
    #(.webm     #"video/webm")
    #(.wmv      #"video/x-ms-wmv")
    #(.woff     #"application/font-woff")
    #(.xbm      #"image/x-xbitmap")
    #(.xls      #"application/vnd.ms-excel")
    #(.xml      #"text/xml")
    #(.xpm      #"image/x-xpixmap")
    #(.xwd      #"image/x-xwindowdump")
    #(.zip      #"application/zip")))

(defun ->binary
  ([string] (when (is_list string)) (list_to_binary string))
  ([bin]    (when (is_binary bin))  bin))
