(defmodule lmug-fs
  (export
   (abs-path 2)
   (find-index 2)
   (find-index-any 2)
   (find-index-html 2)
   (read 1)
   (read-file 1)
   (response 1) (response 2)
   (url-path 2)
   (walk 1) (walk 2)))

(include-lib "kernel/include/file.hrl")
(include-lib "logjam/include/logjam.hrl")
(include-lib "lmug/include/const.hrl")

(defun read (path)
  (let* ((dir? (filelib:is_dir path))
         (file? (filelib:is_regular path))
         (data `#m(dir? ,dir?
                   file? ,file?
                   symlink? false)))
    (cond
     (dir? data)
     (file? (maps:merge data (read-file path)))
     ('true (ERR_NOREAD)))))

(defun read-file (path)
  (let ((file-info (read-file-info path))) 
    (case file-info
      (`#m(error ,_) file-info)
      (_ (maps:merge file-info (read-file-contents path))))))

(defun walk
  ((path) (when (is_binary path))
   (walk (binary_to_list path)))
  ((path) (when (is_list path))
   (walk path (maps:put 'doc-root path (lmug-mw-resource:default-opts))))
  (((= `#m(doc-root ,doc-root) opts)) (when (is_map opts))
   (walk doc-root opts)))

(defun walk
  ((path `#m(doc-root ,doc-root
             max-files ,max-files
             max-file-size ,max-file-size
             max-total-bytes ,max-total-bytes))
   (let* ((doc-root (safe-path doc-root))
          (metadata `#m(file-count 0 total-bytes 0))
          (file-list (fold-list doc-root))
          (file-count (length file-list)))
     (if (>= file-count max-files)
       (let ((err (ERR_MAX_FILES)))
         (log-notice "File count/max: ~p/~p" (list file-count max-files))
         (log-error "Could not walk directory: ~s" `(,(mref err 'error)))
         err)
       (progn
         (log-debug "File list: ~p" `(,file-list))
         (case (process-files file-list
                              doc-root
                              max-file-size
                              max-total-bytes
                              `#m(metadata ,metadata))
           (`#m(error ,msg)
               (log-error "Could not walk directory: ~s" `(,msg))
               `#m(error ,msg))
           (data data)))))))

(defun response (path)
  (response path #m()))

(defun response (path opts)
  (let ((opts (maps:merge (default-response-opts) opts)))
    'tbd))

(defun default-response-opts ()
  #m())

(defun find-index (doc-root path)
  (case (find-index-html doc-root path)
    (`#(error ,_) (find-index-any doc-root path))
    (file file)))

(defun find-index-html (doc-root path)
  (find-file doc-root path "index.htm*"))

(defun find-index-any (doc-root path)
  (find-file doc-root path "index.*"))

(defun find-file (doc-root path pattern)
  (case (filelib:safe_relative_path path doc-root)
    ('unsafe `#(error ,(io_lib:format "unsafe path: ~s" `(,path))))
    (safesubdir (safe-find doc-root safesubdir pattern))))

(defun safe-find (doc-root safesubdir pattern)
  (let ((safepath (filename:join doc-root safesubdir)))
    (case (filelib:wildcard pattern safepath)
      ('() `#(error ,(io_lib:format
                      "no files for pattern at: ~s"
                      `(,safepath))))
      (files (car files)))))

(defun read-file-info (path)
  (case (file:read_file_info path)
    (`#(ok ,file-info-record) (let* (((match-file_info size size
                                                       type type
                                                       access access
                                                       mtime mtime) file-info-record))
                                `#m(path ,path
                                    size ,size
                                    type ,type
                                    symlink? ,(== type 'symlink)
                                    access ,access
                                    mtime ,(datetime->iso8601 mtime)
                                    read-time ,(now->iso8601))))
    (`#(error ,reason) `#m(error ,reason))))

(defun read-file-contents (path)
  (case (file:read_file path)
    (`#(ok ,bytes) `#m(content ,bytes))
    (`#(error ,reason `#m(error ,reason)))))

(defun datetime->iso8601
  ((`#(#(,Y ,M ,D) #(,h ,m ,s)))
   (lists:flatten (io_lib:format "~p-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B"
                                 (list Y M D h m s)))))

(defun now->iso8601 ()
  (datetime->iso8601 (calendar:now_to_datetime (erlang:timestamp))))

(defun fold-list (doc-root)
  ""
  (filelib:fold_files
      doc-root
      ".*"
      'true
      (lambda (file acc) (++ acc (list file)))
      `()))

(defun process-files
  (('() _ _ _ acc)
   acc)
  ((`(,file . ,rest) doc-root max-file-size max-total-bytes acc)
   (let ((md (mref acc 'metadata))
         (url-path (url-path doc-root file)))
     (case (process-file file md max-file-size max-total-bytes)
       ((= `#(error ,_) err)
        err)
       (`#m(metadata ,md file-data ,fd)
        (clj:->> acc
                 (maps:put url-path fd)
                 (maps:put 'metadata md)
                 (process-files rest doc-root max-file-size max-total-bytes)))
       (x
        (log-error "Unknown data format: ~p" (list x)))))))

(defun process-file (file metadata max-file-size max-total-bytes)
  (let* ((file-data (read file))
         (file-size (mref file-data 'size))
         (file-count  (+ 1 (mref metadata 'file-count)))
         (total-bytes (+ file-size (mref metadata 'total-bytes)))
         (metadata (maps:merge metadata
                               `#m(file-count ,file-count
                                   total-bytes ,total-bytes))))
    (cond
     ((>= file-size max-file-size)
      (log-notice "File size/max: ~p/~p" (list file-size max-file-size))
      (ERR_MAX_FILE_SIZE))
     ((>= total-bytes max-total-bytes)
      (log-notice "Total bytes/max: ~p/~p" (list total-bytes max-total-bytes))
      (ERR_MAX_TOTAL_BYTES))
     ('true
      `#m(metadata ,metadata file-data ,file-data)))))

(defun safe-path (path)
  (let* ((root "/")
         (path (filename:absname path))
         (`(,_ ,tail) (string:split path root)))
    (++ root (filelib:safe_relative_path  tail "/"))))

(defun url-path
  ((doc-root abs-path) (when (is_binary abs-path))
   (url-path doc-root (binary_to_list abs-path)))
  ((doc-root abs-path) (when (is_binary doc-root))
   (url-path (binary_to_list doc-root) abs-path))
  ((doc-root abs-path)
   ;;(log-debug "doc-root: ~p; abs-path: ~p" (list doc-root abs-path))
   (let ((`(,_ ,url-path) (string:split abs-path doc-root)))
     (list_to_binary url-path))))

(defun abs-path
  ((doc-root url-path) (when (is_binary url-path))
   (abs-path doc-root (binary_to_list url-path)))
  ((doc-root url-path) (when (is_binary doc-root))
   (abs-path (binary_to_list doc-root) url-path))
  ((doc-root url-path)
   (++ doc-root url-path)))
