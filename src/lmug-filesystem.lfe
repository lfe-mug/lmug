(defmodule lmug-filesystem
  (export
   (find-index 2)
   (find-index-any 2)
   (find-index-html 2)
   (read 1)
   (read-file 1)
   (response 1) (response 2)
   (walk 1)))

(include-lib "kernel/include/file.hrl")
(include-lib "logjam/include/logjam.hrl")
(include-lib "lmug/include/const.hrl")

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

(defun read (path)
  (let* ((dir? (filelib:is_dir path))
         (file? (filelib:is_regular path))
         (data `#m(dir? ,dir?
                   file? ,file?
                   symlink? false)))
    (cond
     (dir? data)
     (file? (maps:merge data (read-file path)))
     ('true `#m(error ,(ERR_NOREAD))))))

(defun read-file (path)
  (maps:merge
   (read-file-info path)
   (read-file-contents path)))

(defun read-file-info (path)
  (case (file:read_file_info path)
    (`#(ok ,file-info-record) (let* (((match-file_info size size
                                                      type type
                                                      access access
                                                      mtime mtime) file-info-record))
                                `#m(size ,size
                                    type ,type
                                    symlink? ,(== type 'symlink)
                                    access ,access
                                    mtime ,(datetime->iso8601 mtime))))
    (`#(error ,reason) `#m(error ,reason))))

(defun read-file-contents (path)
  (case (file:read_file path)
    (`#(ok ,bytes) `#m(content ,bytes))
    (`#(error ,reason `#m(error ,reason)))))

(defun datetime->iso8601
  ((`#(#(,Y ,M ,D) #(,h ,m ,s)))
   (lists:flatten (io_lib:format "~p-~p-~pT~p:~p:~p" (list Y M D h m s)))))

(defun walk
  ((`#m(doc-root ,doc-root
        max-files ,max-files
        max-file-size ,max-file-size
        max-total-bytes ,max-total-bytes))
   (let* ((metadata `#m(file-count 0
                        total-bytes 0))
          (file-list (fold-list doc-root))
          (file-count (length file-list)))
     (if (>= file-count max-files)
       (let ((msg (ERR_MAX_FILES)))
         (log-notice "File count/max: ~p/~p" (list file-count max-files))
         (log-error "Could not walk directory: ~s" `(,msg))
         `#m(error ,msg))
       (progn
         ;;(log-debug "File list: ~p" `(,file-list))
         (case (iterate-files `#m(metadata ,metadata)
                              file-list
                              max-file-size
                              max-total-bytes)
           (`#m(error ,msg)
               (log-error "Could not walk directory: ~s" `(,msg))
               `#m(error ,msg))
           (data data)))))))

(defun fold-list (doc-root)
  ""
  (filelib:fold_files
      doc-root
      ".*"
      'true
      (lambda (file acc) (++ acc (list file)))
      `()))

(defun iterate-files
  ((acc '() _ _)
   acc)
  ((acc `(,file . ,rest) max-file-size max-total-bytes)
   (let* ((file-data (read file))
          (md (mref acc 'metadata))
          (file-size (mref file-data 'size))
          (file-count  (+ 1 (mref md 'file-count)))
          (total-bytes (+ file-size (mref md 'total-bytes)))
          (metadata (maps:merge md `#m(file-count ,file-count
                                       total-bytes ,total-bytes))))

     (cond
      ((>= file-size max-file-size)
       (log-notice "File size/max: ~p/~p" (list file-size max-file-size))
       `#m(error ,(ERR_MAX_FILE_SIZE)))
      ((>= total-bytes max-total-bytes) `#m(error ,(ERR_MAX_TOTAL_BYTES)))
      ('true (clj:-> acc
                     (mset (list_to_binary file) file-data)
                     (mset 'metadata metadata)
                     (iterate-files rest max-file-size max-total-bytes)))))))
