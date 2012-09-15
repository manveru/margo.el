; -*- lexical-binding: t -*-
(require 'web)
(require 'json)
(defun margo--post (request-path query-hash callback)
  (let ((web-log-info nil)
        (post-data (make-hash-table :test 'equal :size 1)))
    (puthash 'data (json-encode query-hash) post-data)
    (web-http-post
     (lambda (con header body)
       (let ((msg (json-read-from-string body)))
         (message "margo--post: %s" (pp msg))
         (let ((data (cdr (assoc 'data msg))))
           (funcall callback data))))
     request-path
     :data post-data
     :host "localhost"
     :mime-type 'application/x-www-form-urlencoded
     :port 57951))

  (defun* margo--post-package (&key callback (fn "") (src ""))
    (let ((query-data (make-hash-table :test 'equal :size 2)))
      (puthash 'fn fn query-data)
      (puthash 'src src query-data)
      (margo--post "/package" query-data callback))))

;; (margo--post-package
;;  :fn '/home/manveru/foo.go
;;  :callback (lambda (j) (message "package: %s" j)))

(defun* margo--post-doc (&key callback (fn "") (src "") (offset 0))
  (let ((query-data (make-hash-table :test 'equal :size 3)))
    (puthash 'fn fn query-data)
    (puthash 'src src query-data)
    (puthash 'offset offset query-data)
    (margo--post "/doc" query-data callback)))

(defun margo-doc ()
  (interactive)
  (margo--post-doc
   :fn (buffer-file-name)
   :src (buffer-substring-no-properties (point-min) (point-max))
   :offset (point)
   :callback (lambda (doc) (message "doc: %s" (pp doc)))))

(defun margo-doc-hint ()
  (interactive)
  (margo--post-doc
   :fn (buffer-file-name)
   :src (buffer-substring-no-properties (point-min) (point-max))
   :offset (point)
   :callback (lambda (doc)
               (with-output-to-temp-buffer "*margo*"
                 (princ (cdr (assoc 'src (aref doc 0))))
                 (with-current-buffer "*margo*"
                   (go-mode))))))

(defun* margo--post-pkgdirs (&key callback)
  (let ((query-data (make-hash-table :test 'equal :size 0)))
    (margo--post "/pkgdirs" query-data callback)))

(defun margo-pkgdirs ()
  (interactive)
  (margo--post-pkgdirs
   :callback (lambda (pkdirs) (message "pkdirs: %s" (pp pkgdirs)))))

;; (margo-pkgdirs)
;; "json: %s  (assoc 'Name  (cdr (assoc 'data j)))"

(provide 'margo)
