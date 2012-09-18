;;; margo.el --- Client for MarGo, providing Go utilities

;;; Version 2012.09.18
;;; Author: Michael Fellinger <michael@iron.io>
;;; URL: http://github.com/manveru/margo.el
;;; Package-Requires: ((web "0.1.8") (json "1.2"))
; -*- lexical-binding: t -*-
(require 'web)
(require 'json)

(defun margo-goto-definition ()
  "jump to the definition of the function under the point"
  (interactive)
  (margo--post
   "/doc"
   (margo--make-hash
    `((:fn ,(buffer-file-name))
      (:src ,(buffer-substring-no-properties (point-min) (point-max)))
      (:offset ,(point))))
   (lambda (doc)
     (if (> 0 (length doc))
         (let ((entry (aref doc 0)))
           (margo--goto-definition-at
            (cdr (assoc 'fn entry))
            (cdr (assoc 'row entry))
            (cdr (assoc 'col entry))))
       (message "Couldn't find definition")))))

(defun margo--goto-definition-at (filename line col)
  (if (and filename (file-readable-p filename))
      (let ((buf (find-file-noselect filename)))
        (goto-line line buf)
        (pop-to-buffer buf))))

(defun margo-doc-hint ()
  "display docs about function at point"
  (interactive)
  (margo--post "/doc" ()
   :fn (buffer-file-name)
   :src (buffer-substring-no-properties (point-min) (point-max))
   :offset (point)
   :callback (lambda (doc)
               (with-output-to-temp-buffer "*margo*"
                 (princ (cdr (assoc 'src (aref doc 0))))
                 (with-current-buffer "*margo*"
                   (go-mode))))))

(defun margo--make-hash (kvs)
  "(margo--make-hash `((k1 ,(+ 1 2)) (k2 v2)))"
  (let ((hash (make-hash-table :test 'equal :size 1)))
    (dolist (item kvs)
      (puthash (car item) (cadr item) hash))
    hash))

(defun margo--post (request-path query-data callback)
  (let ((web-log-info nil)
        (post-data (make-hash-table :test 'equal :size 1)))
    (puthash 'data (json-encode query-data) post-data)
    ;; (message "margo--post post-data %s: " (pp post-data))
    (web-http-post
     (lambda (con header body)
       (let ((msg (json-read-from-string body)))
         (message "margo--post: %s" (pp-to-string msg))
         (let ((data (assoc-default 'data msg)))
           (funcall callback data))))
     request-path
     :data post-data
     :host "localhost"
     :mime-type 'application/x-www-form-urlencoded
     :port 57951)))

;; everything below is just prototyping

(defun* margo--post-package (&key callback (filename "") (src ""))
  (let ((query-data (make-hash-table :test 'equal :size 2)))
    (puthash 'fn filename query-data)
    (puthash 'src src query-data)
    (margo--post "/package" query-data callback)))

(defun* margo--post-pkgdirs (&key callback)
  (let ((query-data (make-hash-table :test 'equal :size 0)))
    (margo--post "/pkgdirs" query-data callback)))

(defun* margo--hello (motd &key callback)
  (margo--post "/" motd callback))

(defun* margo-fmt (src &key (filename "") (tab-indent t) (tab-width 8))
  (let ((query-data (make-hash-table :test 'equal :size 3)))
    (puthash 'fn filename query-data)
    (puthash 'src src query-data)
    (puthash 'tab_indent tab-indent query-data)
    (puthash 'tab_width tab-width query-data)
    (margo--post "/fmt" query-data (lambda (msg) (message "fmt: %s" (pp msg))))))

(provide 'margo)

;; (margo--post-package :filename '/home/manveru/foo.go :callback (lambda (j) (message "package: %s" j)))

;; (defun margo-import (pkg)
;;   (interactive
;;    (let ((pkg (function-called-at-point))
;;          (enable-recursive-minibuffers t)
;;          val)
;;      (setq val (completing-read "Import package: " obarray 'fboundp t nil nil)))))
;; (if (null pkg)
;;     (message "You didn't specify a package")
;;   (help-setup-xref (list #'margo-import pkg)
;;                    (called-interactively-p 'interactive))
;;   (save-excursion (with-help-window (help-buffer)
;;                     (prin1 pkg)
;;                     (princ " is ")
;;                     ())))
;; )

;;; margo.el ends here
