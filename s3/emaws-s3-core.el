;;; emaws-s3-core.el --- s3 api -*- lexical-binding: t -*-

;;Copyright (C) 2025-2025 Andrew Parisi

;;Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;Created: 12 Nov 2025
;;Homepage: N/A
;;Keywords:
;;Package-Requires: ((emacs "30.2"))
;;SPDX-License-Identifier: MIT
;;Version: 0.0.1

;;; Commentary:

;;;

;;; Code:
(require 'emaws-core)
(require 'subr-x)

(defun emaws-s3-core--list-buckets (region)
  (let ((args (list "--output" "json")))
    (when region (setq args (cons "--region" (cons region args))))
    (json-parse-string
     (emaws-core/execute "s3api" "list-buckets" args)
     :object-type 'plist
     :array-type 'list
     :false-object nil)))

(defun emaws-s3-core--parse-aws-bucket! (keys aws-bucket)
  (cl-destructuring-bind (&key Name CreationDate BucketArn &allow-other-keys)
      aws-bucket
    (list :name Name :created CreationDate :arn BucketArn)))

(defun emaws-s3-core/list-buckets (region)
  (let ((keys (list :s3 (or region :default))))
    ;; this function call needs to be cached itself -
    ;; we can add keys to the state in other ways too
    ;;(or (emaws-core/get-in emaws-core/state key)
    (cl-destructuring-bind (&key Buckets _Owner _Prefix &allow-other-keys)
	(emaws-s3-core--list-buckets region)
      (mapcar
       (lambda (aws-bucket)
	 (emaws-s3-core--parse-aws-bucket! keys aws-bucket))
       Buckets))))

(defun emaws-s3-core--list-objects (region bucket path)
  (let* ((args (list "--bucket" bucket "--delimiter" "/")))
    (when region (setq args (cons "--region" (cons region args))))
    (when path
      (let ((aws-path (string-join (reverse (cons "" (reverse path))) "/")))
	(setq args (cons "--prefix" (cons aws-path args)))))
    (json-parse-string
     (emaws-core/execute "s3api" "list-objects" args)
     :object-type 'plist
     :array-type 'list
     :false-object nil)))

(defun emaws-s3-core--parse-aws-object! (aws-object)
  (cl-destructuring-bind (&key Key LastModified &allow-other-keys)
      aws-object
    (list :name (car (last (string-split Key "/" t))) :modified LastModified :filep t)))

(defun emaws-s3-core--parse-aws-prefix! (aws-prefix)
  (cl-destructuring-bind (&key Prefix &allow-other-keys)
      aws-prefix
    (list :name (car (last (string-split Prefix "/" t))) :filep nil)))

(defun emaws-s3-core/list-objects (region bucket path)
  (cl-destructuring-bind (&key CommonPrefixes Contents &allow-other-keys)
      (emaws-s3-core--list-objects region bucket path)
    (seq-concatenate
     'list
     (mapcar
      (lambda (prefix) (emaws-s3-core--parse-aws-prefix! prefix))
      CommonPrefixes)
     (mapcar
      (lambda (object) (emaws-s3-core--parse-aws-object! object))
      Contents))))

(defun emaws-s3-core/download (region bucket path item destination)
  (let* ((key (string-join (reverse (cons item (reverse path))) "/"))
	 (args (list "--bucket" bucket "--key" key destination)))
    (when region (setq args (cons "--region" (cons region args))))
    (json-parse-string
     (emaws-core/execute "s3api" "get-object" args)
     :object-type 'plist
     :array-type 'list
     :false-object nil)))


(provide 'emaws-s3-core)
;;; emaws-s3-core.el ends here
