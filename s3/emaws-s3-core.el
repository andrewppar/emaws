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
(require 'cl-lib)


(defun emaws-s3-core--handle-cli-response (response)
  "Handle the CLI RESPONSE from an S3 operation."
  (if (string-prefix-p "An error occurred" (string-trim response))
      (progn (message response) nil)
    (json-parse-string
     response
     :object-type 'plist
     :array-type 'list
     :false-object nil)))

(defun emaws-s3-core--list-buckets (region)
  "List buckets in REGION."
  (let ((args (list "--output" "json")))
    (when region (setq args (cons "--region" (cons region args))))
    (emaws-s3-core--handle-cli-response
     (emaws-core/execute "s3api" "list-buckets" args))))

(defun emaws-s3-core--parse-aws-bucket! (aws-bucket)
  "Parse the AWS-BUCKET into something emaws can consume."
  (cl-destructuring-bind (&key Name CreationDate BucketArn &allow-other-keys)
      aws-bucket
    (list :name Name :created CreationDate :arn BucketArn)))

(defun emaws-s3-core/list-buckets (region)
  "Get the buckets in REGION in a way that emaws can consume."
  (when-let* ((buckets (emaws-s3-core--list-buckets region)))
    (cl-destructuring-bind (&key Buckets _Owner _Prefix &allow-other-keys)
	buckets
      (mapcar #'emaws-s3-core--parse-aws-bucket! Buckets))))

(defun emaws-s3-core--list-objects (region bucket path)
  "List objects in REGION, BUCKET, and at PATH."
  (let* ((args (list "--bucket" bucket "--delimiter" "/")))
    (when region (setq args (cons "--region" (cons region args))))
    (when path
      (let ((aws-path (string-join (reverse (cons "" (reverse path))) "/")))
	(setq args (cons "--prefix" (cons aws-path args)))))
    (emaws-s3-core--handle-cli-response
     (emaws-core/execute "s3api" "list-objects" args))))

(defun emaws-s3-core--parse-aws-object! (aws-object)
  "Parse an AWS-OBJECT into an emaws object."
  (cl-destructuring-bind (&key Key LastModified &allow-other-keys)
      aws-object
    (list :name (car (last (string-split Key "/" t))) :modified LastModified :filep t)))

(defun emaws-s3-core--parse-aws-prefix! (aws-prefix)
  "Parse an AWS-PREFIX into somethng emaws can consume."
  (cl-destructuring-bind (&key Prefix &allow-other-keys)
      aws-prefix
    (list :name (car (last (string-split Prefix "/" t))) :filep nil)))

(defun emaws-s3-core/list-objects (region bucket path)
  "Get all the objects in REGION, BUCKET, and at PATH."
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
  "Download the object at REGION, BUCKET, PATH, and ITEM to DESTINATION."
  (let* ((key (string-join (reverse (cons item (reverse path))) "/"))
	 (args (list "--bucket" bucket "--key" key destination)))
    (when region (setq args (cons "--region" (cons region args))))
    (emaws-s3-core--handle-cli-response
     (emaws-core/execute "s3api" "get-object" args))))

(provide 'emaws-s3-core)
;;; emaws-s3-core.el ends here
