;;; emaws-s3-display.el --- Display S3 Information -*- lexical-binding: t -*-

;;Copyright (C) 2025-2025 Andrew Parisi

;;Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;Created: 13 Nov 2025
;;Homepage: N/A
;;Keywords:
;;Package-Requires: ((emacs "30.2"))
;;SPDX-License-Identifier: MIT
;;Version: 0.0.1

;;; Commentary:

;;;

;;; Code:
(require 'emaws-s3-core)
(require 'emaws-face)

(define-derived-mode emaws/s3-mode fundamental-mode
  "Mode for displaying s3 objects"
  "Major mode for viewing s3 objects."
  (define-key emaws/s3-mode-map
      (kbd "C-c q") (lambda () (interactive) (kill-buffer (current-buffer)))))

(defvar emaws-s3-display--buffers '())

(defmacro emaws-s3--with-buffer (buffer-name &rest body)
  "Perform an operation with an emaws buffer with BUFFER-NAME and BODY."
  (declare (indent 1))
  (let ((buffer (gensym "buffer")))
    `(unwind-protect
	  (let ((,buffer (switch-to-buffer ,buffer-name))
		(inhibit-read-only t))
	    (push ,buffer emaws-s3-display--buffers)
	    (erase-buffer)
	    (emaws/s3-mode 1)
	    (progn ,@body))
       (read-only-mode 1))))

(defun emaws-s3-display--make-header (header region bucket path)
  "Make a header for HEADER REGION BUCKET and PATH."
  (let* ((max-length (max (length header)
			  (length region)
			  (length bucket)
			  (length path)))
	 (divider (string-join (make-list max-length "=")))
	 (lines (list (emaws-face/text header :title))))
    (when region
      (push (emaws-face/text (format "Region: %s" region) :region) lines))
    (when bucket
      (push (emaws-face/text (format "Bucket: %s" bucket) :bucket) lines))
    (when path
      (push (emaws-face/text (format "Path: %s" (string-join path "/")) :path) lines))
    (push (emaws-face/text divider :divider) lines)
    (string-join (reverse lines) "\n")))

(defun emaws-s3-display/show-buckets (&optional region)
  "Show all buckets in REGION."
  (let ((buckets (mapcar
		  (lambda (bucket)
		    (let ((name (plist-get bucket :name))
			  (created (plist-get bucket :created)))
		      (format " %s %s"
			      (emaws-face/text created :time)
			      (emaws-face/text name :bucket))))
		  (emaws-s3-core/list-buckets region))))
    (emaws-s3--with-buffer "*s3*"
      (insert
       (string-join
	(cons (emaws-s3-display--make-header "AWS S3" region nil nil) buckets)
	"\n")))))

(defun emaws-s3-display--item-at-point ()
  "Get the item at point - return the item along with it's type."
  (let ((line (buffer-substring-no-properties
	       (line-beginning-position) (line-end-position))))
    (cond ((string-prefix-p "AWS" line)
	   (list nil :title))
	  ((string-prefix-p "Region:" line)
	   (list (string-trim (cadr (string-split line ":"))) :region))
	  ((string-prefix-p "Bucket:" line)
	   (list (string-trim (cadr (string-split line ":"))) :bucket))
	  ((string-prefix-p "Path:" line)
	   (list (string-trim (cadr (string-split line ":"))) :path))
	  ((string-prefix-p "==" line)
	   (list nil :divider))
	  (t
	   (let* ((data (string-split line " " t))
		  (prefixp (equal (string-trim (car data)) "PRE"))
		  (raw-item (cadr data))
		  (item (if (string-suffix-p "/" raw-item)
			    (substring raw-item 0 -1)
			  raw-item)))
	     (list item (cond
			  (prefixp :prefix)
			  ((emaws-s3-display--bucket) :object)
			  (t :bucket))))))))

(defun emaws-s3-display--region ()
  "Get the current region."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let* ((line (buffer-substring-no-properties
		  (line-beginning-position) (line-end-position)))
	   (region (cadr (string-split line "Region:"))))
      (when region (string-trim region)))))

(defun emaws-s3-display--bucket ()
  "Get the current bucket."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let* ((line (buffer-substring-no-properties
		  (line-beginning-position) (line-end-position)))
	   (bucket (cadr (string-split line "Bucket:"))))
      (when bucket (string-trim bucket)))))

(defun emaws-s3-display--path ()
  "Get the current path."
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (let* ((line (buffer-substring-no-properties
		  (line-beginning-position) (line-end-position)))
	   (path (cadr (string-split line "Path:"))))
      (when path
	(mapcar #'string-trim (string-split path "/"))))))


(defun emaws-s3-display--format-object (object)
  "Format OBJECT."
  (cl-destructuring-bind (&key name modified filep &allow-other-keys)
      object
    (let* ((time (or modified "PRE"))
	   (format-type (if filep :file :key)))
      (format " %s %s"
	      (emaws-face/text time :time)
	      (emaws-face/text name format-type)))))

(defun emaws-s3-display--step (region bucket path item)
  "Fetch the object at REGION BUCKET PATH and ITEM."
  (let* ((path (when bucket (reverse (cons item (reverse path)))))
	 (bucket (or bucket item))
	 (objects (emaws-s3-core/list-objects region bucket path)))
    (emaws-s3--with-buffer (format "*s3: %s*" (string-join (cons bucket path) "/"))
      (insert
       (string-join
	(cons (emaws-s3-display--make-header "AWS S3" region bucket path)
	      (mapcar #'emaws-s3-display--format-object objects))
	"\n")))))

(defun emaws-s3-display--download (region bucket path item)
  "Download the object at REGION BUCKET PATH and ITEM."
  (let ((to (read-file-name "destination: ")))
    (emaws-s3-core/download region bucket path item to)))

(defun emaws-s3-display/step ()
  "Take one step in an emaws s3 buffer depending on where point is."
  (interactive)
  (cl-destructuring-bind (item item-type)
      (emaws-s3-display--item-at-point)
    (let ((region (emaws-s3-display--region))
	  (bucket (emaws-s3-display--bucket))
	  (path (emaws-s3-display--path)))
      (cond
	((or (equal item-type :prefix))
	 (emaws-s3-display--step region bucket path item))
	((equal item-type :object)
	 (emaws-s3-display--download region bucket path item))
	((equal item-type :path)
	 (let* ((back-step (cdr (reverse path)))
		(previous-item (car back-step))
		(previous-path (reverse (cdr back-step))))
	   (emaws-s3-display--step region bucket previous-path previous-item)))
	((equal item-type :bucket)
	 (emaws-s3-display--step region nil nil item))))))

(provide 'emaws-s3-display)
;;; emaws-s3-display.el ends here
