;;; emaws-core.el --- emacs aws ui interface -*- lexical-binding: t -*-

;;Copyright (C) 2025-2025 Andrew Parisi

;;Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;Created: 12 Nov 2025
;;Homepage: N/A
;;Keywords:
;;Package-Requires: ((emacs "30.2"))
;;SPDX-License-Identifier: MIT
;;Version: 0.0.1

;;; Commentary:

;;; Send and recieve information from the aws cli

;;; Code:
(require 'subr-x)

(defmacro defonce (var value)
  `(defvar ,var ,value))

(defonce emaws-core--executable nil)

(defun emaws-core--get-executable ()
  (or emaws-core--executable
      (let ((executable (string-trim (shell-command-to-string "which aws"))))
	(setq emaws-core--executable executable)
	executable)))

(defun emaws-core/execute (command subcommand args)
  (thread-last
    (string-join args " ")
    (format "%s %s %s %s" (emaws-core--get-executable) command subcommand)
    shell-command-to-string))

(defvar emaws-core/state nil)

(defun emaws-core/get-in (plist keys)
  (let ((result plist))
    (dolist (key keys)
      (setq result (plist-get result key #'equal)))
    result))

(defun emaws-core/put-in (plist keys value)
  (let ((result (seq-copy plist)))
    (if-let ((key (car keys)))
	(if (cdr keys)
	    (let* ((old-value (plist-get result key #'equal))
		   (to-update (if (listp old-value) old-value nil))
		   (recursive-case (emaws-core/put-in to-update (cdr keys) value)))
	      (plist-put result key recursive-case #'equal))
	  (plist-put result key value #'equal))
      result)))

(defun emaws-core/update-in (plist keys function &rest args)
  (let* ((value (emaws-core/get-in plist keys))
	 (new-value (apply function value args)))
    (emaws-core/put-in plist keys new-value)))

(defun emaws-core/state-set-in (keys value)
  (setq emaws-core/state (emaws-core/put-in emaws-core/state keys value)))

(provide 'emaws-core)
;;; emaws-core.el ends here
