;;; emaws-s3.el --- s3 in emacs -*- lexical-binding: t -*-

;;Copyright (C) 2025-2025 Andrew Parisi

;;Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;;Created: 14 Nov 2025
;;Homepage: N/A
;;Keywords:
;;Package-Requires: ((emacs "30.2"))
;;SPDX-License-Identifier: MIT
;;Version: 0.0.1

;;; Commentary:

;;;

;;; Code:
(require 'emaws-s3)

(defun emaws-s3/start ()
  (emaws-s3-display/show-buckets))

(defun emaws-s3/step ()
  (emaws-s3-display/step))

(provide 'emaws-s3)
;;; emaws-s3.el ends here
