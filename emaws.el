;;; emaws.el --- explore aws from emacs -*- lexical-binding: t -*-

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

;;;###autoload
(defun emaws/s3-action ()
  (interactive)
  ;; make this more robust - we probably need some minor modes
  (let ((name (buffer-name)))
    (if (string-prefix-p "*s3" name)
	(emaws-s3/step)
      (emaws-s3/start))))

(provide 'emaws)
;;; emaws.el ends here
