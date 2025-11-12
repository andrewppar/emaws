;;; emaws-face.el -- jj colors -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025 Andrew Parisi

;; Author: Andrew Parisi <andrew.p.parisi@gmail.com>
;; Created 15 May 2025
;; Keywords: vcs, jujutsu
;; Package-Requires: ((emacs 30))
;; SPDX-License-Identifier: GPL-3.0
;; Version: 0.0.1

;;; Commentary:

;; Make some faces for emaws

;;; Code:
(defconst emaws-face/title (list :foreground "#a6e3a1" :weight 'bold))
(defconst emaws-face/bucket (list :foreground "#89dceb"))
(defconst emaws-face/divider (list :foreground "#cdd6f4"))
(defconst emaws-face/time (list :foreground "#bac2de"))
(defconst emaws-face/region (list :foreground  "#eba0ac"))
(defconst emaws-face/path (list :foreground "#a6adc8"))
(defconst emaws-face/key (list :foreground "#b4befe"))
(defconst emaws-face/file (list :foreground "#74c7ec"))

(defun emaws-face/text (text &optional color)
  (if color
      (propertize text 'face (pcase color
			       (:title emaws-face/title)
			       (:divider emaws-face/divider)
			       (:bucket emaws-face/bucket)
			       (:time emaws-face/time)
			       (:region emaws-face/region)
			       (:path emaws-face/path)
			       (:key emaws-face/key)
			       (:file emaws-face/file)))
    text))


(provide 'emaws-face)
;;; emaws-face.el ends here
