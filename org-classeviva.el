;;; org-classeviva.el --- Convert Classeviva Agenda to Org-Mode Agenda

;; Copyright (C) 2020  Mirco De Zorzi

;; Author: Mirco De Zorzi <mircodezorzi@protonmail.com>
;; Keywords: classeviva org
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(require 'json)

(defvar org-classeviva-username "")
(defvar org-classeviva-password "")

(defun org-classeviva-fetch (url)
  "Make request to URL."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (buffer-string)))

(defun org-classeviva-fetch-json (url)
  "Make request to URL and parse response as a JSON."
  (json-read-from-string (org-classeviva-fetch url)))

(provide 'org-classeviva)

;;; org-classeviva.el ends here
