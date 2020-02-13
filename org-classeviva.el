;;; org-classeviva.el --- Convert Classeviva Agenda to Org-Mode Agenda

;; Copyright (C) 2020  Mirco De Zorzi

;; Author: Mirco De Zorzi <mircodezorzi@protonmail.com>
;; Keywords: classeviva org
;; Version: 0.0.1

;;; Commentary:

;;; Code:

(require 'json)        ; parse API response
(require 'org)         ; parse Org file
(require 'org-element) ; operate over Org file
(require 'parse-time)  ; parse time
(require 'request)     ; send HTTP requests

(defvar org-classeviva-username nil)
(defvar org-classeviva-password nil)
(defvar org-classeviva-token    nil)
(defvar org-classeviva-ident    nil)

(defvar org-classeviva-agenda-beg "20190901")
(defvar org-classeviva-agenda-end "20200601")

(defvar org-classeviva-agenda   nil
 "Student's agenda.")

(defun org-classeviva-read-secrets ()
 "Read credentials."
 (interactive)
 (setq org-classeviva-username (read-string "enter username: ")
  org-classeviva-password (read-string "enter password: ")))

(defun org-classeviva-parse-date (timestamp)
 "Format TIMESTAMP to correct format."
 (format-time-string "<%Y-%m-%d %a>" (parse-iso8601-time-string timestamp)))

(defun org-classeviva-parse-date-time (timestamp)
 "Format TIMESTAMP to correct format."
 (format-time-string "<%Y-%m-%d %a %H:%M>" (parse-iso8601-time-string timestamp)))

(defun org-classeviva-auth ()
 "Authenticate user to Spaggiari."
 (interactive)
 (request
  "https://web.spaggiari.eu/rest/v1/auth/login/"
  :type "POST"

  :headers '(("User-Agent" ."zorro/1.0")
             ("Z-Dev-Apikey" . "+zorro+")
             ("Content-Type" . "application/json"))

  :data (json-encode `(("uid"  . ,org-classeviva-username)
                       ("pass" . ,org-classeviva-password)))

  :parser 'json-read

  :success (cl-function
            (lambda (&key data &allow-other-keys)
             (let* ((string (cdr (assoc 'ident data)))
                   (len (length string)))
              (setq org-classeviva-ident (substring string 1 (1- len))
                    org-classeviva-token (cdr (assoc 'token data))))))))

(defun org-classeviva-build-headers (token)
 "Build headers containing authentication TOKEN."
 `(("User-Agent" ."zorro/1.0")
   ("Z-Dev-Apikey" . "+zorro+")
   ("Z-Auth-Token" . ,token)
   ("Content-Type" . "application/json"))
 "Headers needed for all authenticated Spaggiari endpoints.")

(defun org-classeviva-retrieve-agenda ()
 "Retrieve student's agenda."
 (interactive)
 (request
  (string-join
   (list "https://web.spaggiari.eu/rest/v1/students/"
         org-classeviva-ident
         "/agenda/all/"
         org-classeviva-agenda-beg
         "/"
         org-classeviva-agenda-end))

  :headers (org-classeviva-build-headers org-classeviva-token)

  :parser 'json-read

  :success (cl-function
            (lambda (&key data &allow-other-keys)
             (setq org-classeviva-agenda data)))))


(org-classeviva-read-secrets)
(org-classeviva-auth)
(org-classeviva-retrieve-agenda)

(print org-classeviva-agenda)

(provide 'org-classeviva)

;;; org-classeviva.el ends here
