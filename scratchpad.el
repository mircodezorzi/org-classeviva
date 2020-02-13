;;; scratchpad.el --- Scratchpad

;;; Commentary:

;;; Code:

(defvar org-classeviva-username "")
(defvar org-classeviva-password "")
(defvar org-classeviva-token    "")
(defvar org-classeviva-ident    "")

;;; I/O

;;  read string from input
(defun read-username ()
 "Read username."
 (interactive)
 (setq org-classeviva-username (read-string "enter username: ")))

(defun read-password ()
 "Read password."
 (interactive)
 (setq org-classeviva-password (read-string "enter password: ")))

(defun read-credentials ()
 "Read credentials."
 (setq org-classeviva-username (read-string "enter username: ")
       org-classeviva-password (read-string "enter password: ")))

(read-credentials)

;;; HTTP requests v1

;; simple HTTP request
(with-current-buffer
  (url-retrieve-synchronously "http://headers.jsontest.com/")
 (goto-char (point-min))
 (re-search-forward "^$")
 (delete-region (point) (point-min))
 (buffer-string))

;; wrapping it into a function
(defun org-classeviva-fetch (url)
 "Make request to URL."
 (with-current-buffer
  (url-retrieve-synchronously url)
 (goto-char (point-min))
 (re-search-forward "^$")
 (delete-region (point) (point-min))
 (buffer-string)))

;;; JSON

(require 'json)

;; parse response as a JSON
(json-read-from-string (org-classeviva-fetch "http://headers.jsontest.com/"))

;; wrapping it into a function
(defun org-classeviva-fetch-json (url)
 "Make request to URL and parse response as a JSON."
 (json-read-from-string (org-classeviva-fetch url)))

;; accessing JSON fields by key
(assoc 'Host (org-classeviva-fetch-json "http://headers.jsontest.com/"))

;; print all user ids
(mapc
 (lambda (user) (assoc 'id user))
 (org-classeviva-fetch-json "https://jsonplaceholder.typicode.com/users"))

;; print all company names
(mapc
 (lambda (user) (cdr (assoc 'name (assoc 'company user))))
 (org-classeviva-fetch-json "https://jsonplaceholder.typicode.com/users"))

;; format all company names as org headers
(mapc
 (lambda (user) (format "* %s\n" (cdr (assoc 'name (assoc 'company user)))))
 (org-classeviva-fetch-json "https://jsonplaceholder.typicode.com/users"))

;; write to a file
(write-region
 (string-join
  (mapcar
   (lambda (user) (format "* %s\n" (cdr (assoc 'name (assoc 'company user)))))
   (org-classeviva-fetch-json "https://jsonplaceholder.typicode.com/users"))
  "")
 nil
 "/home/mirco/foo.org")

;;; Org-Mode

(require 'org)
(require 'org-element)

;; read org file's headers
(with-temp-buffer (insert-file-contents "/home/mirco/foo.org") (org-element-parse-buffer 'headline))

;; get all header's titles
(org-element-map
 (with-temp-buffer (insert-file-contents "/home/mirco/foo.org") (org-element-parse-buffer 'headline)) 'headline
 (lambda (hl) (org-element-property :title hl)))

;; iterate over all headers and look for specific one
(with-current-buffer "foo.org"
 (let* ((data (org-element-parse-buffer)))
  (org-element-map data 'headline
   (lambda (el)
    (when
     (equal
      (car-safe (org-element-property :title el))
      "Yost and Sons")
     (print "Found"))))
  (org-element-interpret-data data)))

;; find correct header, move to it and schedule and wrapping it into a function
(defun org-classeviva-schedule (header)
 "Demote HEADER."
 (with-current-buffer "foo.org"
  (let* ((data (org-element-parse-buffer)))
   (org-element-map data 'headline
    (lambda (el)
     (when
      (equal
       (car-safe (org-element-property :title el))
       header)
      (progn
       (goto-char (org-element-property :begin el))
       (org-schedule (current-time)))))
     (org-element-interpret-data data)))))

(org-classeviva-schedule "Yost and Sons")

;;; HTTP requests v2
(require 'request)

;; login to Spaggiari API
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
             (progn
              (setq org-classeviva-ident (substring string 1 (1- len)))
              (setq org-classeviva-token (cdr (assoc 'token data))))))))


(print org-classeviva-token)
(print org-classeviva-ident)

(defvar org-agenda "")

;; request student's agenda
(defun org-classeviva-agenda ()
 "Retrieve STUDENT's Agenda."
 (request
  (string-join
   (list "https://web.spaggiari.eu/rest/v1/students/"
         org-classeviva-ident
         "/agenda/all/20191204/20200401"))

  :headers `(("User-Agent" ."zorro/1.0")
            ("Z-Dev-Apikey" . "+zorro+")
            ("Z-Auth-Token" . ,org-classeviva-token)
            ("Content-Type" . "application/json"))

  :parser 'json-read

  :success (cl-function
            (lambda (&key data &allow-other-keys)
             (setq org-agenda data)))))

(org-classeviva-agenda)

(print org-agenda)

;;; Parsing the response

(require 'parse-time)

(defun parse-date (time)
 "Format TIME to correct timestamp."
 (format-time-string "<%Y-%m-%d %a>" (parse-iso8601-time-string time)))

(defun parse-date-time (time)
 "Format TIME to correct timestamp."
 (format-time-string "<%Y-%m-%d %a %H:%M>" (parse-iso8601-time-string time)))

(string-join
 (mapcar
  (lambda (x)
   (format "* %s\n%s\n"
    (cdr (assoc 'notes x))
    (parse-date (cdr (assoc 'evtDatetimeBegin x)))))
  (cdr (assoc 'agenda org-agenda)))
 "")

(mapc
 (lambda (x)
  (max 0 (- (string-to-number (nth 0 (split-string (format-time-string "%H %M"
   (time-subtract (parse-iso8601-time-string (cdr (assoc 'evtDatetimeEnd   x)))
                  (parse-iso8601-time-string (cdr (assoc 'evtDatetimeBegin x)))))))) 1)))
 (cdr (assoc 'agenda org-agenda)))

(mapc
 (lambda (x)
  (let ((time (split-string (format-time-string "%H %M"
              (time-subtract (parse-iso8601-time-string (cdr (assoc 'evtDatetimeEnd   x)))
                             (parse-iso8601-time-string (cdr (assoc 'evtDatetimeBegin x))))))))
   (list (max 0 (- (string-to-number (nth 0 time)) 1))
                   (string-to-number (nth 1 time)))))
 (cdr (assoc 'agenda org-agenda)))

(string-join
 (mapcar
  (lambda (x)
   (format "* %s\n%s-%s\n"
    (cdr (assoc 'notes x))
    (parse-date (cdr (assoc 'evtDatetimeBegin x)))
    (parse-date (cdr (assoc 'evtDatetimeEnd   x)))))
  (cdr (assoc 'agenda org-agenda)))
 "")

(string-join
 (mapcar
  (lambda (x)
   (if (eq (cdr (assoc 'isFullDay x)) t)
    (format "%s"
     (parse-date (cdr (assoc 'evtDatetimeBegin x))))
    (format "%s--%s"
     (parse-date-time (cdr (assoc 'evtDatetimeBegin x)))
     (parse-date-time (cdr (assoc 'evtDatetimeEnd   x))))))
  (cdr (assoc 'agenda org-agenda)))
 "\n")

(with-current-buffer "foo.org" (insert "foo"))

(string-join
 (mapcar
  (lambda (x)
   (if (eq (cdr (assoc 'isFullDay x)) t)
    (with-current-buffer "foo.org"
     (insert (format "%s\n"
      (parse-date (cdr (assoc 'evtDatetimeBegin x)))))
    (with-current-buffer "foo.org"
     (insert (format "%s--%s\n"
     (parse-date-time (cdr (assoc 'evtDatetimeBegin x)))
     (parse-date-time (cdr (assoc 'evtDatetimeEnd   x)))))))))
  (cdr (assoc 'agenda org-agenda)))
 "\n")

;; insert into buffer headers and timestamps
(string-join
 (mapcar
  (lambda (x)
   (let ((ts
    (if (eq (cdr (assoc 'isFullDay x)) t)
     (format "%s\n"
      (parse-date (cdr (assoc 'evtDatetimeBegin x))))
     (format "%s--%s\n"
      (parse-date-time (cdr (assoc 'evtDatetimeBegin x)))
      (parse-date-time (cdr (assoc 'evtDatetimeEnd   x)))))))
    (with-current-buffer "foo.org"
     (insert (format "** %s\n%s\n" (cdr (assoc 'notes x)) ts)))))
  (cdr (assoc 'agenda org-agenda)))
 "\n")

(cl-defun org-classeviva-request (&key url type parser headers data)
 (unless parser (setq parser 'buffer-string))
 (print parser)
 (let ((url-request-method type)
       (url-request-extra-headers headers)
       (url-request-data data))
  (with-current-buffer (url-retrieve-synchronously url)
   (goto-char (point-min))
   (re-search-forward "^$")
   (delete-region (point) (point-min))
   (funcall parser))))

(org-classeviva-request
 :url "https://web.spaggiari.eu/rest/v1/auth/login/"

 :type "POST"
 :parser 'json-read

 :headers '(("User-Agent" ."zorro/1.0")
            ("Z-Dev-Apikey" . "+zorro+")
            ("Content-Type" . "application/json"))

 :data (json-encode `(("uid"  . ,org-classeviva-username)
                      ("pass" . ,org-classeviva-password))))

;;; scratchpad.el ends here
