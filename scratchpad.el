;;; scratchpad.el --- Scratchpad

;;; Commentary:

;;; Code:

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
  (lambda (user) (format "* %s \n" (cdr (assoc 'name (assoc 'company user)))))
  (org-classeviva-fetch-json "https://jsonplaceholder.typicode.com/users"))

;; join list of all company names into a single string
(string-join
  (mapcar
    (lambda (user) (format "* %s\n" (cdr (assoc 'name (assoc 'company user)))))
    (org-classeviva-fetch-json "https://jsonplaceholder.typicode.com/users"))
  "")

;; write to a file
(write-region
  (string-join
    (mapcar
      (lambda (user) (format "* %s\n" (cdr (assoc 'name (assoc 'company user)))))
      (org-classeviva-fetch-json "https://jsonplaceholder.typicode.com/users"))
    "")
  nil
  "/home/mirco/foo.org")

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

;; find correct header, move to it and schedule
;; wrapping it into a function
(defun org-classeviva-demote (header)
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

(org-classeviva-demote "Yost and Sons")

;;; scratchpad.el ends here
