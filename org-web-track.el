;;; org-web-track.el --- Web data tracking framework in org mode -*- lexical-binding: t -*-

;; Author: p-snow <public@p-snow.org>
;; Maintainer: p-snow <<public@p-snow.org>>
;; Version: 0.0.2
;; Package-Requires: (org org-agenda request enlive)
;; Homepage: https://github.com/p-snow/org-web-track
;; Keywords: org, agenda, web, hypermedia


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; org-web-tools offers a set of elisp functions and commands which are useful
;; for retrieving and managing data from the web in Org mode.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'url-http)
(require 'request)
(require 'enlive)
(require 'rx)

(defvar org-web-track-update-property "WEB_TRACK_LAST"
  "Property name for latest value.")
(put 'org-web-track-update-property 'label "UPDATE")

(defvar org-web-track-prev-property "WEB_TRACK_PREV"
  "Property name for last value.")
(put 'org-web-track-prev-property 'label "PREV")

(defvar org-web-track-url-property "WEB_TRACK_URL"
  "Property name for tracking URL.")

(defvar org-web-track-date-property "WEB_TRACK_AT"
  "Property name for the date at which track value.")
(put 'org-web-track-date-property 'label "DATE")

(defvar org-web-track-update-timeout 20
  "Time out in second for accessing web site to get values.")

(defcustom org-web-track-selector-alist nil
  "An alist of selectors to obtain tracking data.

Each element has the form (URL-MATCH . SELECTOR), where URL-MATCH is used to
find which tracking entry this SELECTOR is responsible for and SELECTOR itself."
  :type '(alist :key-type (string :tag "Regexp")
                :value-type
                (choice (vector :tag "A CSS selector")
                        (string :tag "A shell command that processes HTTP response content")
                        function))
  :group 'org-web-track)

(defun org-web-track-setup (url)
  "Setup tracking entry for URL by putting `org-web-track-url-property'.

If there is no selector defined in `org-web-track-selector-alist' for the URL,
encourage user to custom beforehand"
  (interactive (list (read-string "Tracking URL: ")))
  (org-entry-put (point) org-web-track-url-property url)
  (if (assoc-default url org-web-track-selector-alist #'string-match)
      (org-web-track-update)
    (user-error "No selector defined for the URL")))

(defun org-web-track-update (&optional async)
  "Start tracking and update properties.

This command blocks user interaction unless ASYNC is non-nil.
Note that ASYNC mode is not adequately tested."
  (interactive "P")
  (let ((track-url (org-entry-get (point) org-web-track-url-property))
        (marker (point-marker)))
    (if async
        (apply #'funcall
               #'org-web-track-get-values
               track-url async #'org-web-track-record nil (list marker))
      (let ((updates (apply (if (called-interactively-p 'any)
                                #'funcall-interactively
                              #'funcall)
                            #'org-web-track-get-values
                            track-url async)))
        (org-web-track-record marker updates)))))

(defun org-web-track-update-all ()
  "Update all tracking entries in `org-web-track-files' and return a set of change."
  (interactive)
  (delq nil (org-map-entries (lambda ()
                               (call-interactively 'org-web-track-update))
                             (format "%s={.+}" org-web-track-url-property)
                             org-web-track-files)))

(defun org-web-track-get-values (url &optional async on-success on-fail marker)
  "Get target values by accessing URL.

If ASYNC is non-nil, this process will be executed asynchronously (Synchronous access is default)."
  (let ((the-selector
         (assoc-default url org-web-track-selector-alist
                        (lambda (car key)
                          (cond
                           ((functionp car) (funcall car key))
                           ((stringp car) (string-match-p car key))))))
        (request-backend 'url-retrieve)
        (request-curl-options
         `(,(format "-H \"%s\"" (string-trim (url-http-user-agent-string)))))
        (values nil))
    (unless the-selector
      (and (functionp on-fail)
           (progn (apply on-fail marker)
                  (user-error "No tracker available for this entry"))))
    (request url
      :sync (not async)
      :timeout org-web-track-update-timeout
      :success
      (cl-function
       (lambda (&key response &allow-other-keys)
         (let* ((content-type (request-response-header response "content-type"))
                (content-charset (if (string-match url-mime-content-type-charset-regexp
                                                   content-type)
                                     (match-string 1 content-type)
                                   ""))
                (coding-sys (intern (downcase content-charset)))
                (content (request-response-data response)))
           (setq values
                 (mapcar (lambda (selector)
                           (funcall #'org-web-track--select-value
                                    (when (string-match (rx (or (seq (or "application/" "text/")
                                                                     (group-n 1 (or "html" "xml" "json" "csv" "plain")))))
                                                        content-type)
                                      (intern (match-string 1 content-type)))
                                    (decode-coding-string content
                                                          (and (member coding-sys (coding-system-list))
                                                               coding-sys))
                                    selector))
                         (ensure-list the-selector))))))
      :error
      (cl-function
       (lambda (&key response &allow-other-keys)
         (when (eq (request-response-symbol-status response) 'timeout)
           (setq values nil))
         (message "Error %s occurred"
                  (request-response-error-thrown response))))
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (if (seq-some 'stringp values)
             (and (functionp on-success)
                  (funcall on-success marker values))
           (and (functionp on-fail)
                (apply on-fail marker))
           (message "No value available at the end of tracker appliance")))))
    (unless async
      values)))

(defmacro org-web-track--with-content-buffer (content &rest body)
  "Execute BODY in the temporary buffer where CONTENT is inserted."
  (declare (indent 1))
  `(with-temp-buffer
     (set-buffer-multibyte t)
     (insert ,content)
     ,@body))

(defun org-web-track--select-value (format content selector)
  "Return a value derived from CONTENT which is FORMAT format by using SELECTOR."
  (let ((val (cond
              ((stringp selector)
               (org-web-track--with-content-buffer content
                 (when (= 0 (shell-command-on-region (point-min) (point-max) selector t t))
                   (buffer-substring-no-properties (point-min) (point-max)))))
              ((functionp selector)
               (funcall selector (cond
                                  ((eq format 'json) (json-parse-string content :object-type 'alist))
                                  ((eq format 'html)
                                   (org-web-track--with-content-buffer content
                                     (libxml-parse-html-region)))
                                  ((eq format 'xml)
                                   (org-web-track--with-content-buffer content
                                     (libxml-parse-xml-region))))))
              ((and (vectorp selector)
                    (eq format 'html))
               (enlive-text (enlive-query (enlive-parse content)
                                          selector))))))
    (when (stringp val)
      (string-trim val))))

(defun org-web-track-record (marker updates)
  "Propagate UPDATES to the entry in consequence of getting updates-in-entry in success.

Return non-nil if value has changed."
  (when (cl-delete-if-not 'stringp updates)
    (let ((updates-in-entry
           (or (org-entry-get-multivalued-property marker org-web-track-update-property)
               (when-let ((single-val (org-entry-get marker org-web-track-update-property)))
                 (make-list (length updates) single-val))))
          (update-time (format-time-string (org-time-stamp-format t t))))
      (cl-labels ((update-value ()
                    (apply #'org-entry-put-multivalued-property marker org-web-track-update-property updates)
                    (org-with-point-at marker
                      (setf (alist-get 'track org-log-note-headings)
                            "Track %-12s on %t")
                      (prog1 (org-add-log-setup 'track
                                                ;; work around for stuck process in
                                                ;; string conversion at `org-store-log-note'
                                                (replace-regexp-in-string "\\(?:%20\\([DSTUdstu]\\)\\)" "_\\1"
                                                                          (org-entry-get (point) org-web-track-update-property))
                                                nil 'state update-time)
                        (run-hooks 'post-command-hook))))
                  (update-last-value ()
                    (apply #'org-entry-put-multivalued-property marker org-web-track-prev-property updates-in-entry)))
        (org-entry-put marker org-web-track-date-property update-time)
        (if (not updates-in-entry)
            (progn (update-value)
                   (cons marker updates))
          (if (not (equal updates updates-in-entry))
              (progn (update-last-value)
                     (update-value)
                     (cons marker updates))
            nil))))))

(defun org-web-track-insert-value-change-table ()
  "Insert a table whose row represents value change at the time."
  (interactive)
  (let ((table-rows))
    (org-with-wide-buffer
     (org-save-outline-visibility t
       (org-back-to-heading t)
       (org-fold-show-all '(drawers))
       (let* ((case-fold-search t)
              (date (org-entry-get (point) org-web-track-date-property))
              (values (org-entry-get-multivalued-property (point) org-web-track-update-property))
              (drawer-end (save-excursion (re-search-forward org-logbook-drawer-re (save-excursion (org-end-of-subtree)) t)))
              (re (concat (rx "- Track "
                              "\"" (group (+ not-newline)) "\""
                              (+ space) (opt "on") (+ space))
                          org-ts-regexp-inactive))
              (end-mkr (set-marker (mark-marker) drawer-end)))
         (goto-char (match-beginning 0))
         (while (re-search-forward re (marker-position end-mkr) t)
           (push `(,(match-string-no-properties 2)
                   ,@(mapcar #'org-entry-restore-space (split-string (match-string-no-properties 1))))
                 table-rows)))))
    (and table-rows
         (org-web-track--insert-table-from table-rows))))

(defun org-web-track--insert-table-from (rows)
  ""
  (insert "|TIME")
  (sort rows
        (lambda (elm-a elm-b)
          (cl-labels ((make-time (elm) (apply #'encode-time (parse-time-string (car elm)))))
            (time-less-p (make-time elm-a) (make-time elm-b)))))
  (cl-dotimes (index (1- (seq-reduce (lambda (len-max element)
                                       (if (< len-max (length element))
                                           (length element)
                                         len-max))
                                     rows
                                     0)))
    (insert (format "|VALUE%d" (1+ index))))
  (insert "\n|--\n")
  (mapc (lambda (row)
          (mapc (lambda (cell)
                  (insert "| ")
                  (insert cell))
                row)
          (insert " |\n"))
        rows)
  (org-table-align))

(defun org-agenda-web-track-update ()
  "Invoke `org-web-track-update' in org agenda mode."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (user-error "Not in agenda"))
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-redo-all)
  (org-agenda-maybe-loop
   #'org-agenda-web-track-update nil nil nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          (buffer (marker-buffer marker))
          (pos (marker-position marker)))
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
         (widen)
         (org-show-all)
         (goto-char pos)
         (funcall-interactively #'org-web-track-update))))))

(defun org-web-track-display-values (column-title values)
  "Return clean print of current value title in column view is COLUMN-TITLE."
  (when (string-prefix-p (get 'org-web-track-update-property 'label)
                         column-title)
    (org-web-track-changes (org-entry-get-multivalued-property (point) org-web-track-update-property)
                           (org-entry-get-multivalued-property (point) org-web-track-prev-property))))

(defun org-web-track-changes (values last-values)
  "Return a string which represents VALUE along with LAST-VALUE in case of non-nil."
  (string-join (cl-mapcar (lambda (val last-val)
                            (concat (and (stringp val)
                                         val)
                                    (and (stringp last-val)
                                         (not (string= val last-val))
                                         (format " [%s]" last-val))))
                          values
                          (or last-values
                              (make-list (length values) nil)))
               ", "))

(defcustom org-web-track-item-column-width 0
  "0 means unspecified."
  :type 'natnum
  :group 'org-web-track)

(defcustom org-web-track-update-column-width 0
  "0 means unspecified."
  :type 'natnum
  :group 'org-web-track)

(defvar org-web-track-columns-format
  (apply #'format "%%%sITEM %%%s%s(%s [%s]) %%%s(%s)"
         `(,@(mapcar (lambda (w) (if (= 0 w) "" (format "%d" w)))
                     `(,org-web-track-item-column-width
                       ,org-web-track-update-column-width))
           ,org-web-track-update-property
           ,(get 'org-web-track-update-property 'label)
           ,(get 'org-web-track-prev-property 'label)
           ,org-web-track-date-property
           ,(get 'org-web-track-date-property 'label)))
  "Format for columns in `org-agenda' column view.")

(defcustom org-web-track-files nil "docstring"
  :type '(repeat :tag "List of files" file)
  :group 'org-web-track)

(defun org-web-track-agenda-view ()
  "Dsiplay agenda with column view."
  (interactive)
  (when (require 'org-colview nil t)
    (let ((org-agenda-files org-web-track-files)
          (org-columns-modify-value-for-display-function
           'org-web-track-display-values)
          (org-overriding-columns-format org-web-track-columns-format)
          (org-agenda-view-columns-initially t))
      (org-tags-view nil (format "%s={.+}" org-web-track-url-property)))))

(defun org-web-track-test-tracker (tracker url)
  "Return a value, which is a result of applying TRACKER for contents at URL.

User can test their tracker without setting `org-web-track-selector-alist'."
  (let ((org-web-track-selector-alist
         (append `((,(regexp-quote url) ,tracker))
                 org-web-track-selector-alist)))
    (princ (org-web-track-get-values url nil nil))))

(provide 'org-web-track)

;;; org-web-track.el ends here
