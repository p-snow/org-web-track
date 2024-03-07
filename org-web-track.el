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
(require 'org-colview)
(require 'url-http)
(require 'request)
(require 'enlive)
(require 'rx)

(defvar org-web-track-update-property "TRACK_CURRENT_VALUE"
  "Property name for holding current value.")
(put 'org-web-track-update-property 'label "CURRENT VALUE")

(defvar org-web-track-prev-property "TRACK_PREVIOUS_VALUE"
  "Property name for holding previous value.")
(put 'org-web-track-prev-property 'label "PREVIOUS")

(defvar org-web-track-url-property "TRACK_URL"
  "Property name for a URL to track.")

(defvar org-web-track-date-property "TRACK_LAST_UPDATED_TIME"
  "Property name for the date at which track value.")
(put 'org-web-track-date-property 'label "UPDATED TIME")

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

(defcustom org-web-track-files nil
  "A collection of files that contain tracking entries.

This must be either a list of strings, which is a path for the above-referenced file,
or a function that returns a list of files."
  :type '(choice
          (repeat :tag "List of files" file)
          (function))
  :group 'org-web-track)

(defun org-web-track-files ()
  "Return a list of files that contain tracking entries."
  (pcase org-web-track-files
    ((and (pred functionp) fun) (funcall fun))
    ((and (pred listp) li) li)))

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
  "Look up the current values and set them to the entry at point.

This command will block the main Emacs thread unless ASYNC is set to non-nil.
Note that ASYNC mode is auxiliary and may not be thoroughly tested."
  (interactive "P")
  (let ((track-url (org-entry-get (point) org-web-track-url-property))
        (marker (point-marker)))
    (if async
        (apply #'funcall
               #'org-web-track-retrieve-values
               track-url async #'org-web-track-record nil (list marker))
      (let ((updates (apply (if (called-interactively-p 'any)
                                #'funcall-interactively
                              #'funcall)
                            #'org-web-track-retrieve-values
                            track-url async)))
        (org-web-track-record marker updates)))))

(defun org-web-track-update-all ()
  "Update all track items that have the `org-web-track-url-property' property in `org-web-track-files'.

Return a list of markers pointing to items where new values are obtained and recorded."
  (interactive)
  (delq nil (org-map-entries (lambda ()
                               (call-interactively 'org-web-track-update))
                             (format "%s={.+}" org-web-track-url-property)
                             (org-web-track-files))))

(defun org-web-track-retrieve-values (url &optional async on-success on-fail marker)
  "Retrieve values by accessing the URL.

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
  "Record UPDATES to the entry at MARKER by setting `org-web-track-update-property'.

Update `org-web-track-date-property' and return MARKER if UPDATES are a new value
against `org-web-track-update-property'."
  (when (cl-delete-if-not 'stringp updates)
    (let ((values-in-existence
           (or (org-entry-get-multivalued-property marker org-web-track-update-property)
               (when-let ((single-val (org-entry-get marker org-web-track-update-property)))
                 (make-list (length updates) single-val))))
          (current-time (format-time-string (org-time-stamp-format t t))))
      (cl-labels ((record-current-value ()
                    (apply #'org-entry-put-multivalued-property marker org-web-track-update-property updates)
                    (org-with-point-at marker
                      (setf (alist-get 'track org-log-note-headings)
                            "Track %-12s %t")
                      (prog1 (org-add-log-setup 'track
                                                ;; work around for stuck process in
                                                ;; string conversion at `org-store-log-note'
                                                (replace-regexp-in-string "\\(?:%20\\([DSTUdstu]\\)\\)" "_\\1"
                                                                          (org-entry-get (point) org-web-track-update-property))
                                                nil 'state current-time)
                        (run-hooks 'post-command-hook)))))
        (if (not values-in-existence)
            (progn (record-current-value)
                   (org-entry-put marker org-web-track-date-property current-time)
                   marker)
          (if (not (equal updates values-in-existence))
              (progn (apply #'org-entry-put-multivalued-property marker org-web-track-prev-property values-in-existence)
                     (record-current-value)
                     (org-entry-put marker org-web-track-date-property current-time)
                     marker)
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

(defun org-web-track-agenda-update ()
  "Update the tracking item in `org-agenda-mode'.

This command provides a way to invoke `org-web-track-update' after `org-web-track-agenda-columns'."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (user-error "Not in agenda"))
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-redo-all)
  (org-agenda-maybe-loop
   #'org-web-track-agenda-update nil nil nil
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
  "Modify the display of column VALUES for COLUMN-TITLE to be more understandable.

This function is designed to be set for `org-columns-modify-value-for-display-function'."
  (when (and (org-entry-get (point) org-web-track-url-property)
             (string-prefix-p (get 'org-web-track-update-property 'label)
                              column-title))
    (org-web-track-current-changes (point))))

(defun org-web-track-current-changes (&optional pom format separator)
  "Return a string that represents the current value changes at POM with respect to FORMAT and SEPARATOR.

If POM is nil, a return value of `point' is used.
FORMAT defines how to describe the current change for a single target and should
contain %p and %c as placeholders for the previous value and current value, respectively.
SEPARATOR is used in between changes for multiple targets."
  (let* ((cur-val (org-entry-get-multivalued-property (or pom (point)) org-web-track-update-property))
         (prev-val (org-entry-get-multivalued-property (or pom (point)) org-web-track-prev-property))
         (n (max 1 (length cur-val) (length prev-val))))
    (string-join
     (cl-mapcar (lambda (prev cur)
                  (format-spec (or format "%c [%p]")
                               `((?p . ,prev) (?c . ,cur))))
                (append prev-val (make-list (- n (length prev-val)) "N/A"))
                (append cur-val (make-list (- n (length cur-val)) "N/A")))
     (or separator ", "))))

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
  "Columns format for `org-web-track-columns' and `org-web-track-agenda-columns'.")

(defun org-web-track-columns ()
  "Invoke `org-columns' with `org-web-track-columns-format' to specify COLUMNS-FMT-STRING.

This command provides a columns view to comprehend current information, such as
changed values and updated time, for tracking items in the current buffer."
  (interactive)
  (let ((org-columns-modify-value-for-display-function
         'org-web-track-display-values))
    (org-columns nil org-web-track-columns-format)))

(defun org-web-track-agenda-columns ()
  "Invoke `org-agenda-columns' with `org-web-track-columns-format' to specify COLUMNS-FMT-STRING.

This command provides an agenda columns view to comprehend current information,
such as changed values and updated time, for tracking items in `org-web-track-files'."
  (interactive)
  (let ((org-columns-modify-value-for-display-function
         'org-web-track-display-values)
        (org-overriding-columns-format org-web-track-columns-format)
        (org-agenda-files (org-web-track-files))
        (org-agenda-sorting-strategy '((tags user-defined-up)))
        (org-agenda-cmp-user-defined 'org-web-track-cmp-updated-time)
        (org-agenda-view-columns-initially t))
    (org-tags-view nil (format "%s={.+}" org-web-track-url-property))))

(defun org-web-track-cmp-updated-time (a b)
  "Compare A and B with respect to their 'org-web-track-date-property' property.
Return -1 if A has an earlier time stamp indicating that the track item was
updated before B.
Return +1 if B is earlier, and nil if they are equal.

This function is intended to be set for `org-agenda-cmp-user-defined'."
  (cl-labels ((updated-time (ent)
                (ignore-errors
                  (encode-time
                   (org-parse-time-string
                    (org-entry-get (get-text-property 0 'org-hd-marker ent)
                                   org-web-track-date-property))))))
    (let ((ta (updated-time a))
          (tb (updated-time b)))
      (cond ((if ta (and tb (time-less-p ta tb)) tb) -1)
            ((if tb (and ta (time-less-p tb ta)) ta) +1)))))

(defun org-web-track-test-selector (selector url)
  "Return the values acquired by applying SELECTOR to the HTTP response for the URL.

End users can use this function as a SELECTOR tester before setting up
`org-web-track-selector-alist' in advance."
  (let ((org-web-track-selector-alist
         (append `((,(regexp-quote url) ,selector))
                 org-web-track-selector-alist)))
    (princ (org-web-track-retrieve-values url nil nil))))

(provide 'org-web-track)

;;; org-web-track.el ends here
