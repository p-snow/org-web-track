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

;; org-web-track offers a set of Elisp functions and commands that are useful
;; for keeping track of data snippets web pages in Org Mode.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-colview)
(require 'url-http)
(require 'request)
(require 'enlive)
(require 'rx)

;;;; Variables

(defvar org-web-track-value "TRACK_CURRENT_VALUE"
  "Property name for holding current value.")
(put 'org-web-track-value 'label "CURRENT VALUE")

(defvar org-web-track-prev-value "TRACK_PREVIOUS_VALUE"
  "Property name for holding previous value.")
(put 'org-web-track-prev-value 'label "PREVIOUS")

(defvar org-web-track-url "TRACK_URL"
  "Property name for a URL to track.")

(defvar org-web-track-updated "TRACK_LAST_UPDATED_TIME"
  "Property name for the date at which track value.")
(put 'org-web-track-updated 'label "UPDATED TIME")

(defvar org-web-track-update-timeout 20
  "Time out in second for accessing web site to get values.")

;;;; Customization

(defgroup org-web-track nil
  "Options for `org-web-track'."
  :group 'org
  :link '(url-link "https://github.com/p-snow/org-web-track"))

(defcustom org-web-track-selectors-alist nil
  "An alist of selectors used to obtain desired data.

Each element in this alist must be a list that comprises URL-MATCH, SELECTORS
and FILTER in that specific order.

URL-MATCH:
URL-MATCH for the car indicates for which URL this selector is responsible.
URL-MATCH can be either a string or a function. The string is used as a regexp
to determine URLs that match against it. The function takes one argument,
a URL candidate, and must return non-nil if this selector can deal with the URL.

SELECTORS:
SELECTORS for the second is either a single selector or a list of selectors,
which are responsible for determining the target data.

If the selector is a vector, it is supposed to represent a CSS selector that is
used to determine the target data. org-web-track delegates the determination
procedure with CSS selector to the enlive package.
Check the following page for information on how to specify a CSS selector.

https://github.com/zweifisch/enlive/blob/master/README.org

If the selector is a function, it is expected to take a data object derived from
the HTTP response and return the target data as a string or a list of strings.
Note that each string represents one tracking unit.
The data object varies depending on the content type of the response.
If the content type is HTML, it is a parse tree obtained from
`libxml-parse-html-region'.
If the content type is XML, it is a parse tree obtained from
`libxml-parse-xml-region'.
If the content type is JSON, it is a Lisp object obtained from
`json-parse-string'.

If the selector is a string, it is expected to be a shell command that takes
the response as stdin and returns the target data.
Users can use parsing utility commands like pup or htmlq.

FILTER:
The optional third FILTER takes the multiple arguments that the selector returns
for the target data and returns filtered results as either a string or a list of
strings."
  :type '(alist :key-type (string :tag "Regexp")
                :value-type
                (choice (vector :tag "A CSS selector")
                        (string :tag "A shell command that processes HTTP response content")
                        function))
  :group 'org-web-track)

(defcustom org-web-track-files nil
  "The files to be used in `org-web-track-agenda-columns'.

Its value must be either a list of strings representing file paths
or a function that returns the same data structure."
  :type '(choice
          (repeat :tag "List of files" file)
          (function))
  :group 'org-web-track)

(defcustom org-web-track-item-column-width 0
  "0 means unspecified."
  :type 'natnum
  :group 'org-web-track)

(defcustom org-web-track-update-column-width 0
  "0 means unspecified."
  :type 'natnum
  :group 'org-web-track)


(defun org-web-track-files ()
  "Return a list of files that contain tracking entries."
  (pcase org-web-track-files
    ((and (pred functionp) fun) (funcall fun))
    ((and (pred listp) li) li)))

;;;###autoload
(defun org-web-track-setup-entry (url)
  "Initialize the entry at point by setting URL to `org-web-track-url'.

If point is positioned before the first org heading, insert a new one above it
initially. After the URL has been set, try to retrieve a value if there is
an appropriate selector in `org-web-track-selectors-alist'."
  (interactive (list (read-string "URL: "
                                  (org-entry-get (point) org-web-track-url))))
  (when (org-before-first-heading-p)
    (org-insert-heading))
  (org-entry-put (point) org-web-track-url url)
  (if (assoc-default url org-web-track-selectors-alist #'string-match)
      (org-web-track-update-entry)
    (message "No selector for the URL. Please set up `org-web-track-selectors-alist'.")))

;;;###autoload
(defun org-web-track-update-entry (&optional marker)
  "Update the tracking item at MARKER.

If called interactively, update the org entry at point.

This command looks up the current values and updates `org-web-track-value'
and `org-web-track-prev-value' if the values have been changed,
then logs them using org's logging feature. The placement of logs respects
`org-log-into-drawer'."
  (interactive (list (point-marker)))
  (when-let* ((track-url (org-entry-get marker org-web-track-url))
              (updates (funcall #'org-web-track-retrieve-values
                                track-url))
              (current-time (format-time-string (org-time-stamp-format t t)))
              (org-log-note-headings (append '((update . "Update %-12s %t"))
                                             org-log-note-headings)))
    (let ((values-in-existence
           (or (org-entry-get-multivalued-property marker org-web-track-value)
               (when-let ((single-val (org-entry-get marker org-web-track-value)))
                 (make-list (length updates) single-val)))))
      (when (or (null values-in-existence)
                (and (not (equal updates values-in-existence))
                     (apply #'org-entry-put-multivalued-property
                            marker org-web-track-prev-value values-in-existence)))
        (apply #'org-entry-put-multivalued-property
               marker org-web-track-value updates)
        (org-with-point-at marker
          (org-add-log-setup 'update
                             ;; work around for stuck process in
                             ;; string conversion at `org-store-log-note'
                             (replace-regexp-in-string "\\(?:%20\\([DSTUdstu]\\)\\)" "_\\1"
                                                       (org-entry-get (point) org-web-track-value))
                             nil 'state current-time)
          (run-hooks 'post-command-hook))
        (org-entry-put marker org-web-track-updated current-time)
        (when (called-interactively-p 'any)
          (message "Updated: %s" (org-entry-get marker org-web-track-value)))
        marker))))

;;;###autoload
(defun org-web-track-update-files ()
  "Update all track items (org entries) in `org-web-track-files'.

Return a list of markers pointing to items where the value has been updated."
  (interactive)
  (delq nil (org-map-entries (lambda ()
                               (org-web-track-update-entry (point-marker)))
                             (format "%s={.+}" org-web-track-url)
                             (org-web-track-files))))

(defun org-web-track-retrieve-values (url &optional async on-success on-fail marker)
  "Retrieve values by accessing the URL.

If ASYNC is non-nil, the procedure will be executed asynchronously.
However, as of now, asynchronous data retrieval is discouraged."
  (pcase-let ((`(,selectors . (,filter))
               (assoc-default url org-web-track-selectors-alist
                              (lambda (car key)
                                (cond
                                 ((functionp car) (funcall car key))
                                 ((stringp car) (string-match-p car key))))))
              (request-backend 'url-retrieve)
              (request-curl-options
               `(,(format "-H \"%s\"" (string-trim (url-http-user-agent-string)))))
              (values nil))
    (unless selectors
      (and (functionp on-fail)
           (progn (apply on-fail marker)
                  (user-error "No selector available for this entry"))))
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
                 (ensure-list
                  (apply
                   (or filter 'list)
                   (flatten-list
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
                            (if (functionp selectors)
                                (list selectors)
                              (ensure-list selectors))))))))))
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
           (message "No value available at the end of selector appliance")))))
    (unless async values)))

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
    (cl-labels ((validate-value (str)
                  (if (stringp str)
                      (if (string-blank-p str)
                          " " (string-trim str))
                    " ")))
      (pcase val
        ((and (pred listp) li) (mapcar (lambda (s) (validate-value s)) li))
        ((and (pred stringp) st) (validate-value st))))))

;;;###autoload
(defun org-web-track-report ()
  "Insert a table whose row represents value change at the time."
  (interactive)
  (let ((table-rows))
    (org-with-wide-buffer
     (org-save-outline-visibility t
       (org-back-to-heading t)
       (org-fold-show-all)
       (let* ((case-fold-search t)
              (subtree-end (save-excursion (org-end-of-subtree)))
              (re (concat (rx (or "Update" "Track") (+ space)
                              "\"" (group (+ not-newline)) "\""
                              (+ space) (opt "on") (* space))
                          org-ts-regexp-inactive))
              (end-mkr (set-marker (mark-marker) subtree-end)))
         (while (re-search-forward re (marker-position end-mkr) t)
           (push `(,(match-string-no-properties 2)
                   ,@(mapcar #'org-entry-restore-space (split-string (match-string-no-properties 1))))
                 table-rows)))))
    (and table-rows
         (org-web-track--insert-table-from table-rows))))

(defun org-web-track--insert-table-from (rows)
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

This command provides a way to invoke `org-web-track-update-entry'
after `org-web-track-agenda-columns'."
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
         (org-fold-show-all)
         (goto-char pos)
         (funcall-interactively #'org-web-track-update-entry))))))

(defun org-web-track-display-values (column-title values)
  "Modify the display of column VALUES for COLUMN-TITLE to be more understandable.

This function is designed to be set for
`org-columns-modify-value-for-display-function'."
  (when (and (org-entry-get (point) org-web-track-url)
             (string-prefix-p (get 'org-web-track-value 'label)
                              column-title))
    (org-web-track-current-changes (point))))

(defun org-web-track-current-changes (&optional pom format separator)
  "Return a string showing the current data change on the item at POM
using FORMAT and SEPARATOR.

If POM is nil, a return value of `point' is used.
FORMAT defines how to describe the current change for a single target and should
contain %p and %c as placeholders for the previous value and current value,
respectively.
SEPARATOR is used in between changes for multiple targets."
  (let (chnages)
    (cl-do ((curr-vals (org-entry-get-multivalued-property (or pom (point)) org-web-track-value) (cdr curr-vals))
            (prev-vals (org-entry-get-multivalued-property (or pom (point)) org-web-track-prev-value) (cdr prev-vals)))
        ((not (or curr-vals prev-vals)) (string-join (nreverse chnages) (or separator ", ")))
      (push (format-spec (or format "%c [%p]")
                         `((?p . ,(or (car prev-vals) "N/A"))
                           (?c . ,(or (car curr-vals) "N/A"))))
            chnages))))

(defvar org-web-track-columns-format
  (apply #'format "%%%sITEM %%%s%s(%s [%s]) %%%s(%s)"
         `(,@(mapcar (lambda (w) (if (= 0 w) "" (format "%d" w)))
                     `(,org-web-track-item-column-width
                       ,org-web-track-update-column-width))
           ,org-web-track-value
           ,(get 'org-web-track-value 'label)
           ,(get 'org-web-track-prev-value 'label)
           ,org-web-track-updated
           ,(get 'org-web-track-updated 'label)))
  "Columns format for `org-web-track-columns' and `org-web-track-agenda-columns'.")

;;;###autoload
(defun org-web-track-columns ()
  "Display a column view specialized for tracking items.

This command invokes `org-columns' with `org-web-track-columns-format'
to display current changes on tracking items along with the updated time."
  (interactive)
  (let ((org-columns-modify-value-for-display-function
         'org-web-track-display-values))
    (org-columns nil org-web-track-columns-format)))

;;;###autoload
(defun org-web-track-agenda-columns ()
  "Display a agenda column view specialized for tracking items.

This command invokes `org-agenda-columns' with `org-web-track-columns-format'
to display current changes on tracking items in `org-web-track-files'
along with the updated time."
  (interactive)
  (let ((org-columns-modify-value-for-display-function
         'org-web-track-display-values)
        (org-overriding-columns-format org-web-track-columns-format)
        (org-agenda-files (org-web-track-files))
        (org-agenda-sorting-strategy '((tags user-defined-up)))
        (org-agenda-cmp-user-defined 'org-web-track-cmp-updated-time)
        (org-agenda-view-columns-initially t))
    (org-tags-view nil (format "%s={.+}" org-web-track-url))))

(defun org-web-track-cmp-updated-time (a b)
  "Compare A and B with respect to their `org-web-track-updated' property.

Return -1 if A has an earlier time stamp indicating that the track item was
updated before B.
Return +1 if B is earlier, and nil if they are equal.

This function is intended to be set for `org-agenda-cmp-user-defined'."
  (cl-labels ((updated-time (ent)
                (ignore-errors
                  (encode-time
                   (org-parse-time-string
                    (org-entry-get (get-text-property 0 'org-hd-marker ent)
                                   org-web-track-updated))))))
    (let ((ta (updated-time a))
          (tb (updated-time b)))
      (cond ((if ta (and tb (time-less-p ta tb)) tb) -1)
            ((if tb (and ta (time-less-p tb ta)) ta) +1)))))

(defun org-web-track-test-selector (url selector &optional filter)
  "Test SELECTOR and FILTER to determine if they are suitable for use with URL.

This function return the values acquired by applying SELECTOR and optionally
FILTER to the HTTP response for URL.
Users can be aware whether SELECTOR and FILTER are usable for
`org-web-track-selectors-alist'.
SELECTOR can be either a single selector or a list of selectors,
whereas FILTER must be singular."
  (let ((org-web-track-selectors-alist
         (append `((,(regexp-quote url) ,selector ,filter))
                 org-web-track-selectors-alist)))
    (org-web-track-retrieve-values url)))

(provide 'org-web-track)

;;; org-web-track.el ends here
