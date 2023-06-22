;;; org-web-track.el --- Web data tracking framework in org mode -*- lexical-binding: t -*-

;; Author: p-snow <p-snow@daisychain.jp>
;; Maintainer: p-snow <p-snow@daisychain.jp>
;; Version: 0.0.1
;; Package-Requires: (org org-agenda request enlive)
;; Homepage: homepage
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

;;

;;; Code:


(require 'org)
(require 'org-agenda)
(require 'url-http)
(require 'request)
(require 'enlive)
(require 'rx)

(defvar org-web-track-update-property "WEB_TRACK_UPDATE"
  "Property name for value.")
(put 'org-web-track-update-property 'label "UPDATE")

(defvar org-web-track-prev-property "WEB_TRACK_PREV"
  "Property name for last value.")
(put 'org-web-track-prev-property 'label "PREV")

(defvar org-web-track-url-property "WEB_TRACK_URL"
  "Property name for tracking URL.")

(defvar org-web-track-date-property "WEB_TRACK_DATE"
  "Property name for the date at which track value.")
(put 'org-web-track-date-property 'label "DATE")

(defvar org-web-track-update-timeout 20
  "Time out in second for accessing web site to get values.")

(defcustom org-web-track-trackers nil
  "An alist for tracking each site.

Each element is for specific site whose car is regexp for the site url and
cdr is a function responsible for tracking data in the site.")

(defvar org-web-track-grant-update t)

(defun org-web-track-initialize (url)
  "Initialize the org entry at point as a web tracking item by putting URL."
  (interactive (list (read-string "URL: ")))
  (org-entry-put (point) org-web-track-url-property url)
  (org-web-track-update))

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
               track-url nil
               #'org-web-track-record
               nil
               (list marker))
      (let ((updates (apply (if (called-interactively-p)
                                #'funcall-interactively
                              #'funcall)
                            #'org-web-track-get-values
                            track-url
                            (list (not async)))))
        (prog1 (org-web-track-record marker updates)
          (when (called-interactively-p)
            (run-hooks 'post-command-hook)))))))

(defun org-web-track-update-all (&optional check-only)
  ""
  (interactive)
  (let ((org-web-track-grant-update (not check-only)))
    (delq nil (org-map-entries (lambda ()
                                 (funcall-interactively 'org-web-track-update))
                               (format "%s={.+}" org-web-track-url-property)


                               org-web-track-files))))

(defun org-web-track-get-values (url &optional sync on-success on-fail marker)
  "Get values by accessing URL."
  (let ((tracker-def (assoc-default url org-web-track-trackers #'string-match))
        (request-backend
         (if (called-interactively-p) 'curl 'url-retrieve))
        (request-curl-options
         `(,(format "-H \"%s\"" (string-trim (url-http-user-agent-string)))))
        (values nil))
    (unless tracker-def
      (and (functionp on-fail)
           (progn (apply on-fail marker)
                  (user-error "No tracker available for this entry"))))
    (request url
      :sync sync
      :timeout org-web-track-update-timeout
      :success
      (cl-function
       (lambda (&key response &allow-other-keys)
         (let* ((content-type (request-response-header response "content-type"))
                (content-charset (if (string-match url-mime-content-type-charset-regexp
                                                   content-type)
                                     (match-string 1 content-type)
                                   ""))
                (content (request-response-data response)))
           (setq values
                 (mapcar (lambda (selector)
                           (string-trim
                            (funcall (cond
                                      ((string-prefix-p "text/html" content-type t)
                                       #'org-web-track--parse-html)
                                      ((string-prefix-p "application/json" content-type t)
                                       #'org-web-track--parse-json))
                                     (if (string-match-p (rx (or "UTF" "utf")
                                                             (? "-") "8")
                                                         content-charset)
                                         (decode-coding-string content 'utf-8)
                                       content)
                                     selector)))
                         (ensure-list tracker-def))))))
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
    (and sync
         values)))

(defun org-web-track--parse-html (html selector)
  "Return tracked value by applying SELECTOR to HTML.

SELECTOR is supposed to be a vector or a string. If vector, it is used as a css
selector."
  (cond
   ((vectorp selector)
    (enlive-text
     (enlive-query (enlive-parse html)
                   selector)))
   ((stringp selector)
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert html)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (if (= 0 (shell-command-on-region (point-min) (point-max)
                                        selector
                                        t t))
          (buffer-substring-no-properties (point-min) (point-max))
        "")))
   (t nil)))

(defun org-web-track--parse-json (json selector)
  "Return tracked value by applying SELECTOR to JSON.

SELECTOR is supposed to be a function that take a json object."
  (let ((json-obj (json-parse-string json
                                     :object-type 'alist)))
    (cond
     ((or (functionp selector)
          (symbolp selector))
      (funcall selector json-obj)))))

(defun org-web-track-record (marker updates)
  "Propagate UPDATES to the entry in consequence of getting updates-in-entry in success.

Return non-nil if value has changed."
  (when (cl-delete-if (lambda (elm) (or (not (stringp elm))
                                    (>= 0 (length elm))))
                      updates)
    (let ((updates-in-entry (org-entry-get-multivalued-property marker org-web-track-update-property))
          (update-time (format-time-string (org-time-stamp-format t t))))
      (setf (alist-get 'track org-log-note-headings)
            "Record %-12s on %t")
      (cl-labels ((update-value ()
                    (when org-web-track-grant-update
                      (apply #'org-entry-put-multivalued-property marker org-web-track-update-property updates)
                      (org-add-log-setup 'track
                                         (org-entry-get (point) org-web-track-update-property)
                                         nil 'state update-time)))
                  (update-last-value ()
                    (when org-web-track-grant-update
                      (apply #'org-entry-put-multivalued-property marker org-web-track-prev-property updates-in-entry))))
        (when org-web-track-grant-update
          (org-entry-put marker org-web-track-date-property update-time))
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
              (re (concat (rx "- Record "
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
  :type 'natnum)

(defcustom org-web-track-update-column-width 0
  "0 means unspecified."
  :type 'natnum)

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
  :type '(repeat :tag "List of files" file))

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

User can test their tracker without setting `org-web-track-trackers'."
  (let ((org-web-track-trackers
         (append `((,(regexp-quote url) ,tracker))
                 org-web-track-trackers)))
    (princ (org-web-track-get-values url t nil))))

(provide 'org-web-track)

;;; org-web-track.el ends here
