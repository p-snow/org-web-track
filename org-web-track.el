;;; org-web-track.el --- Web data tracking framework in Org Mode -*- lexical-binding: t -*-

;; Author: p-snow <public@p-snow.org>
;; Maintainer: p-snow <public@p-snow.org>
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (request "0.3.0") (enlive "0.0.1") (gnuplot "0.8.1"))
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

;; Org Web Track provide an Org Mode framework that assists users in managing
;; their values of interest on a website or via a Web API.

;; Through the use of Org Web Track:
;; - Any location in the web page or any piece in the web API response can be an
;;   item to track
;; - Users can monitor changes for the item and manage them using the facilities
;;   of Org mode
;; - A set of items to track can be reviewed with their updated values in an Org
;;   column view
;; - Users can extract an Org table displaying value changes and create a visual
;;   graph using Gnuplot.


;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-colview)
(provide 'org-plot)
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
  "Org property for a URL to access.")

(defvar org-web-track-http-headers "TRACK_HTTP_HEADERS"
  "Org property for headers in HTTP request.")

(defvar org-web-track-unix-socket "TRACK_UNIX_SOCKET"
  "Org property for a Unix Domain Socket path to connect.")

(defvar org-web-track-updated "TRACK_LAST_UPDATED_TIME"
  "Property name for the date at which track value.")
(put 'org-web-track-updated 'label "UPDATED TIME")

(defvar org-web-track-update-timeout 20
  "Time out in second for accessing web site to get values.")

(defvar org-web-track-value-column-format "%c [%p]"
  "Column format where current and previous values are placed in %c and %p.")

;;;; Customization

(defgroup org-web-track nil
  "Options for `org-web-track'."
  :group 'org
  :link '(url-link "https://github.com/p-snow/org-web-track"))

(defcustom org-web-track-selectors-alist nil
  "An alist of URL-MATCH and SELECTOR(S).

URL-MATCH specifies the URL to which SELECTOR will be applied. SELECTOR is
utilized to extract the desired value from the content of the URL. A single
URL-MATCH links to one or more SELECTORS. Therefore, each element appears as
\(URL-MATCH . SELECTOR) or (URL-MATCH SELECTORS...).

URL-MATCH:
URL-MATCH must be either a string or a function. The string is used as a regexp
to determine the URL that match against it. The function takes one argument, a
URL candidate, and must return non-nil if this selector can deal with the URL.

SELECTOR:
SELECTOR must be either a vector, a function or a string.

If SELECTOR is a vector, it is assumed to represent a CSS selector that is used
to determine the result value. The selection rules are as follows: the text of
the selected tag will serve as the result value.

  id              : [:main]
  class           : [.content.home-page]
  tagname         : [p.demo a]
  direct children : [.article > p]
  all nodes       : [:section > *]

org-web-track delegates CSS selector procedure to enlive package. For more
details, please visit the website.

  enlive: https://github.com/zweifisch/enlive

If SELECTOR is a function, it must accept a data object and return a value.

The type of the data object depends on the MIME type of the HTTP response. If
the MIME type is text/html, the data object is an HTML parse tree obtained from
`libxml-parse-html-region'. Likewise, if the MIME type is application/xml, the
data object will be an XML parse tree from `libxml-parse-xml-region', if the
MIME type is application/json, an ELisp object from `json-parse-string' will be
the data object, and if the MIME type is text/plain, the content itself will be
the data object as a string.

The return value of the function must be either a string, a number, or a list of
those. If a list is returned, each element represents a tracking unit that will
be compared over time.

If SELECTOR is a string, it is assumed to be a shell command that should accept
the HTTP response content as stdin and return a value as stdout."
  :type '(alist :key-type (string :tag "Regexp")
                :value-type
                (choice (vector :tag "A CSS selector")
                        (string :tag "A shell command that processes HTTP response content")
                        function))
  :group 'org-web-track)

(defcustom org-web-track-content-fetcher-alist nil
  "An alist of URL-MATCH and CONTENT-FETCHER.

URL-MATCH specifies the URL for which CONTENT-FETCHER is
responsible. If CONTENT-FETCHER is specified for the URL,
org-web-track delegates the HTTP connection and content fetching
to it and receives the content, which will then be applied to
SELECTOR.

CONTENT-FETCHER must be either a function or a string.

If it is a function, it must accept a single argument - a URL. It
should then return the content obtained by accessing the URL,
presented as a string. If the content is anything other than an
HTML, the return format has to be (MIME-TYPE . CONTENT), where
the supported MIME-TYPE can be either \"text/html\" ,
\"application/xml\", \"application/json\" or \"text/plain\" as a
string.

If CONTENT-FETCHER is a string, it is interpreted as a shell
command string. In the shell command, %s may appear, which is
replaced with a URL at execution time. When %s is not present,
the shell command can receive the URL via stdin.

The shell command does not have a method to return the MIME type.
Therefore, CONTENT-FETCHER, which returns a content other than
HTML, needs to be implemented as a function."
  :type '(alist :key-type (string :tag "Regexp")
                :value-type function)
  :group 'org-web-track)

(defcustom org-web-track-files nil
  "The files to be used in `org-web-track-agenda-columns'.

Its value must be either a list of strings representing file paths
or a function that returns the same data structure."
  :type '(choice
          (repeat :tag "List of files" file)
          (function))
  :group 'org-web-track)

(defcustom org-web-track-use-curl
  (not (null (executable-find "curl")))
  "Whether to use cURL as the fetching backend instead of `url-retrieve'.

If non-nil, the cURL program at a searchable location will be
used to fetch web content. Otherwise, the Elisp function
`url-retrieve' will be used.

This can be a string, which indicates the path for the cURL
executable to be used.

Note that even if this is nil, the cURL program will still be
used when the tracking item prefers to access the Unix Socket
Server (which means the org entry has
`org-web-track-set-unix-socket' property), as `url-retrieve'
cannot access the Unix Socket Server."
  :group 'org-web-track
  :package-version '(org-web-track . "0.0.3")
  :type 'boolean)

(defcustom org-web-track-default-http-headers
  (list (string-trim (url-http-user-agent-string)))
  "Default HTTP http-headers that are sent in every HTTP request session."
  :group 'org-web-track
  :package-version '(org-web-track . "0.1.0")
  :type '(list (string :tag "Field name and value pair")))

(defcustom org-web-track-trim-values t
  "Whether to trim retrieved values.

Non-nil means to remove preceding and trailing whitespace
characters from the values returned by each selector."
  :group 'org-web-track
  :package-version '(org-web-track . "0.1.0")
  :type 'boolean)

(defcustom org-web-track-report-date-format nil
  "Date format in the report created by `org-web-track-report'.

This format string will be assigned to `format-time-string'. If
it is not a string, ISO 8601 date format (%+4Y-%m-%d) will be
used."
  :type 'string
  :package-version '(org-web-track . "0.1.0")
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
initially.

After the URL has been set, try to retrieve a valuees if there is
an appropriate selector in `org-web-track-selectors-alist'."
  (interactive (list (when (or (null (org-entry-get (point) org-web-track-url))
                               (y-or-n-p (format "Are you sure of overwriting existing %s property?" org-web-track-url)))
                       (read-string "URL: "))))
  (when (stringp url)
    (when (org-before-first-heading-p)
      (org-insert-heading))
    (org-entry-put (point) org-web-track-url url)
    (org-web-track-update-entry)))

;;;###autoload
(defun org-web-track-set-http-headers (epom http-headers)
  "Set HTTP-HEADERS for the entry at EPOM.

If called interactively, a minibuffer appears and the user is
required to submit HTTP headers on it for the entry at point.

The field name and value in HTTP headers must be delimtted by
colon and multiple headers must be separated by a newline. When
users write HTTP headers in the minibuffer, they should use `C-q'
`C-j', instead of RET, for a newline. This is because pressing RET
triggers an exit in the minibuffer."
  (interactive (let* ((headers-text (org-entry-get-multivalued-property nil org-web-track-http-headers))
                      (headers-text-anew (read-from-minibuffer "" (string-join headers-text "\n"))))
                 (list nil headers-text-anew)))
  (apply #'org-entry-put-multivalued-property
         epom org-web-track-http-headers
         (split-string http-headers "[\n\r]+" t (rx space))))

;;;###autoload
(defun org-web-track-set-unix-socket (epom unix-socket)
  "Set UNIX-SOCKET for the entry at EPOM.

If called interactively, a minibuffer appears and the user is
required to input a Unix socket path for the entry at point."
  (interactive (let* ((unix-sock (org-entry-get nil org-web-track-unix-socket))
                      (unix-sock-anew (read-file-name "Unix socket: " nil nil nil unix-sock)))
                 (list nil unix-sock-anew)))
  (org-entry-put epom org-web-track-unix-socket unix-socket))

;;;###autoload
(defun org-web-track-update-entry (&optional marker)
  "Update the tracking item at MARKER.

If called interactively, update the org entry at point.

This command looks up the current values and updates `org-web-track-value'
and `org-web-track-prev-value' if the values have been changed,
then logs them using org's logging feature.  The placement of logs respects
the configuration in the variable `org-log-into-drawer'."
  (interactive (list (point-marker)))
  (when-let* ((track-url (or (rx-let ((url-re (seq (regexp "https?://") (+ graph))))
                               (pcase (org-entry-get marker org-web-track-url)
                                 ((rx "[[" (let link url-re) "][" (* print) "]]") link)
                                 ((rx "[[" (let link url-re) "]]") link)
                                 ((rx (let url url-re)) url)))
                             (user-error "No valid %s property" org-web-track-url)))
              (selectors
               (or (ensure-list
                    (assoc-default track-url org-web-track-selectors-alist
                                   (lambda (car key)
                                     (pcase car
                                       ((and (pred functionp) match-fun)
                                        (funcall match-fun key))
                                       ((and (pred stringp) match-str)
                                        (string-match-p match-str key))))))
                   (user-error "No selector found responsible for %s in org-web-track-selectors-alist"
                               track-url)))
              (updates
               (if-let* ((fetcher-cdr (assoc-default track-url org-web-track-content-fetcher-alist
                                                     (lambda (car key) (string-match-p car key))))
                         (content (pcase fetcher-cdr
                                    ((or (and (pred functionp)
                                              fetcher-fun)
                                         (and `(,(pred functionp) . ,_)
                                              (let fetcher-fun (car fetcher-cdr))))
                                     (funcall fetcher-fun track-url))
                                    ((and (pred stringp) fetcher-cmd)
                                     (with-temp-buffer
                                       (when (= 0 (call-shell-region (insert track-url) nil
                                                                     (format fetcher-cmd (shell-quote-argument track-url))
                                                                     t t))
                                         (buffer-substring-no-properties (point-min) (point-max))))))))
                   (apply #'org-web-track--apply-selectors
                          (pcase content
                            ((and (pred stringp) str-cnt) `("text/html" ,str-cnt ,selectors))
                            (`(,mtype . ,cnt)
                             `(,mtype ,(or (and (listp cnt)
                                                (car cnt))
                                           cnt)
                                      ,selectors))))
                 (org-web-track-retrieve-values track-url
                                                selectors
                                                (org-entry-get-multivalued-property marker org-web-track-http-headers)
                                                (org-entry-get marker org-web-track-unix-socket))))
              (current-time (format-time-string (org-time-stamp-format t t)))
              (org-log-note-headings (append '((update . "Update %-12s %t"))
                                             org-log-note-headings)))
    (let ((incumbent-values
           (or (org-entry-get-multivalued-property marker org-web-track-value)
               (make-list (length updates) " "))))
      (when (or (null incumbent-values)
                (and (not (equal updates incumbent-values))
                     (apply #'org-entry-put-multivalued-property
                            marker org-web-track-prev-value incumbent-values)))
        (apply #'org-entry-put-multivalued-property
               marker org-web-track-value updates)
        (let ((inhibit-message t)) ; inhibit "Note Stored" message
          (org-with-point-at marker
            (org-add-log-setup 'update
                               ;; work around for stuck process in
                               ;; string conversion at `org-store-log-note'
                               (replace-regexp-in-string "\\(?:%20\\([DSTUdstu]\\)\\)" "_\\1"
                                                         (org-entry-get (point) org-web-track-value))
                               nil 'state current-time)
            (run-hooks 'post-command-hook)))
        (org-entry-put marker org-web-track-updated current-time)
        (when (called-interactively-p 'any)
          (message "Updated: %s" (org-entry-get marker org-web-track-value)))
        marker))))

;;;###autoload
(defun org-web-track-update-files ()
  "Update all track items in variable `org-web-track-files'.

Return a list of markers pointing to items where the value has been updated."
  (interactive)
  (delq nil (org-map-entries (lambda ()
                               (org-web-track-update-entry (point-marker)))
                             (format "%s={.+}" org-web-track-url)
                             (org-web-track-files))))

(defun org-web-track-retrieve-values (url selectors &optional http-headers unix-socket)
  "Retrieve values using URL, SELECTORS, HTTP-HEADERS and UNIX-SOCKET.

If an optional argument UNIX-SOCKET is provided as a path for a Unix Domain
Socket to connect, the function will attempt to access the HTTP socket server
running on the local machine instead of the WWW server."
  (pcase-let ((request-backend (if (or org-web-track-use-curl unix-socket)
                                   'curl 'url-retrieve))
              (request-curl (if (stringp org-web-track-use-curl)
                                org-web-track-use-curl
                              (default-value 'request-curl)))
              (`(,request-curl-options . ,url-request-extra-headers)
               (let ((header-list (append org-web-track-default-http-headers
                                          (when (listp http-headers)
                                            http-headers))))
                 `(,(mapcar (lambda (header)
                              (format "-H %s" header))
                            header-list)
                   . ,(mapcar (lambda (header)
                                (split-string header (rx (seq ":" (0+ space)))))
                              header-list))))
              (values nil))
    (request url
      :sync t
      :timeout org-web-track-update-timeout
      :unix-socket unix-socket
      :success
      (cl-function
       (lambda (&key response &allow-other-keys)
         (setq values
               (org-web-track--apply-selectors
                (request-response-header response "content-type")
                (request-response-data response)
                selectors))))
      :error
      (cl-function
       (lambda (&key response &allow-other-keys)
         (when (eq (request-response-symbol-status response) 'timeout)
           (setq values nil))
         (message "HTTP error occurred: %s\n  URL: %s"
                  (request-response-error-thrown response)
                  url)))
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (unless (seq-some 'stringp values)
           (message "No value was retrieved\n  URL: %s" url))
         data)))
    values))

(defmacro org-web-track--with-content-buffer (content &rest body)
  "Execute BODY in the temporary buffer where CONTENT is inserted."
  (declare (indent 1))
  `(with-temp-buffer
     (set-buffer-multibyte t)
     (insert ,content)
     ,@body))

(defun org-web-track--apply-selectors (content-type content selectors)
  "Apply SELECTORS to CONTENT where HTTP Content-Type is CONTENT-TYPE."
  (pcase-let* ((`(,mime-subtype . ,decoded-content)
                (save-match-data
                  (and (string-match
                        (rx (seq (or (seq "application/" (group-n 1 (or "json" "xml")))
                                     (seq "text/" (group-n 1 (or "html" "plain"))))
                                 (opt (regexp url-mime-content-type-charset-regexp))))
                        content-type)
                       (cons (intern (match-string 1 content-type))
                             (decode-coding-string content
                                                   (car (member
                                                         (intern (downcase (or (match-string 2 content-type)
                                                                               "utf-8")))
                                                         (coding-system-list)))))))))
    (ensure-list
     (flatten-tree
      (mapcar
       (lambda (selector)
         (mapcar (lambda (val)
                   (let ((crude-val (pcase val
                                      ((and (pred stringp) str)
                                       (if org-web-track-trim-values
                                           (string-trim val) str))
                                      ((and (pred numberp) num)
                                       (number-to-string num))
                                      (_ ""))))
                     ;; Replace an empty string with a single space
                     ;; since `org-entry-put-multivalued-property' does not accept an empty string
                     (if (string-empty-p crude-val)
                         " " crude-val)))
                 (ensure-list
                  (pcase `(,mime-subtype . ,selector)
                    (`(,_ . ,(and (pred stringp) selector-command))
                     (org-web-track--with-content-buffer content
                       (when (= 0 (shell-command-on-region (point-min) (point-max)
                                                           selector-command t t))
                         (buffer-substring-no-properties (point-min) (point-max)))))
                    (`(,subtype . ,(and (pred functionp) selector-func))
                     (funcall selector-func
                              (cl-case subtype
                                (json (json-parse-string decoded-content :object-type 'alist))
                                (html (org-web-track--with-content-buffer decoded-content
                                        (libxml-parse-html-region)))
                                (xml (org-web-track--with-content-buffer decoded-content
                                       (libxml-parse-xml-region)))
                                (plain decoded-content))))
                    (`(html . ,(and (pred vectorp) css-selector))
                     (enlive-text (enlive-query (enlive-parse decoded-content)
                                                css-selector)))))))
       ;; ensure that SELECTORS is a list, except in the case of lambda
       (if (or (nlistp selectors)
               (eq (car selectors) 'lambda))
           (list selectors)
         selectors))))))

;;;###autoload
(defun org-web-track-report (&optional plot)
  "Insert an Org table displaying values in chronological order, then PLOT a graph.

If called with the \\[universal-argument] prefix, the table will be inserted
with #+PLOT special headers, which are recognizable for org-plot to create a
graph. After that, the graph will be display automatically. Note that this
feature requires the Gnuplot program and the Emacs gnuplot package."
  (interactive "P")
  (let ((table-rows))
    (org-with-wide-buffer
     (org-save-outline-visibility t
       (org-back-to-heading t)
       (let* ((case-fold-search t)
              (subtree-end (save-excursion
                             (org-next-visible-heading 1)
                             (point)))
              (re (concat (rx (or "Update" "Track") (+ space)
                              "\"" (group (+ not-newline)) "\""
                              (+ space) (opt "on") (* space))
                          org-ts-regexp-inactive)))
         (while (re-search-forward re subtree-end t)
           (push `(,(encode-time (parse-time-string (match-string-no-properties 2)))
                   ,@(mapcar #'org-entry-restore-space (split-string (match-string-no-properties 1))))
                 table-rows)))))
    (when table-rows
      (save-excursion
        (let ((num-column (1- (apply 'max (mapcar #'length table-rows))))
              (format-string (or org-web-track-report-date-format "%Y-%m-%d")))
          (sort table-rows
                (pcase-lambda (`(,time-a ,_) `(,time-b ,_))
                  (time-less-p time-a time-b)))
          (when (equal plot '(4))
            (insert (format "#+PLOT: ind:1 deps:%s with:boxes type:2d\n"
                            (cl-loop for i from 2 below (+ 2 num-column)
                                     collect i)))
            (insert "#+PLOT: set:\"xdata time\"\n")
            (insert (format "#+PLOT: set:\"timefmt '%s'\"\n" format-string))
            (insert (apply #'format "#+PLOT: set:\"xrange ['%s':'%s']\"\n"
                           (mapcar (apply-partially
                                    #'format-time-string "%F")
                                   `(,(time-add (car (first table-rows)) (* -60 60 24))
                                     ,(time-add (caar (last table-rows)) (* 60 60 24)))))))
          (insert "|DATE")
          (cl-dotimes (num num-column)
            (insert (format "|VALUE %d" (1+ num))))
          (insert "\n|--\n")
          (mapc (pcase-lambda (`(,time . ,values))
                  (insert (concat "| " (format-time-string format-string time)))
                  (mapc (lambda (value)
                          (insert (concat "| "
                                          (if (equal plot '(4))
                                              (org-web-track--extract-number value)
                                            value))))
                        values)
                  (insert " |\n"))
                table-rows)
          (org-table-align)))
      (when (and (equal plot '(4))
                 (called-interactively-p 'any)
                 (fboundp 'org-plot/gnuplot))
        (org-plot/gnuplot)))))

(defun org-web-track--extract-number (string)
  "Extract an integer or floating-point number representation from STRING."
  (let ((string-sans-comma (replace-regexp-in-string "\\([0-9]\\),\\([0-9]\\)" "\\1\\2" string)))
    (and (string-match "[-+]?\\([0-9]*\\.[0-9]+\\|[0-9]+\\.?[0-9]*\\)" string-sans-comma)
         (match-string 0 string-sans-comma))))

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
  (and (org-entry-get (point) org-web-track-url)
       (string= column-title
                (format-spec org-web-track-value-column-format
                             `((?p . ,(get 'org-web-track-prev-value 'label))
                               (?c . ,(get 'org-web-track-value 'label)))))
       (string= values
                (org-entry-get (point) org-web-track-value))
       (org-web-track-current-changes (point))))

(defun org-web-track-current-changes (&optional pom format separator)
  "Return the current data change on the item at POM using FORMAT and SEPARATOR.

If POM is nil, a return value of `point' is used.
FORMAT defines how to describe the current change for a single target and should
contain %p and %c as placeholders for the previous value and current value,
respectively.
SEPARATOR is used in between changes for multiple targets."
  (let (chnages)
    (cl-do ((curr-vals (org-entry-get-multivalued-property (or pom (point)) org-web-track-value) (cdr curr-vals))
            (prev-vals (org-entry-get-multivalued-property (or pom (point)) org-web-track-prev-value) (cdr prev-vals)))
        ((not (or curr-vals prev-vals)) (string-join (nreverse chnages) (or separator ", ")))
      (push (format-spec (or format org-web-track-value-column-format)
                         `((?p . ,(or (car prev-vals) "N/A"))
                           (?c . ,(or (car curr-vals) "N/A"))))
            chnages))))

(defun org-web-track-columns-format ()
  "Return columns format for `org-columns' for org-web-track."
  (apply #'format "%%%sITEM %%%s%s(%s) %%%s(%s)"
         `(,@(mapcar (lambda (w) (if (= 0 w) "" (format "%d" w)))
                     `(,org-web-track-item-column-width
                       ,org-web-track-update-column-width))
           ,org-web-track-value
           ,(format-spec org-web-track-value-column-format
                         `((?p . ,(get 'org-web-track-prev-value 'label))
                           (?c . ,(get 'org-web-track-value 'label))))
           ,org-web-track-updated
           ,(get 'org-web-track-updated 'label))))

;;;###autoload
(defun org-web-track-columns ()
  "Display a column view specialized for tracking items.

This command invokes `org-columns' with `org-web-track-columns-format'
to display current changes on tracking items along with the updated time."
  (interactive)
  (let ((org-columns-modify-value-for-display-function
         'org-web-track-display-values))
    (org-columns nil (org-web-track-columns-format))))

;;;###autoload
(defun org-web-track-agenda-columns ()
  "Display a agenda column view specialized for tracking items.

This command invokes `org-agenda-columns' with `org-web-track-columns-format'
to display current changes on tracking items in variable `org-web-track-files'
along with the updated time."
  (interactive)
  (let ((org-columns-modify-value-for-display-function
         'org-web-track-display-values)
        (org-columns-default-format-for-agenda (org-web-track-columns-format))
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

(provide 'org-web-track)

;;; org-web-track.el ends here
