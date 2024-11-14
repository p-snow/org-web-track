;;; unit-test--update-entry.el --- Unit test for org-web-track -*- lexical-binding: t -*-

;; Author: p-snow
;; Package-Requires: ((mocker "0.5.0"))
;; Homepage: https://github.com/p-snow/org-web-track

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

(require 'ert)
(require 'mocker)
(require 'org-web-track)
(require 'project)

(defvar test--org-web-track-selectors-alist
  '(("example.com" [.price])))

(ert-deftest test--update-entry--track-url ()
  "Test `org-web-track-update-entry' with various URL patterns."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (expand-file-name "tests/track-entry.org"
                                            (project-root (project-current))))

    (let* ((org-web-track-selectors-alist test--org-web-track-selectors-alist)
           (current-time-sans-sec (decode-time (current-time))))
      (setf (nth 0 current-time-sans-sec) 0)

      (mocker-let ((org-web-track-retrieve-values (url sel x y)
                                                  ((:input '("https://www.example.com/product01" ([\.price]) nil nil) :output '("$3" "$30")))))
        ;; test B-000
        (re-search-forward "* B-000:")
        (should-error (org-web-track-update-entry))
        (should (equal (org-entry-get-multivalued-property nil org-web-track-value)
                       '("$2" "$20")))
        (should (equal (org-entry-get-multivalued-property nil org-web-track-prev-value)
                       '("$1" "$10")))
        ;; test B-001
        (re-search-forward "* B-001:")
        (org-web-track-update-entry)
        (should (equal (org-entry-get-multivalued-property nil org-web-track-value)
                       '("$3" "$30")))
        (should (equal (org-entry-get-multivalued-property nil org-web-track-prev-value)
                       '("$2" "$20")))
        (should (time-equal-p current-time-sans-sec
                              (org-parse-time-string (org-entry-get nil org-web-track-updated))))
        ;; test B-002
        (re-search-forward "* B-002:")
        (org-web-track-update-entry)
        (should (equal (org-entry-get-multivalued-property nil org-web-track-value)
                       '("$3" "$30")))
        (should (equal (org-entry-get-multivalued-property nil org-web-track-prev-value)
                       '("$2" "$20")))
        (should (time-equal-p current-time-sans-sec
                              (org-parse-time-string (org-entry-get nil org-web-track-updated))))
        ;; test B-003
        (re-search-forward "* B-003:")
        (org-web-track-update-entry)
        (should (equal (org-entry-get-multivalued-property nil org-web-track-value)
                       '("$3" "$30")))
        (should (equal (org-entry-get-multivalued-property nil org-web-track-prev-value)
                       '("$2" "$20")))
        (should (time-equal-p current-time-sans-sec
                              (org-parse-time-string (org-entry-get nil org-web-track-updated))))
        )
      )))

(provide 'test-org-web-track)
;;; unit-test--update-entry.el ends here
