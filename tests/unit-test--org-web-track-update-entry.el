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

(ert-deftest test--update-entry ()
  "Test `org-web-track-setup-entry' with a URL that does not match any selector."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (expand-file-name "tests/track-entry.org"
                                            (project-root (project-current))))

    (let ((org-web-track-selectors-alist test--org-web-track-selectors-alist))
      (re-search-forward "* B-001:")
      (mocker-let ((org-web-track-retrieve-values (url sel x y)
                                                  ((:input '("https://www.example.com/product01" ([\.price]) nil nil) :output '("$20")))))
        (org-web-track-update-entry)
        (should (string= (org-entry-get nil org-web-track-value)
                         "$20"))))))

(provide 'test-org-web-track)
;;; unit-test--update-entry.el ends here
