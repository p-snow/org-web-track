;;; test-org-web-track.el --- Unit test for org-web-track -*- lexical-binding: t -*-

;; Author: p-snow
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
(require 'org-web-track)
(require 'project)

(defvar test--org-web-track-selectors-alist
  '(("example.com" [.price])))

(ert-deftest test--setup-entry--invalid-url--noninteractive ()
  "Test `org-web-track-setup-entry' with a URL that does not match any selector."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (expand-file-name "tests/tracking-items.org"
                                            (project-root (project-current))))
    (let ((url "https://dummy.com")
          (sock "/tmp/test.sock")
          (org-web-track-selectors-alist test--org-web-track-selectors-alist))
      (goto-char (point-min))
      (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                     (should-error (org-web-track-setup-entry url))))
      (should (org-at-heading-p))
      (should (string= (org-entry-get (point) org-web-track-url)
                       url))
      (re-search-forward "* 001:")
      (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                     (should-error (org-web-track-setup-entry url))))
      (should (string= (org-entry-get (point) org-web-track-url)
                       url))
      (re-search-forward "* 002:")
      (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                     (should-error (org-web-track-setup-entry url))))
      (should (string= (org-entry-get (point) org-web-track-url)
                       url))
      (re-search-forward "* 003:")
      (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                     (should-error (org-web-track-setup-entry url))))
      (should (string= (org-entry-get (point) org-web-track-url)
                       url))
      (re-search-forward "* 004:")
      (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                     (should-error (org-web-track-setup-entry url)))))))

(ert-deftest test--setup-entry--invalid-url--interactive ()
  "Test `org-web-track-setup-entry' as an interactive command with a URL that does not match any selector."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (expand-file-name "tests/tracking-items.org"
                                            (project-root (project-current))))
    (let ((url "https://dummy.com")
          (sock "/tmp/test.sock")
          (org-web-track-selectors-alist test--org-web-track-selectors-alist)
          (executing-kbd-macro t))
      (goto-char (point-min))
      (let ((unread-command-events (listify-key-sequence
                                    (kbd (format "%s RET" url)))))
        (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                       (should-error (call-interactively #'org-web-track-setup-entry))))
        (should (org-at-heading-p))
        (should (string= (org-entry-get (point) org-web-track-url)
                         url)))
      (let ((unread-command-events (listify-key-sequence
                                    (kbd (format "%s RET" url)))))
        (re-search-forward "* 001:")
        (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                       (should-error (call-interactively #'org-web-track-setup-entry))))
        (should (string= (org-entry-get (point) org-web-track-url)
                         url)))
      (let ((unread-command-events (listify-key-sequence
                                    (kbd (format "y %s %s RET"
                                                 (if noninteractive "RET" "")
                                                 url)))))
        (re-search-forward "* 002:")
        (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                       (should-error (call-interactively #'org-web-track-setup-entry))))
        (should (string= (org-entry-get (point) org-web-track-url)
                         url)))
      (let ((unread-command-events (listify-key-sequence
                                    (kbd (format "y %s %s RET"
                                                 (if noninteractive "RET" "")
                                                 url)))))
        (re-search-forward "* 003:")
        (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                       (should-error (call-interactively #'org-web-track-setup-entry))))
        (should (string= (org-entry-get (point) org-web-track-url)
                         url)))
      (let ((unread-command-events (listify-key-sequence
                                    (kbd (format "y %s %s RET"
                                                 (if noninteractive "RET" "")
                                                 url)))))
        (re-search-forward "* 004:")
        (should (equal `(user-error ,(format "No selector found responsible for %s in org-web-track-selectors-alist" url))
                       (should-error (call-interactively #'org-web-track-setup-entry))))
        (should (string= (org-entry-get (point) org-web-track-url)
                         url))
        (should (string= (org-entry-get (point) org-web-track-unix-socket)
                         sock))))))

(provide 'test-org-web-track)
;;; unit-test--org-web-track.el ends here
