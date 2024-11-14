;;; unit-test--report.el --- Unit test for org-web-track -*- lexical-binding: t -*-

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

(ert-deftest test--report--no-plot ()
  "Test `org-web-track-report' non plotting scenario."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (expand-file-name "tests/track-entry.org"
                                            (project-root (project-current))))

    (re-search-forward "* L-000:")
    (org-next-visible-heading 1)
    (org-open-line 1)
    (org-web-track-report)
    (let ((answer (concat "|       DATE | VALUE 1 | VALUE 2 |\n"
                          "|------------+---------+---------|\n"
                          "| 2024-11-12 | ¥1,000  | ¥100    |\n"
                          "| 2024-11-13 | ¥2,000  | ¥200    |\n"
                          "| 2024-11-14 | ¥3,000  | ¥300    |\n")))
      (should (string-equal (buffer-substring-no-properties (point) (org-table-end))
                            answer)))))

(ert-deftest test--report--plot ()
  "Test `org-web-track-report' plotting scenario."
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (expand-file-name "tests/track-entry.org"
                                            (project-root (project-current))))

    (re-search-forward "* L-000:")
    (org-next-visible-heading 1)
    (org-open-line 1)
    (org-web-track-report '(4))
    (let ((answer (concat "#+PLOT: ind:1 deps:(2 3) with:boxes type:2d\n"
                          "#+PLOT: set:\"xdata time\"\n"
                          "#+PLOT: set:\"timefmt '%Y-%m-%d'\"\n"
                          "#+PLOT: set:\"xrange ['2024-11-11':'2024-11-15']\"\n"
                          "|       DATE | VALUE 1 | VALUE 2 |\n"
                          "|------------+---------+---------|\n"
                          "| 2024-11-12 |    1000 |     100 |\n"
                          "| 2024-11-13 |    2000 |     200 |\n"
                          "| 2024-11-14 |    3000 |     300 |\n")))
      (should (string-equal (buffer-substring-no-properties (point)
                                                            (save-excursion
                                                              (re-search-forward "|")
                                                              (org-table-end)))
                            answer)))))

;;; unit-test--report.el ends here
