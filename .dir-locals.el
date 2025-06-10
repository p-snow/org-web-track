((nil . ((eval . (defun proj/org-export-readme-md ()
                   (let ((org-export-exclude-tags '("noexport_md")))
                     (org-md-export-to-markdown))
                   (rename-file "org-web-track.md" "README.md"))))))
