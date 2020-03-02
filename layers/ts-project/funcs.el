(defun ts-project/open-file-with-projectile-or-lsgit ()
  "open file in current projectfile"
  (interactive)
  (if (ts-project/project-root)
      (counsel-git)
    (find-file-in-project)
    ))

(defun ts-project/project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (or (locate-dominating-file directory ".git"))))
