(defconst ts-git-packages
  '(
    ;; git-messenger for github
    git-messenger
    ))

;; https://github.com/syohex/emacs-git-messenger
(defun ts-git/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (defun ts-git/github-browse-commit ()
        "Show the GitHub page for the current commit."
        (interactive)
        (use-package github-browse-file
          :commands (github-browse-file--relative-url))

        (let* ((commit git-messenger:last-commit-id)
               (url (concat "https://github.com/"
                            (github-browse-file--relative-url)
                            "/commit/"
                            commit)))
          (github-browse--save-and-view url)
          (git-messenger:popup-close)))
      (define-key git-messenger-map (kbd "f") '/github-browse-commit))))

;;TODO: In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
