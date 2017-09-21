(defconst ts-git-packages
  '(
    ;; git-messenger for github
    git-messenger
    magit
    helm-github-stars                   ;; for github start
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
          :defer t
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


(defun ts-git/post-init-magit ()
  (progn
    (with-eval-after-load 'magit
      (progn
        (add-to-list 'magit-no-confirm 'stage-all-changes)
        (define-key magit-log-mode-map (kbd "W") 'magit-copy-as-kill)
        (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
        (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
        (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
        (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)
        (setq magit-completing-read-function 'magit-builtin-completing-read)

        ;; http://emacs.stackexchange.com/questions/6021/change-a-branchs-upstream-with-magit/6023#6023
        (magit-define-popup-switch 'magit-push-popup ?u
          "Set upstream" "--set-upstream")
        ;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
        ;; (add-hook 'magit-section-set-visibility-hook '(lambda (section) (let ((section-type (magit-section-type section)))
        ;;                                                              (if (or (eq 'untracked section-type)
        ;;                                                                      (eq 'stashes section-type))
        ;;                                                                  'hide))))
        ))

    ;; Githu PR settings
    ;; "http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html"
    (setq magit-repository-directories '("~/cocos2d-x/"))
    (setq magit-push-always-verify nil)


    (defun ts-git/magit-visit-pull-request ()
      "Visit the current branch's PR on GitHub."
      (interactive)
      (let ((remote-branch (magit-get-current-branch)))
        (cond
         ((null remote-branch)
          (message "No remote branch"))
         (t
          (browse-url
           (format "https://github.com/%s/pull/new/%s"
                   (replace-regexp-in-string
                    "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                    (magit-get "remote"
                               (magit-get-remote)
                               "url"))
                   remote-branch))))))

    (eval-after-load 'magit
      '(define-key magit-mode-map (kbd "s-g")
         #'ts-git/magit-visit-pull-request))


    (setq magit-process-popup-time 10)))


(defun ts-git/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "tinysong")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))
