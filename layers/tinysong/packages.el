;;; packages.el --- TinySong layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;

;; Author: song <song@GE60-SHADOW-Walker.lan>
;; URL: https://github.com/TinySong/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:


;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `TinySong-packages'. Then, for each package PACKAGE:
;;

;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `TinySong/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `TinySong/pre-init-PACKAGE' and/or
;;   `TinySong/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst tinysong-packages
  ;; (setq tinysong-packages
  '(
    ;; (hackernews :location build-in)
    js-doc
    smartparens
    markdown-mode
    erc
    ;; document create
    (doxymacs :location local)
    evil-vimish-fold
    beacon
    ctags-update
    projectile
    hl-anything
    keyfreq
    ;; git-messenger for github
    git-messenger
    ;; visual-regexp-steroids and visual-regexp are reg serarch, steroids is an externsion to visual-regexp
    visual-regexp
    visual-regexp-steroids
    flycheck
    cmake-font-lock
    cmake-mode
    google-c-style
    ;; post extension names go here
    ;; nodejs-repl-eval don't support es6 and js2-mode also don't support it
    ;; so I use js-comit instead.
    nodejs-repl
    wrap-region
    youdao-dictionary
    ;; TODO: https://www.emacswiki.org/emacs/FindFileInProject
    find-file-in-project
    deft
    ;; header2
    ;; (unicad :location local)
    ;; worf
    ))


;;; packages.el ends here

;; TODO: https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html

;; package init

(defun tinysong/post-init-js-doc ()
  (setq js-doc-mail-address "TinySong1226@gmail.com"
        js-doc-author (format "RongXiang Song <%s>" js-doc-mail-address)
        js-doc-url "http://www.TinySong.com"
        js-doc-license "MIT"))


;; https://ebzzry.github.io/emacs-pairs.html
(defun tinysong/post-init-smartparens ()
  (progn
    (defun wrap-sexp-with-new-round-parens ()
      (interactive)
      (insert "()")
      (backward-char)
      (sp-forward-slurp-sexp))

    (global-set-key (kbd "C-(") 'wrap-sexp-with-new-round-parens)
    (with-eval-after-load 'smartparens
      (evil-define-key 'normal sp-keymap
        (kbd ")>") 'sp-forward-slurp-sexp
        (kbd ")<") 'sp-forward-barf-sexp
        (kbd "(>") 'sp-backward-barf-sexp
        (kbd "(<") 'sp-backward-slurp-sexp))

    ))

(setq-default tab-width 4)


;; https://www.emacswiki.org/emacs/ERC
;; TODO: erc linux notation

;; (defun do-notify (nickname message)
;;   (let* ((channel (buffer-name))
;;          ;; using https://github.com/leathekd/erc-hl-nicks
;;          (nick (erc-hl-nicks-trim-irc-nick nickname))
;;          (title (if (string-match-p (concat "^" nickname) channel)
;;                     nick
;;                   (concat nick " (" channel ")")))
;;          ;; Using https://github.com/magnars/s.el
;;          (msg (s-trim (s-collapse-whitespace message))))
;;     ;; call the system notifier here
;;     ))

(defun tinysong/post-init-erc ()
  (progn
    (defun my-erc-hook (match-type nick message)
      "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
      (unless (posix-string-match "^\\** *Users on #" message)
        (tinysong/growl-notification
         (concat "ERC: : " (buffer-name (current-buffer)))
         message
         t
         )))
    (message "notation")
    (add-hook 'erc-text-matched-hook 'my-erc-hook)
    ;; (spaceline-toggle-erc-track-off)
    ))


;; https://www.ibm.com/developerworks/cn/aix/library/au-learningdoxygen/
;; http://emacser.com/doxymacs.htm
(defun tinysong/init-doxymacs ()
  "Initialize doxymacs, create document from souce-code"
  (use-package doxymacs
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    :config
    (progn
      (defun my-doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
            (doxymacs-font-lock)))
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode))))


;; fold functions
(defun tinysong/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :init
    (vimish-fold-global-mode 1)))

;; http://endlessparentheses.com/beacon-never-lose-your-cursor-again.html
(defun tinysong/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")
      ;; (setq beacon-push-mark 60)
      ;; (setq beacon-color "#666600")
      ;; (setq beacon-color "#990099")
      (setq beacon-color "#FF9900")
      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)))

;; http://blog.binchen.org/posts/how-to-use-ctags-in-emacs-effectively-3.html
(defun tinysong/init-ctags-update ()
  (use-package ctags-update
    :init
    (progn
      (add-hook 'c++-mode-hook 'turn-on-ctags-auto-update-mode)
      (add-hook 'c-mode-hook 'turn-on-ctags-auto-update-mode)
      (define-key evil-normal-state-map (kbd "gf")
        (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

      (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

      (define-key evil-normal-state-map (kbd "gn")
        (lambda () (interactive) (find-tag last-tag t)))
      )
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))


;; https://github.com/bbatsov/projectile
(defun tinysong/post-init-projectile ()
  (with-eval-after-load 'projectile
    (progn
      (setq projectile-completion-system 'ivy)
      (projectile-global-mode)
      (setq projectile-indexing-method 'native)
      ;; to disable remote file exists cache that use this snippet of code
      (setq projectile-file-exists-remote-cache-expire nil)
      ;; to change the remote file exists ache expire to 30 second use the snippet of code
      (setq projectile-file-exists-remote-cache-expire (* 1 20))
      (add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
      (add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
      (spacemacs/set-leader-keys "pf" 'tinysong/open-file-with-projectile-or-lsgit) ;;
      )))


(defun tinysong/post-init-hl-anything ()
  (progn
    (hl-highlight-mode -1)
    (spacemacs|add-toggle toggle-hl-anything
      :status hl-highlight-mode
      :on (hl-highlight-mode)
      :off (hl-highlight-mode -1)
      :documentation "Toggle highlight anything mode."
      :evil-leader "ths")))

;; https://github.com/benma/visual-regexp.el
(defun tinysong/init-visual-regexp ()
  (use-package visual-regexp
    :init))

(defun tinysong/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :init))

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles




(defun tinysong/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

;; https://github.com/syohex/emacs-git-messenger
(defun tinysong/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (defun song/github-browse-commit ()
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


(defun tinysong/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      ;; (setq flycheck-display-errors-function 'flycheck-display-error-messages)
      (setq flycheck-display-errors-delay 0.1)
      ;; (remove-hook 'c-mode-hook 'flycheck-mode)
      ;; (remove-hook 'c++-mode-hook 'flycheck-mode)
      ;; (evilify flycheck-error-list-mode flycheck-error-list-mode-map)
      )))

;; cmake https://www.ibm.com/developerworks/cn/linux/l-cn-cmake/
(defun tinysong/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
                                       "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
      "hd" 'cmake-help)
    (defun cmake-rename-buffer ()
      "Renames a CMakeLists.txt buffer to cmake-<directory name>."
      (interactive)
      (when (and (buffer-file-name)
                 (string-match "CMakeLists.txt" (buffer-name)))
        (setq parent-dir (file-name-nondirectory
                          (directory-file-name
                           (file-name-directory (buffer-file-name)))))
        (setq new-buffer-name (concat "cmake-" parent-dir))
        (rename-buffer new-buffer-name t)))

    (add-hook 'cmake-mode-hook (function cmake-rename-buffer))))

(defun tinysong/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

;; http://blog.csdn.net/csfreebird/article/details/9250989
(defun tinysong/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))


(defun tinysong/init-nodejs-repl ()
  (use-package nodejs-repl
    :init
    :defer t))


;; ;; https://github.com/capitaomorte/yasnippet
;; (defun tinysong/post-init-yasnippet ()
;;   (progn
;;     (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
;;     (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
;;                                                                        org-mode-hook
;;                                                                        markdown-mode-hook))

;;     (defun tinysong/load-yasnippet ()
;;       (unless yas-global-mode
;;         (progn
;;           ;; (yas-global-mode 1)
;;           (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
;;           (setq yas-snippet-dirs  my-snippet-dir)
;;           (yas-load-directory my-snippet-dir)
;;           (setq yas-wrap-around-region t)))
;;       (yas-minor-mode 1))

;;     (spacemacs/add-to-hooks 'tinysong/load-yasnippet '(prog-mode-hook
;;                                                        markdown-mode-hook
;;                                                        org-mode-hook))
;;     ))

(defun tinysong/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)))

;; TODO : SPC o
(defun tinysong/init-occur-mode ()
  (defun occur-dwim ()
    "Call `occur' with a sane default."
    (interactive)
    (push (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            (let ((sym (thing-at-point 'symbol)))
              (when (stringp sym)
                (regexp-quote sym))))
          regexp-history)
    (call-interactively 'occur))
  (bind-key* "M-s o" 'occur-dwim)
  (evilified-state-evilify occur-mode occur-mode-map
    "RET" 'occur-mode-goto-occurrence))

(defun tinysong/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode)
      )
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))

(defun tinysong/post-init-youdao-dictionary ()
  ;; ((interactive "P"))
  (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+))

(defun tinysong/post-init-persp-mode ()
  (when (fboundp 'spacemacs|define-custom-layout)
    (spacemacs|define-custom-layout "@Kernel"
      :binding "k"
      :body
      (find-file "~/kernel-2.6.11.12/Makefile")
      ;; (split-window-right)
      ;; (find-file "~/cocos2d-x/cocos/cocos2d.cpp")
      )))
(defun tinysong/init-unicad ()
  (use-package unicad
    :init))

;; (defun tinysong/init-header2
;;     (interactive "p")
;;   (use-package header2
;;     :init
;;     (autoload 'auto-make-header "header2")
;;     (add-hook 'write-file-hooks 'auto-update-file-header)
;;     (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;;     (add-hook 'c-mode-common-hook 'auto-make-header)
;;     (add-hook 'tex-mode-hook 'auto-make-header)
;;     ))

(defun tinysong/post-init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :init
    (progn
      ;; If you use other VCS (subversion, for example), enable the following option
      ;;(setq ffip-project-file ".svn")
      ;; in MacOS X, the search file command is CMD+p
      (bind-key* "s-p" 'find-file-in-project)
      (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
      ;; for this project, I'm only interested certain types of files
      ;; (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
      ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
      ;; OR if I'm reading my personal issue track document,
      )))

(defun tinysong/post-init-deft ()
  (progn
    (setq deft-use-filter-string-for-filename t)
    (spacemacs/set-leader-keys-for-major-mode 'deft-mode "q" 'quit-window)
    (setq deft-extension "org")
    (setq deft-directory "~/org-notes")))

(defun tinysong/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

    (with-eval-after-load 'markdown-mode
      (progn
        (when (configuration-layer/package-usedp 'company)
          (spacemacs|add-company-hook markdown-mode))

        (defun zilongshanren/markdown-to-html ()
          (interactive)
          (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name))
          (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

        (spacemacs/set-leader-keys-for-major-mode 'gfm-mode-map
          "p" 'zilongshanren/markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "p" 'zilongshanren/markdown-to-html)

        (evil-define-key 'normal markdown-mode-map (kbd "TAB") 'markdown-cycle)
        ))
    ))
