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
    evil
    ;; document create
    (doxymacs :location local)
    evil-vimish-fold
    beacon
    hl-anything
    keyfreq
    ;; visual-regexp-steroids and visual-regexp are reg serarch, steroids is an externsion to visual-regexp
    visual-regexp
    visual-regexp-steroids
    flycheck
    ;; post extension names go here
    ;; nodejs-repl-eval don't support es6 and js2-mode also don't support it
    ;; so I use js-comit instead.
    nodejs-repl
    wrap-region
    youdao-dictionary
    deft
    swiper
    command-log
    ;; hydra
    elfeed
    elfeed-org
    osx-dictionary
    org-mac-link
    ;; (auto-rsync :location local)
    multiple-cursors
    graphviz-dot-mode
    persp-mode
    fasd
    bookmark
    ))


;;; packages.el ends here

;; TODO: https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html

;; package init

(defun tinysong/post-init-js-doc ()
  (setq js-doc-mail-address "TinySong1226@gmail.com"
        js-doc-author (format "RongXiang Song <%s>" js-doc-mail-address)
        js-doc-url "http://www.tinysong.com"
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
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))
    ))



(defun tinysong/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

(defun tinysong/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      ;; (setq flycheck-display-errors-function 'flycheck-display-error-messages)
      (setq flycheck-display-errors-delay 0.1)
      ;; (remove-hook 'c-mode-hook 'flycheck-mode)
      ;; (remove-hook 'c++-mode-hook 'flycheck-mode)
      ;; (evilify flycheck-error-list-mode flycheck-error-list-mode-map)
      )))



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
  (spacemacs/declare-prefix "od" "directory")
  (spacemacs/set-leader-keys "ody" 'youdao-dictionary-search-at-point+))

(defun tinysong/post-init-persp-mode ()
  (when (fboundp 'spacemacs|define-custom-layout)
    (progn
      (spacemacs|define-custom-layout "@Kernel"
        :binding "k"
        :body
        ;; (split-window-right)
        (find-file "~/development/kernel-2.6.11.12/Makefile"))
      (spacemacs|define-custom-layout "@Docker"
        :binding "d"
        :body
        ;; (split-window-right)
        (find-file "~/development/golang/src/github.com/docker/docker/cmd/dockerd/daemon.go"))
      )
    ))
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
          (spacemacs|add-company-backends markdown-mode)
          )

        (defun tinysong/markdown-to-html ()
          (interactive)
          (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name))
          (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

        (spacemacs/set-leader-keys-for-major-mode 'gfm-mode-map
          "p" 'tinysong/markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "p" 'tinysong/markdown-to-html)

        (evil-define-key 'normal markdown-mode-map (kbd "TAB") 'markdown-cycle)
        ))
    ))

(defun tinysong/post-init-swiper ()
  "Initialize my package"
  (progn
    (defun my-swiper-search (p)
      (interactive "P")
      (let ((current-prefix-arg nil))
        (call-interactively
         (if p #'spacemacs/swiper-region-or-symbol
           #'swiper))))

    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)

    (use-package recentf
      :config
      (setq recentf-exclude
            '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
              ".*png$"))
      (setq recentf-max-saved-items 60))
    (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map)

    (use-package ivy
      :defer t
      :config
      (progn
        (spacemacs|hide-lighter ivy-mode)
        (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
        (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-dispatching-done)
        (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
        (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

    (define-key global-map (kbd "C-s") 'my-swiper-search)
    ))

(defun tinysong/post-init-command-log ()
  (with-eval-after-load 'command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun tinysong/init-hydra ()
  (use-package hydra
    :init
    (progn
      ;; major mode hydra is really cool, don't need to switch mode anymore
      ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
      ;; If the command will change the buffer, they should be put in these groups.
      ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      ;; (bin-key* "<f3>" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (bind-key*  "<f4>" 'hydra-apropos/body)
      )))

;; https://github.com/skeeto/elfeed
;; http://nullprogram.com/blog/2013/09/04/
(defun tinysong/init-elfeed ()
  (use-package elfeed
    :init
    (global-set-key (kbd "C-x w") 'elfeed)
    (spacemacs/set-leader-keys "of" 'elfeed)
    :defer t
    :config
    (progn
      (setq elfeed-feeds
            '(
              ("http://thenewstack.io/blog/feed/" docker)
              "http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              ("http://feeds.feedburner.com/LostInTheTriangles" emacs)
              "http://planet.emacsen.org/atom.xml"
              ("http://feeds.feedburner.com/emacsblog" emacs)
              "http://blog.binchen.org/rss.xml"
              "http://www.masteringemacs.org/feed/"
              "http://coolshell.cn/feed"
              "http://emacsist.com/rss"
              "http://oremacs.com/atom.xml"
              "http://www.masteringemacs.org/feed/"
              ("http://www.infoq.com/cn/feed/architecture-design" infoq)
              ("http://www.infoq.com/cn/feed/AI" infoq)
              ("http://www.infoq.com/cn/feed/ronqi" infoq)
              ("http://www.infoq.com/cn/feed/html-5/minibooks" infoq)
              ("http://www.infoq.com/cn/feed/culture-methods" culture-method)
              ))

      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      ;;insert space before elfeed filter
      (defun elfeed-search-live-filter-space ()
        "Insert space when running elfeed filter"
        (interactive)
        (let ((elfeed-search-filter (concat elfeed-search-filter " ")))
          (elfeed-search-live-filter)))


      (defun elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)
      (define-key elfeed-search-mode-map (kbd "s") 'elfeed-search-live-filter-space)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

(defun tinysong/init-elfeed-org ()
  (use-package elfeed-org
    :ensure t
    :config
    (setq rmh-elfeed-org-files (list "~/org-notes/elfeed.org"))
    ))


(defun tinysong/post-init-osx-dictionary ()
  (use-package osx-dictionary
    :config
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      (spacemacs/set-leader-keys "odr" 'osx-dictionary-search-pointer)
      )))

(defun tinysong/init-org-mac-link ()
  (use-package org-mac-link
    :init
    (add-hook 'org-mode-hook
              (lambda ()
                (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))))

;; (def tinysong/int-auto-rsync ()
;;      (use-package auto-rsync
;;        :init
;;        :config
;;        (progn
;;          (auto-rsync-mode t)
;;          (setq auto-rsync-dir-alist
;;                (("/path/to/src1/" . "/path/to/dest1/")
;;                 ("/path/to/src2/" . "username@hostname:/path/to/dest2/")))
;;          )
;;        ))

(defun tinysong/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (spacemacs/declare-prefix "om" "mc")
      (spacemacs/set-leader-keys "oml" 'mc/edit-lines)
      (spacemacs/set-leader-keys "omb" 'mc/edit-beginnings-of-lines)
      (spacemacs/set-leader-keys "ome" 'mc/edit-ends-of-lines)

      (spacemacs/set-leader-keys "oma" 'mc/mark-all-like-this)
      (spacemacs/set-leader-keys "omA" 'mc/mark-all-dwim)

      (spacemacs/set-leader-keys "omj" 'mc/mark-next-like-this)
      (spacemacs/set-leader-keys "omJ" 'mc/unmark-next-like-this)
      (spacemacs/set-leader-keys "omk" 'mc/mark-previous-like-this)
      (spacemacs/set-leader-keys "omK" 'mc/unmark-previous-like-this)

      (spacemacs/set-leader-keys "omi" 'mc/insert-numbers)
      (spacemacs/set-leader-keys "omh" 'mc-hide-unmatched-lines-mode)
      (spacemacs/set-leader-keys "omd" 'mc/mark-all-symbols-like-this-in-defun)
      (spacemacs/set-leader-keys "omr" 'mc/reverse-regions)
      (spacemacs/set-leader-keys "oms" 'mc/sort-regions)

      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))
    :config
    (setq mc/cmds-to-run-for-all
          '(
            electric-newline-and-maybe-indent
            hungry-delete-backward
            spacemacs/backward-kill-word-or-region
            spacemacs/smart-move-beginning-of-line
            lispy-move-beginning-of-line
            lispy-move-end-of-line
            evil-exit-visual-state
            evil-backward-char
            evil-delete-char
            evil-escape-emacs-state
            evil-escape-insert-state
            evil-exit-emacs-state
            evil-previous-visual-line
            evil-next-visual-line
            evil-forward-char
            evil-insert
            evil-next-line
            evil-normal-state
            evil-previous-line
            evil-append
            evil-append-line
            forward-sentence
            kill-sentence
            org-self-insert-command
            sp-backward-delete-char
            sp-delete-char
            sp-remove-active-pair-overlay))))


(defun tinysong/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    ;; (evil-move-cursor-back nil)
    (push "TAGS" spacemacs-useless-buffers-regexp)

    ;; ;; change evil initial mode state
    (loop for (mode . state) in
          '((shell-mode . normal))
          do (evil-set-initial-state mode state))

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    (define-key evil-normal-state-map
      (kbd "Y") 'tinysong/yank-to-end-of-line)

    ;; rebind g,k to gj and gk
    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))


    (define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)

    ;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
    ;; (define-key evil-insert-state-map "\C-n" 'next-line)
    ;; (define-key evil-insert-state-map "\C-k" 'kill-line)
    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)

    (spacemacs/set-leader-keys "bi" 'ibuffer)
    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
    (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
    ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
    (define-key evil-visual-state-map (kbd "C-r") 'tinysong/evil-quick-replace)

    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

    ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
    ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
    ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
    ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

    ;; for emacs shell mode
    ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
    ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
    (evil-define-key 'emacs term-raw-map (kbd "C-w")
      'evil-delete-backward-word)

    (spacemacs/set-leader-keys "fR" 'tinysong/rename-file-and-buffer)

    ;; This will break visual column edit
    ;; enable hybrid editing style
    ;; (defadvice evil-insert-state (around zilongshanren/holy-mode activate)
    ;;   "Preparing the holy water flasks."
    ;;   (evil-emacs-state))
    ;; disable c-[ temporally
    ;; (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
    ;; (bind-keys ("<C-[>" . evil-normal-state))
    ;; (setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
    ;; (define-key evil-emacs-state-map [escape] 'evil-normal-state)
    ))

(defun tinysong/post-init-graphviz-dot-mode ()
  (with-eval-after-load 'graphviz-dot-mode
    (require 'company-keywords)
    (progn
      (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph" "label" "edge" "bgcolor" "style" "record" "rankdir") company-keywords-alist)
      (when (configuration-layer/layer-usedp 'company)
        (spacemacs|add-company-backends graphviz-dot-mode))
      )
    )
  )

(defun tinysong/init-fasd ()
  (use-package fasd
    :init
    :config
    ))


(defun tinysong/post-init-bookmark ()
  (progn
    (spacemacs/declare-prefix "ob" "bookmark")
    (spacemacs/set-leader-keys "obs" 'bookmark-set)
    (spacemacs/set-leader-keys "obj" 'bookmark-jump)
    (spacemacs/set-leader-keys "obr" 'bookmark-rename)
    (spacemacs/set-leader-keys "obd" 'bookmark-delete)
    )
  )
