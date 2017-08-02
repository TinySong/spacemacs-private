;;; packages.el --- TinySong layer packages file for Spacemacs. ;; -*- lexical-binding: t -*-
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
    ;; hl-anything
    js2-mode
    markdown-mode
    ;; evil
    (doxymacs :location local)
    evil-vimish-fold
    beacon
    ;; keyfreq
    ;; visual-regexp-steroids and visual-regexp are reg serarch, steroids is an externsion to visual-regexp
    visual-regexp
    visual-regexp-steroids
    ;; post extension names go here
    ;; nodejs-repl-eval don't support es6 and js2-mode also don't support it
    ;; so I use js-comit instead.
    nodejs-repl
    (nodejs-repl-eval :location local)
    wrap-region
    youdao-dictionary
    ;; deft
    swiper
    command-log
    ;; hydra
    osx-dictionary
    org-mac-link
    ;; multiple-cursors // with evil-mc instead of
    graphviz-dot-mode
    persp-mode
    bookmark
    prodigy
    company
    which-func
    header2
    smartparens
    all-the-icons
    all-the-icons-dired
    ;; spaceline-all-the-icons
    all-the-icons-ivy
    ))


;;; packages.el ends here

;; TODO: https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html

;; package init

;; (defun tinysong/post-init-js-doc ()
;;   (setq js-doc-mail-address "TinySong1226@gmail.com"
;;         js-doc-author (format "RongXiang Song <%s>" js-doc-mail-address)
;;         js-doc-url "http://www.tinysong.com"
;;         js-doc-license "MIT"))


;; ;; https://ebzzry.github.io/emacs-pairs.html
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

;; (defun tinysong/post-init-erc ()
;;   (progn
;;     (defun my-erc-hook (match-type nick message)
;;       "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
;;       (unless (posix-string-match "^\\** *Users on #" message)
;;         (tinysong/growl-notification
;;          (concat "ERC: : " (buffer-name (current-buffer)))
;;          message
;;          t
;;          )))
;;     (message "notation")
;;     (add-hook 'erc-text-matched-hook 'my-erc-hook)
;;     ;; (spaceline-toggle-erc-track-off)
;;     ))

;; (defun tinysong/post-init-hl-anything ()
;;   "Highlight Symbols, Selections, Enclosing Parens and More."
;;   (progn
;;     (hl-highlight-mode -1)
;;     (spacemacs|add-toggle toggle-hl-anything
;;       :status hl-highlight-mode
;;       :on (hl-highlight-mode)
;;       :off (hl-highlight-mode -1)
;;       :documentation "Toggle highlight anything mode."
;;       :evil-leader "ths")))

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


;; func fold functions
(defun tinysong/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :defer t
    :init
    (vimish-fold-global-mode 1)))

;; http://endlessparentheses.com/beacon-never-lose-your-cursor-again.html
(defun tinysong/init-beacon ()
  (use-package beacon
    :defer t
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")
      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)
    (setq beacon-size 80)
    (setq beacon-blink-duration 0.5)
    (setq beacon-color "#FF9900")
    ))


;; https://github.com/benma/visual-regexp.el
(defun tinysong/init-visual-regexp ()
  (use-package visual-regexp
    :init
    :defer t))

(defun tinysong/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace)
      (define-key global-map (kbd "C-c m") 'vr/mc-mark))
    ))


(defun tinysong/init-keyfreq ()
  (use-package keyfreq
    :defer t
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))


(defun tinysong/init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t
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
    :defer t
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (go-mode javascript-mode css-mode js2-mode))
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
  (spacemacs/set-leader-keys "ody" 'youdao-dictionary-search-at-point+)
  )

(defun tinysong/post-init-persp-mode ()
  (when (fboundp 'spacemacs|define-custom-layout)
    (progn
      (spacemacs|define-custom-layout "@terminal"
        :binding "d"
        :body
        (terminal))
      )
    ))

(defun tinysong/init-header2 ()
  ;; (interactive "p")
  (use-package header2
    :defer t
    :init
    :config
    (autoload 'auto-make-header "header2")
    (add-hook 'write-file-hooks 'auto-update-file-header)
    (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
    (add-hook 'c-mode-common-hook 'auto-make-header)
    (add-hook 'go-mode-hook 'auto-make-header)
    (add-hook 'tex-mode-hook 'auto-make-header)
    ))

(defun tinysong/post-init-deft ()
  (progn
    (setq deft-use-filter-string-for-filename t)
    (spacemacs/set-leader-keys-for-major-mode 'deft-mode "q" 'quit-window)
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
      :defer t
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
  (with-eval-after-load 'hydra
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
      )
    ))

;; https://github.com/skeeto/elfeed
;; http://nullprogram.com/blog/2013/09/04/

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
    :defer t
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
    :defer t
    :init
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
            sp-remove-active-pair-overlay))
    (progn
      ;; (spacemacs/declare-prefix "om" "mc")
      (spacemacs/set-leader-keys "dl" 'mc/edit-lines)
      (spacemacs/set-leader-keys "db" 'mc/edit-beginnings-of-lines)
      (spacemacs/set-leader-keys "de" 'mc/edit-ends-of-lines)

      (spacemacs/set-leader-keys "da" 'mc/mark-all-like-this)
      (spacemacs/set-leader-keys "dA" 'mc/mark-all-dwim)

      (spacemacs/set-leader-keys "dj" 'mc/mark-next-like-this)
      (spacemacs/set-leader-keys "dJ" 'mc/unmark-next-like-this)
      (spacemacs/set-leader-keys "dk" 'mc/mark-previous-like-this)
      (spacemacs/set-leader-keys "dK" 'mc/unmark-previous-like-this)

      (spacemacs/set-leader-keys "di" 'mc/insert-numbers)
      (spacemacs/set-leader-keys "dh" 'mc-hide-unmatched-lines-mode)
      (spacemacs/set-leader-keys "dd" 'mc/mark-all-symbols-like-this-in-defun)
      (spacemacs/set-leader-keys "dr" 'mc/reverse-regions)
      (spacemacs/set-leader-keys "ds" 'mc/sort-regions)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
      ;; From active region to multiple cursors:
      ;; (global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
      ;; (global-set-key (kbd "C-c c c") 'mc/edit-lines)
      ;; (global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
      ;; (global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))
    ))


(defun tinysong/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    ;; (evil-move-cursor-back nil)
    (push "TAGS" spacemacs-useless-buffers-regexp)

    ;; change evil initial mode state
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
    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    ;; (define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
    ;; (define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)
    
    (define-key evil-emacs-state-map (kbd "\s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "C-f") 'forward-word)
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
  (use-package fasdq
    :defer t
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

(defun tinysong/init-which-func ()
  ;; Show the current function name in the header line
  (which-function-mode)
  (setq which-func-unknown "n/a")
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))
  )

;; http://rejeep.github.io/emacs/2013/12/14/prodigy-emacs-service-manager.html
(defun tinysong/post-init-prodigy ()
  (progn
    (prodigy-define-tag
      :name 'jekyll
      :env '(("LANG" "en_US.UTF-8")
             ("LC_ALL" "en_US.UTF-8")))

    (prodigy-define-service
      :name "Hexo Server"
      :command "hexo"
      :args '("server")
      :cwd "~/4gamers.cn"
      :tags '(hexo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Deploy"
      :command "hexo"
      :args '("deploy" "--generate")
      :cwd "~/4gamers.cn"
      :tags '(hexo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)


    (prodigy-define-service
      :name "Org wiki preview"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "8088")
      :cwd "~/org-notes/public_html"
      :tags '(org-mode)
      :init (lambda () (browse-url "http://localhost:8088"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)))

(defun tinysong/post-init-js2-mode ()
  (progn
    (setq company-backends-js2-mode '((company-dabbrev-code
                                       company-keywords
                                       company-etags) company-files company-dabbrev))

    (tinysong|toggle-company-backends company-tern)


    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "tb" 'tinysong/company-toggle-company-tern)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)

    (spacemacs/set-leader-keys-for-major-mode 'web-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)
    (eval-after-load 'js2-mode
      '(progn
         (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
         (define-key js2-mode-map   (kbd "s-.") 'company-tern)))))
(defun tinysong/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major))))

;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun tinysong/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :commands (nodejs-repl-eval-buffer nodejs-repl-eval-dwim nodejs-repl-eval-function)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode
        "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "sb" 'nodejs-repl-eval-buffer
        "sf" 'nodejs-repl-eval-function
        "sd" 'nodejs-repl-eval-dwim))
    :defer t
    ))

(defun tinysong/post-init-company ()
  (progn
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.08)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes shell-script-mode makefile-bsdmake-mode sh-mode lua-mode nxml-mode conf-unix-mode json-mode graphviz-dot-mode))
    ))

(defun tinysong/init-evil-multiedit ()
  (use-package evil-multiedit
    :defer t
    :init
    :config
    ))

;; TODO fix load time
(defun tinysong/init-spaceline-all-the-icons ()
  (use-package spaceline-all-the-icons
    :init
    :defer t
    :after spaceline
    :config
    (spaceline-all-the-icons-theme)
    (spaceline-all-the-icons--setup-anzu)            ;; Enable anzu searching
    (spaceline-all-the-icons--setup-package-updates) ;; Enable package update indicator
    (spaceline-all-the-icons--setup-git-ahead)       ;; Enable # of commits ahead of upstream in git
    (spaceline-all-the-icons--setup-paradox)         ;; Enable Paradox mode line
    (spaceline-all-the-icons--setup-neotree)         ;; Enable Neotree mode linez
    ))

(defun tinysong/init-prettify-utils ()
  (use-package prettify-utils))

(defun tinysong/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :init
    :after all-the-icons
    :config
    (progn
      (all-the-icons-ivy-setup)
      ;; (advice-add 'all-the-icons-ivy-file-transformer
      ;;             :override 'ivy-file-transformer-fixed-for-files)
      )))

(defun tinysong/init-all-the-icons ()
  (use-package all-the-icons
    :init
    :defer t
    :ensure t
    :config
    ;; (progn
    ;;   (add-to-list
    ;;    'all-the-icons-icon-alist
    ;;    '("\\.hy$" all-the-icons-fileicon "lisp" :face all-the-icons-orange))
    ;;   (add-to-list
    ;;    'all-the-icons-mode-icon-alist
    ;;    '(hy-mode all-the-icons-fileicon "lisp" :face all-the-icons-orange)))
    (all-the-icons-octicon "file-binary") ;; GitHub Octicon for Binary File
    (all-the-icons-faicon  "cogs")        ;; FontAwesome icon for cogs
    (all-the-icons-wicon   "tornado")     ;; Weather Icon for tornado
    ))

(defun tinysong/init-pretty-magit ()
  (use-package pretty-magit
    :config
    (progn
      (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.2))
      (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.2))
      (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.2))
      (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.2))
      (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.2))
      (pretty-magit "master"  ? (:box t :height 1.2) t)
      (pretty-magit "origin"  ? (:box t :height 1.2) t))))

(defun tinysong/init-pretty-fonts ()
  (use-package pretty-fonts
    :init
    (progn
      (defconst pretty-fonts-hy-mode
        '(("\\(self\\)"   ?⊙))))

    :config
    (progn
      (pretty-fonts-set-kwds
       '(;; Fira Code Ligatures
         (pretty-fonts-fira-font prog-mode-hook org-mode-hook)
         ;; Custom replacements not possible with `pretty-code' package
         (pretty-fonts-hy-mode hy-mode-hook)))

      (pretty-fonts-set-fontsets
       '(("fontawesome"
          ;;                         
          #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

         ("all-the-icons"
          ;;    
          #xe907 #xe928)

         ("github-octicons"
          ;;                          
          #xf091 #xf059 #xf076 #xf075 #xe192  #xf016)

         ("material icons"
          ;;        
          #xe871 #xe918 #xe3e7
          ;;
          #xe3d0 #xe3d1 #xe3d2 #xe3d4)

         ("Symbola"
          ;; 𝕊    ⨂      ∅      ⟻    ⟼     ⊙      𝕋       𝔽
          #x1d54a #x2a02 #x2205 #x27fb #x27fc #x2299 #x1d54b #x1d53d
          ;; 𝔹    𝔇       𝔗
          #x1d539 #x1d507 #x1d517))))))

(defun tinysong/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :init
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    :defer t
    :ensure t
    )
  )
