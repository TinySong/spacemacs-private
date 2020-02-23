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
    beacon
    visual-regexp
    visual-regexp-steroids
    youdao-dictionary
    osx-dictionary
    persp-mode
    bookmark
    which-func
    ))


;;; packages.el ends here


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

;; https://github.com/benma/visual-regexp-steroids.el
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



(defun tinysong/post-init-osx-dictionary ()
  (use-package osx-dictionary
    :config
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      (spacemacs/set-leader-keys "odr" 'osx-dictionary-search-pointer)
      )))


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



(defun tinysong/init-evil-multiedit ()
  (use-package evil-multiedit
    :defer t
    :init
    :config
    ))



