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

(bind-key* "C-c l" 'tinysong/insert-chrome-current-tab-url)
(spacemacs/set-leader-keys "oil" 'tinysong/insert-chrome-current-tab-url)
(spacemacs/set-leader-keys "oit" 'tinysong/insert-chrome-current-tab-title)

(global-set-key (kbd "<f5>") 'tinysong/run-current-file)

(global-set-key (kbd "s-/") 'hippie-expand)

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)


(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)

(define-key 'help-command (kbd "C-i") 'info-display-manual)

(global-set-key [(shift return)] 'smart-open-line)

(define-key global-map (kbd "<f1>") 'tinysong/hotspots)

;; (global-set-key (kbd "C-.") 'company-capf)


;; some easy functions for navigate functions
;;C-M-a beginning-of-defun
;;C-M-e end-of-defun
;;C-M-h mark-defun
(global-set-key (kbd "C-s-h") 'mark-defun)

(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "C-`") 'toggle-input-method)

;; "http://endlessparentheses.com/transposing-keybinds-in-emacs.html?source=rss"
;; (global-set-key "\C-t" #'transpose-lines)
;; (define-key ctl-x-map "\C-t" #'transpose-chars)

(when (spacemacs/system-is-mac)
  (spacemacs/set-leader-keys "o!" 'tinysong/iterm-shell-command))

(spacemacs|add-toggle toggle-shadowsocks-proxy-mode
  :status shadowsocks-proxy-mode
  :on (global-shadowsocks-proxy-mode)
  :off (global-shadowsocks-proxy-mode -1)
  :documentation "Toggle shadowsocks proxy mode."
  :evil-leader "ots")

(global-set-key (kbd "s-s") 'save-buffer)
(bind-key* "s-k" 'scroll-other-window-down)
(bind-key* "s-j"  'scroll-other-window)
(bind-key* "C-c /" 'company-files)

(bind-key* "s-r" 'tinysong/browser-refresh--chrome-applescript)

(bind-key* "s-;" 'tinysong/insert-semicolon-at-the-end-of-this-line)
(bind-key* "C-s-;" 'tinysong/delete-semicolon-at-the-end-of-this-line)

(bind-key* "s-," 'tinysong/insert-comma-at-the-end-of-this-line)
(bind-key* "C-s-," 'tinysong/delete-comma-at-the-end-of-this-line)

(bind-key* "C-=" 'er/expand-region)


(bind-key* "M--" 'tinysong/goto-match-paren)

(spacemacs/set-leader-keys "ol" 'tinysong/load-my-layout)

(global-set-key (kbd "s-<return>") 'goto-address-at-point)

(spacemacs/set-leader-keys "or" 'tinysong/dired-rsync)
