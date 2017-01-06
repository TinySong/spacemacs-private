;;; packages.el --- ts-project layer packages file for Spacemacs. ;; -*- lexical-binding: t -*-
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
;; added to `ts-project-packages'. Then, for each package PACKAGE:
;;

;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ts-project/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ts-project/pre-init-PACKAGE' and/or
;;   `ts-project/post-init-PACKAGE' to customize the package as it is loaded.
(defconst ts-project-packages
  '(
    ctags-update
    cmake-font-lock
    cmake-mode
    find-file-in-project
    projectile
    ivy
    ;; TODO: https://www.emacswiki.org/emacs/FindFileInProject
    ;; post extension names go here
    ))


;; http://blog.binchen.org/posts/how-to-use-ctags-in-emacs-effectively-3.html
(defun ts-project/init-ctags-update ()
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
(defun ts-project/post-init-projectile ()
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
      ;; (spacemacs/set-leader-keys "pf" 'ts-project/open-file-with-projectile-or-lsgit)
      )))

(defun ts-project/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

;; cmake https://www.ibm.com/developerworks/cn/linux/l-cn-cmake/
(defun ts-project/post-init-cmake-mode ()
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

(defun ts-project/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :init
    (progn
      ;; If you use other VCS (subversion, for example), enable the following option
      ;;(setq ffip-project-file ".svn")
      ;; in MacOS X, the search file command is CMD+p
      ;; (bind-key* "s-p" 'find-file-in-project)
      (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
      ;; for this project, I'm only interested certain types of files
      ;; (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
      ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
      ;; OR if I'm reading my personal issue track document,
      )))

(defun ts-project/post-init-ivy ()
  (use-package ivy
    :init
    :config
    (progn
      (bind-key* "s-p" 'counsel-git))))
