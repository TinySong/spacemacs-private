;;; packages.el --- ts-lang layer packages file for Spacemacs. ;; -*- lexical-binding: t -*-
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
;; added to `ts-lang-packages'. Then, for each package PACKAGE:
;;

;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ts-lang/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ts-lang/pre-init-PACKAGE' and/or
;;   `ts-lang/post-init-PACKAGE' to customize the package as it is loaded.
(defconst ts-lang-packages
  '(
    lispy
    go
    ))


(defun ts-lang/init-lispy ()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn
      (add-hook 'lispy-mode-hook 'spacemacs/toggle-aggressive-indent-on)
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
      )
    (with-eval-after-load 'lispy
      (progn
        (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
        (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline)

        ))

    ))

(defun ts-lang/post-init-go ()
  (with-eval-after-load 'go-mode
    (add-hook 'go-mode (lambda () (smartparens-strict-mode 1))))
  )
