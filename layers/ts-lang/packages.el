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
    (cc-mode :location built-in)
    company-c-headers
    lispy
    lua-mode
    irony
    ))


(defun ts-lang/post-init-cc-mode ()
  (progn

    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                       ; no additional indent
              ad-do-it)))               ; default behavior


    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (with-eval-after-load 'c++-mode
      (define-key c++-mode-map (kbd "s-.") 'company-ycmd)))
  ;; company backend should be grouped
  )

;; can add c-header list
(defun ts-lang/post-init-company-c-headers ()
  (progn
    (setq company-c-headers-path-system
          (quote
           ("/usr/include/" "/usr/local/include/")))
    (setq company-c-headers-path-user
          (quote
           (".")))))

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

(defun ts-lang/post-init-lua-mode ()
  (progn
    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :backends company-dabbrev :modes lua-mode)
      (spacemacs|add-company-backends :backends company-etags :modes lua-mode)
      ;; (push 'company-dabbrev company-backends-lua-mode)
      ;; (push 'company-etags company-backends-lua-mode)
      )
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    (add-hook 'lua-mode-hook 'smartparens-mode)
    (setq lua-indent-level 4)

    (spacemacs/set-leader-keys-for-major-mode 'lua-mode
      "<tab>" 'hs-toggle-hiding
      "gg" 'helm-gtags-dwim
      "gr" 'helm-gtags-find-rtag
      "gs" 'helm-gtags-find-symbol
      "gf" 'helm-gtags-find-files)))

(defun ts-lang/init-irony ()
  (use-package irony
    :defer t
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  )
