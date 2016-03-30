;;; packages.el --- TinySong layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: song <song@GE60-SHADOW-Walker.lan>
;; URL: https://github.com/TinySong/spacemacs
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
  '(
    ;; (hackernews :location build-in)
    js-doc
    smartparens
    erc
    )
  "The list of Lisp packages required by the TinySong layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


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
