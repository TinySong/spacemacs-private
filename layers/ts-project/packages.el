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
    find-file-in-project
    ))



(defun ts-project/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :init
    (progn
      (bind-key* "s-p" 'ts-project/open-file-with-projectile-or-lsgit)
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
