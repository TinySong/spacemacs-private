;;; funcs.el --- tinysong Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 tinysong
;;
;; Author: tinysong <guanghui8827@gmail.com>
;; URL: https://github.com/tinysong/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl-lib)
(require 'ido)


(defun ts-org/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun ts-org/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

(defun ts-org/position-to-kill-ring ()
  "Copy to the kill ring a string in the format
  \"file:file-name::line-number\"for the current
  buffer's file name, and the line number at point."
  (interactive)
  (kill-new
   (format "file:%s::%d"
           (buffer-file-name) (save-restriction
                                (widen) (line-number-at-pos)))))
