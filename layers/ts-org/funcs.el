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


(defun ts-org/easy-hugo-ag ()
  "Search for blog article with helm-ag."
  (interactive)
  (spacemacs/counsel-search (list "ag") nil easy-hugo-basedir)
  (error "search %s error",easy-hugo-postdir))


(defun ts-org/easy-hugo-publish ()
  "convert .org to .md and publish new path"
  (interactive)
  ;; base-dir
  (buffer-file-name)
  (setq destpath (string-remove-prefix (expand-file-name (concat easy-hugo-basedir "/org-blog"))  (file-name-directory buffer-file-name)))
  (message "%s" destpath)
  (let ((outfile (org-export-output-file-name ".md" nil (concat easy-hugo-basedir "content" destpath) )))
    (message "%s" outfile)
    (org-export-to-file 'gfm outfile))
  )


(setq hugo-base-dir "~/hugo-blog/")

(defun today-is ()
  (format-time-string "%Y-%m-%d-"))

(defun now-is ()
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))

(defun org-hugo-new-post ()
  "Creates a new org file in Hugo's content/post directory"
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (subtitle (read-from-minibuffer "SubTitle: "))
         (filename (read-from-minibuffer "Filename: "
                                         (replace-regexp-in-string "-\\.org" ".org"
                                                                   (concat (downcase
                                                                            (replace-regexp-in-string "[^a-z0-9]+" "-" title))
                                                                           ".org"))))
         (blogfile (concat "post/" (today-is) filename))
         (url (concat (downcase
                       (replace-regexp-in-string "[^a-z0-9]+" "-"
                                                 title))))
         (path (concat hugo-base-dir "content/" blogfile)))

    (if (file-exists-p path)
        (message "File already exists!")
      (message blogfile)
      (switch-to-buffer (generate-new-buffer blogfile))
      (insert "#+DATE: " (now-is))
      (insert "\n#+TITLE: " title)
      (insert "\n#+SUBTITLE: " subtitle)
      (insert "\n#+URL: /" url "/")
      (insert "\n#+BANNER: \n#+DESCRIPTION: \n#+IMAGE: \n#+CATEGORIES: \n#+TAGS:\n\n")
      (org-mode)
      (write-file path)
      (goto-char (point-max)))))
