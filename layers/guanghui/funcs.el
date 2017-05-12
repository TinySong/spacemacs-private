;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl)

;; (setq octopress-workdir (expand-file-name "~/4gamers.cn/"))

;; (defun zilongshanren/octopress-rake (command)
;;   "run rake commands"
;;   (let ((command-str (format "/bin/bash -l -c 'source $HOME/.rvm/scripts/rvm && rvm use ruby 2.0.0  && cd %s && rake %s'" octopress-workdir command)))
;;     (shell-command-to-string command-str)))

;; (defun zilongshanren/octopress-qrsync (command)
;;   (let ((command-str (format "/usr/local/bin/qrsync %s" command )))
;;     (shell-command-to-string command-str)))

;; (defun zilongshanren/octopress-generate ()
;;   "generate jekyll site"
;;   (interactive)
;;   (zilongshanren/octopress-rake "generate")
;;   (message "Generate site OK"))

;; (defun zilongshanren/octopress-deploy ()
;;   "default deploy task"
;;   (interactive)
;;   (zilongshanren/octopress-rake "deploy")
;;   (zilongshanren/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
;;   (message "Deploy site OK"))

;; (defun zilongshanren/octopress-gen-deploy ()
;;   "generate website and deploy"
;;   (interactive)
;;   (zilongshanren/octopress-rake "gen_deploy")
;;   (zilongshanren/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
;;   (message "Generate and Deploy OK"))

;; (defun zilongshanren/octopress-upimg ()
;;   (interactive)
;;   (zilongshanren/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
;;   (message "Up Img to Qiniu"))

;; (defun directory-parent (directory)
;;   (let ((parent (file-name-directory (directory-file-name directory))))
;;     (if (not (equal directory parent))
;;         parent)))

;; (defun zilongshanren/jekyll-serve ()
;;   (interactive)
;;   (let* ((default-directory
;;            (if (string-match "_posts/$" default-directory)
;;                (directory-parent (directory-parent default-directory))
;;              (directory-parent default-directory)))
;;          (buffer (if (get-buffer "*jekyll*")
;;                      (switch-to-buffer "*jekyll*")
;;                    (ansi-term "/bin/zsh" "jekyll")))
;;          (proc (get-buffer-process buffer)))
;;     (term-send-string proc "rake generate && rake preview\n")
;;     (sit-for 4)
;;     (browse-url "http://localhost:4000")))


;; insert ; at the end of current line
;; (defun zilongshanren/insert-semicolon-at-the-end-of-this-line ()
;;   (interactive)
;;   (save-excursion
;;     (end-of-line)
;;     (insert ";")))

;; (defun zilongshanren/delete-semicolon-at-the-end-of-this-line ()
;;   (interactive)
;;   (save-excursion
;;     (end-of-line)
;;     (if (looking-back ";")
;;         (progn
;;           (backward-char)
;;           (delete-char 1)))))

;; (defun zilongshanren/insert-comma-at-the-end-of-this-line ()
;;   (interactive)
;;   (save-excursion
;;     (end-of-line)
;;     (insert ",")))

;; (defun zilongshanren/delete-comma-at-the-end-of-this-line ()
;;   (interactive)
;;   (save-excursion
;;     (end-of-line)
;;     (if (looking-back ",")
;;         (progn
;;           (backward-char)
;;           (delete-char 1)))))

;; (defmacro zilongshanren|toggle-company-backends (backend)
;;   "Push or delete the backend to company-backends"
;;   (let ((funsymbol (intern (format "zilong/company-toggle-%S" backend))))
;;     `(defun ,funsymbol ()
;;        (interactive)
;;        (if (eq (car company-backends) ',backend)
;;            (setq-local company-backends (delete ',backend company-backends))
;;          (push ',backend company-backends)))))
