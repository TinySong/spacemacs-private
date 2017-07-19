;;; packages.el --- ts-org layer packages file for Spacemacs. ;; -*- lexical-binding: t -*-
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
;; added to `ts-org-packages'. Then, for each package PACKAGE:
;;

;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ts-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ts-org/pre-init-PACKAGE' and/or
;;   `ts-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(defconst ts-org-packages
  '(
    org
    org-tree-slide
    org-octopress
    org-pomodoro
    ;; org-caldav
    ;; org-mac-iCal
    ;; org-jira  ;;TODO  don't know emacs how to read /etc/hosts info when access internet
    ))

(defun ts-org/post-init-org ()
  (with-eval-after-load 'org
    (progn
      ;; https://github.com/syl20bnr/spacemacs/issues/2994#issuecomment-139737911
      (spacemacs|disable-company org-mode)
      ;; set org-priority major-mode leader key
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "," 'org-priority)
      ;; (require 'org-compat)
      ;; http://orgmode.org/manual/Tracking-your-habits.html
      (require 'org-habit)
      (add-to-list 'org-modules 'org-habit)

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))
      ;; config stuck project
      ;; http://orgmode.org/manual/Stuck-projects.html#Stuck-projects
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t)       ;; ~50x speedup
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-include-diary t)
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)

      ;; ;FIX: ~t~ keybindings evil-find-char-to
      ;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg112173.html
      (evil-define-key 'normal evil-org-mode-map
        (kbd "t") 'org-todo)

      ;; 加密文章
      ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
      ;; https://www.emacswiki.org/emacs/EasyPG#toc5
      ;; org-mode 設定
      (require 'org-crypt)

      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)

      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")

      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))

      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)

      (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|text\\)\\’" . org-mode))



      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)" "|" "NEXT(n)")
                    (sequence "HOLD(h)" "WAITING(w@/!)" "SOMEDAY(S)"  "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line
      (setq org-clock-persist 'history)
      (org-clock-persistence-insinuate)
      (setq org-tags-match-list-sublevels nil)

      ;; http://wenshanren.org/?p=327
      ;; change it to helm
      (defun ts-org/org-insert-src-block (src-code-type)
        "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
        (interactive
         (let ((src-code-types
                '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                  "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                  "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                  "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                  "scheme" "sqlite" "yaml" "go")))
           (list (ido-completing-read "Source code type: " src-code-types))))
        (progn
          (newline-and-indent)
          (insert (format "#+BEGIN_SRC %s\n" src-code-type))
          (newline-and-indent)
          (insert "#+END_SRC\n")
          (previous-line 2)
          (org-edit-src-code)))

      (add-hook 'org-mode-hook '(lambda ()
                                  ;; keybinding for editing source code blocks
                                  ;; keybinding for inserting code blocks
                                  (local-set-key (kbd "C-c i s")
                                                 'ts-org/org-insert-src-block)
                                  ))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iS" 'ts-org/org-insert-src-block)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "ie" 'org-edit-src-code)
      ;; http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
      (require 'ox-publish)
      (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}
      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process
            '(
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "rm -fr %b.out %b.log %b.tex auto"))

      (setq org-latex-listings t)

      ;; =================== start ========================================================
      ;;reset all subtask when finish the repeat task
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

      (defun org-reset-subtask-state-subtree ()
        "Reset all subtasks in an entry subtree."
        (interactive "*")
        (if (org-before-first-heading-p)
            (error "Not inside a tree")
          (save-excursion
            (save-restriction
              (org-narrow-to-subtree)
              (org-show-subtree)
              (goto-char (point-min))
              (beginning-of-line 2)
              (narrow-to-region (point) (point-max))
              (org-map-entries
               '(when (member (org-get-todo-state) org-done-keywords)
                  (org-todo (car org-todo-keywords))))
              ))))

      (defun org-reset-subtask-state-maybe ()
        "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
        (interactive "*")
        (if (org-entry-get (point) "RESET_SUBTASKS")
            (org-reset-subtask-state-subtree)))

      (defun org-subtask-reset ()
        (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
          (org-reset-subtask-state-maybe)
          (org-update-statistics-cookies t)))

      (add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)
      ;; =================== end ========================================================

      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")


      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (sh . t)
         (js . t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (ditaa . t)))
      ;; (setq org-agenda-files (quote ("~/org-notes/gtd.org" "~/org-notes/notes.org" "~/org-notes/CD.org" "~/org-notes/programLang.org")))

      ;; (setq  org-default-notes-file (quote ("~/org-notes" )))
      ;; (setq org-default-notes-file "~/org-notes/gtd.org")
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-default-kernel-file (expand-file-name "kernel.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))


      (with-eval-after-load 'org-agenda
        (when (configuration-layer/package-usedp 'org-pomodoro)
          ;; (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
          )

        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body)
        )
      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
               "* TODO [#B] %?\n  %i\n%U"
               :empty-lines 1)
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %U"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
               "* TODO [#B] %?\n  %i\n %U"
               :empty-lines 1)
              ("k" "Kernel" entry (file+headline org-default-kernel-file "Kernel Quick Note")
               "* TODO [#B] %?\n  %i\n %U"
               ;; :empty-lines 1
               )
              ("w" "work" entry (file+headline org-agenda-file-gtd "Careerdream")
               "* TODO [#A] %?\n  %i\n %U"
               :empty-lines 1)
              ("c" "Chrome" entry (file+headline org-default-notes-file "Quick notes")
               "* TODO [#C] %?\n %(tinysong/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline org-default-notes-file "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("L" "CodeLine" entry (file+headline org-agenda-file-gtd "CodeTask")
               "* TODO [#B] %?\n  %(ts-org/position-to-kill-ring)\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               entry (file+datetree org-agenda-file-journal)
               "* %?"
               :empty-lines 1)))

      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "项目安排")
              ;; ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"tinysong\"")
              ("W" "Weekly Review"
               ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))

      (defvar tinysong-website-html-preamble
        "<div class='nav'>
<ul>
<li><a href='http://tinysong.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
      (defvar tinysong-website-html-blog-head
        " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
      (setq org-publish-project-alist
            `(
              ("blog-notes"
               :base-directory "~/org-notes"
               :base-extension "org"
               :publishing-directory "~/org-notes/public_html/"

               :recursive t
               :html-head , tinysong-website-html-blog-head
               :publishing-function org-html-publish-to-html
               :headline-levels 4       ; Just the default for this project.
               :auto-preamble t
               :exclude "gtd.org"
               :exclude-tags ("ol" "noexport")
               :section-numbers nil
               :html-preamble ,tinysong-website-html-preamble
               :author "tinysong"
               :email "tinysong1226@gmail.com"
               :auto-sitemap t               ; Generate sitemap.org automagically...
               :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
               :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
               :sitemap-sort-files anti-chronologically
               :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
               )
              ("blog-static"
               :base-directory "~/org-notes"
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
               :publishing-directory "~/org-notes/public_html/"
               :recursive t
               :publishing-function org-publish-attachment
               )
              ("blog" :components ("blog-notes" "blog-static"))))

      (defun org-summary-todo (n-done n-not-done)
        "Switch entry to DONE when all subentries are done, to TODO otherwise."
        (let (org-log-done org-log-states) ; turn off logging
          (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

      (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
      ;; used by org-clock-sum-today-by-tags
      (defun filter-by-tags ()
        (let ((head-tags (org-get-tags-at)))
          (member current-tag head-tags)))

      (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
        (interactive "P")
        (let* ((timerange-numeric-value (prefix-numeric-value timerange))
               (files (org-add-archive-files (org-agenda-files)))
               (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                               "LIFE" "PROJECT" "OTHER"))
               (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
               (output-string "")
               (tstart (or tstart
                           (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                           (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                           (org-time-today)))
               (tend (or tend
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                         (+ tstart 86400)))
               h m file item prompt donesomething)
          (while (setq file (pop files))
            (setq org-agenda-buffer (if (file-exists-p file)
                                        (org-get-agenda-file-buffer file)
                                      (error "No such file %s" file)))
            (with-current-buffer org-agenda-buffer
              (dolist (current-tag include-tags)
                (org-clock-sum tstart tend 'filter-by-tags)
                (setcdr (assoc current-tag tags-time-alist)
                        (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
          (while (setq item (pop tags-time-alist))
            (unless (equal (cdr item) 0)
              (setq donesomething t)
              (setq h (/ (cdr item) 60)
                    m (- (cdr item) (* 60 h)))
              (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
          (unless donesomething
            (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
          (unless noinsert
            (insert output-string))
          output-string))
      (global-set-key (kbd "C-c a") 'org-agenda)
      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (define-key global-map (kbd "<f9>") 'org-capture)
      (global-set-key (kbd "C-c b") 'org-iswitchb)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "owh" 'plain-org-wiki-helm
        )
      (setq org-mobile-directory "~/org-notes/org")
      )))


(defun ts-org/init-org-tree-slide ()
  (use-package org-tree-slide
    :init
    ;; (spacemacs/set-leader-keys "oto" 'org-tree-slide-mode)
    :defer t
    :config))


(defun ts-org/init-org-octopress ()
  (use-package org-octopress
    :defer t
    :init
    (progn
      (evilified-state-evilify org-octopress-summary-mode org-octopress-summary-mode-map)
      (add-hook 'org-octopress-summary-mode-hook
                #'(lambda () (local-set-key (kbd "q") 'bury-buffer)))

      (defun ts-org/org-save-and-export ()
        (interactive)
        (org-octopress-setup-publish-project)
        (org-publish-project "octopress" t))

      (spacemacs/set-leader-keys "op" 'ts-org/org-save-and-export)
      )))

(defun ts-org/post-init-org-pomodoro ()
  (use-package org-pomodoro
    :init
    :defer t
    :config
    (progn
      ;; (tinysong/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)
      (add-hook 'org-pomodoro-finished-hook '(lambda () (tinysong/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)))
      (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (tinysong/growl-notification "Short Break" "🐝 Ready to Go?" t)))
      (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (tinysong/growl-notification "Long Break" " 💪 Ready to Go?" t)))
      )))

(defun ts-org/init-org-caldav ()
  (use-package org-caldav
    :init
    :config
    (progn
      (setq org-caldav-url "https://www.google.com/calendar/dav")
      (setq org-caldav-calendar-id "tinysong1226@gmail.com")
      (setq org-caldav-inbox "~/org-note/caldav.org")
      (setq org-caldav-files org-agenda-files)
      )
    )
  )

(defun ts-org/init-org-mac-iCal ()
  (use-package org-mac-iCal
    :init
    :config
    (progn
      (setq org-agenda-include-diary t)))
  )



(defun ts-org/init-org-jira ()
  (use-package org-jira
    :init
    (setq jiralib-url "http://jira.tenxcloud.com")
    :config (spacemacs|diminish org-jira-mode " Ⓙ" "jr")
    ;; TODO test domain code inflowing
    ;; (let ((domain "jira.tenxcloud.com"))
    ;;   (switch-to-buffer-other-window "*temp*")
    ;;   (erase-buffer)
    ;;   (setq www (format "www.%s" domain))
    ;;   (setq ip "")
    ;;   (setq mx "")
    ;;   ;; (setq ret-val (call-process "ping" nil t nil domain "-n" "1"))
    ;;   (setq ret-val (call-process-shell-command "ping" nil t nil domain "-n" "1"))
    ;;   (if (equal ret-val 0)
    ;;       (progn
    ;;         (goto-char 1)
    ;;         (setq ip (buffer-substring-no-properties
    ;;                   (search-forward "[") (- (search-forward "]") 1)))))
    ;;   (erase-buffer)
    ;;   (call-process-shell-command "nslookup" nil t nil "-type=MX" domain)
    ;;   (goto-char 1)
    ;;   (if (search-forward "mail exchanger = " nil t)
    ;;       (progn
    ;;         (setq mx (thing-at-point 'filename))))
    ;;   (other-window 1)
    ;;   (print (concat "\t" ip "\t" mx)))
    ;; (url-gateway-nslookup-host "jira.tenxcloud.com")
    ))
