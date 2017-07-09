;;; funcs.el --- tinysong Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 tinysong
;;
;; Author: tinysong <tinysong1226@gmail.com>
;; URL: https://github.com/tinysong/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl-lib)
(require 'ido)
(require 'cl)

(setq octopress-workdir (expand-file-name "~/4gamers.cn/"))

(defun tinysong/octopress-rake (command)
  "run rake commands"
  (let ((command-str (format "/bin/bash -l -c 'source $HOME/.rvm/scripts/rvm && rvm use ruby 2.0.0  && cd %s && rake %s'" octopress-workdir command)))
    (shell-command-to-string command-str)))

(defun tinysong/octopress-qrsync (command)
  (let ((command-str (format "/usr/local/bin/qrsync %s" command )))
    (shell-command-to-string command-str)))

(defun tinysong/octopress-generate ()
  "generate jekyll site"
  (interactive)
  (tinysong/octopress-rake "generate")
  (message "Generate site OK"))

(defun tinysong/octopress-deploy ()
  "default deploy task"
  (interactive)
  (tinysong/octopress-rake "deploy")
  (tinysong/octopress-qrsync "/Users/song/4gamers.cn/song.json")
  (message "Deploy site OK"))

(defun tinysong/octopress-gen-deploy ()
  "generate website and deploy"
  (interactive)
  (tinysong/octopress-rake "gen_deploy")
  (tinysong/octopress-qrsync "/Users/song/4gamers.cn/song.json")
  (message "Generate and Deploy OK"))

(defun tinysong/octopress-upimg ()
  (interactive)
  (tinysong/octopress-qrsync "/Users/song/4gamers.cn/song.json")
  (message "Up Img to Qiniu"))

;; just for macOS
(defun tinysong/insert-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (tinysong/retrieve-chrome-current-tab-url)))
;; execute macOS AppleScript
(defun tinysong/retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "	set theUrl to get URL of active tab of first window\n"
                  "	set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n")))
        (title (do-applescript
                (concat
                 "set frontmostApplication to path to frontmost application\n"
                 "tell application \"Google Chrome\"\n"
                 "set theTitle to get title of active tab of first window\n"
                 "set theResult to (get theTitle) \n"
                 "end tell\n"
                 "activate application (frontmostApplication as text)\n"
                 "set links to {}\n"
                 "copy theResult to the end of links\n"
                 "return links as string\n"
                 ))))
    (format " %s\n \t %s" title (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))


;; "https://github.com/vhallac/.emacs.d/blob/master/config/customize-org-agenda.el"


;; remove all the duplicated emplies in current buffer
(defun tinysong/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;; for running long run ansi-term
(defun named-term (name)
  (interactive "sName: ")
  (ansi-term "/bin/zsh" name))


;; http://stackoverflow.com/questions/2886184/copy-paste-in-emacs-ansi-term-shell
(defun ash-term-hooks ()
  ;; dabbrev-expand in term
  (define-key term-raw-escape-map "/"
    (lambda ()
      (interactive)
      (let ((beg (point)))
        (dabbrev-expand nil)
        (kill-region beg (point)))
      (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; yank in term (bound to C-c C-y)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (current-kill 0)))))

(defun terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer-other-window "*ansi-term*")
    (progn
      (split-window-right-and-focus)
      (ansi-term "/bin/zsh")))
  (get-buffer-process "*ansi-term*"))

(defalias 'tt 'terminal)

(defun tinysong/comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode. Bind S-RET"
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun tinysong/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


;;add count for chinese, mainly used for writing chinese blog post
;; http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))
(defvar wc-regexp-chinese-punc
  "[。，！？；：「」『』（）、【】《》〈〉※—]")
(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun tinysong/word-count-for-chinese ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數（不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))

(defun prelude-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun prelude-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun tinysong/open-line-above()
  "open an empty line above the current line"
  (interactive)
  (save-excursion
    (evil-open-above 1)
    (evil-normal-state)
    ))

;; (defun tinysong/open-line-below()
;;   "open an empty line below the current line"
;;   (interactive)
;;   (save-excursion
;;     (evil-open-below 1)
;;     (evil-normal-state))
;;   )

(defun tinysong/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun tinysong/evil-quick-replace (beg end)
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string))
      )))


;; insert date and time
(defun tinysong/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun tinysong/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; "http://stackoverflow.com/questions/2242572/emacs-todo-indicator-at-left-side"
(defun tinysong/annotate-todo ()
  "put fringe marker on TODO: lines in the curent buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string (propertize "A"
                                                        'display '(left-fringe right-triangle)))))))
;; https://github.com/bbatsov/emacs-dev-kit/blob/master/editing-utils.el
(defun tinysong/move-line-up ()
  "move line up"
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun tinysong/move-line-down ()
  "move line down"
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun tinysong/copy-file-name-to-clipboard ()
  "Put the current file path name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


;;js2-mode enhancement
(defun my-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function-mode t)
  (which-function))

(defun tinysong/run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be emacs lisp, php, perl, python, ruby, javascript, bash, ocaml, Visual Basic.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-08-21"
  (interactive)
  (let* (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ;; ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("lua" . "lua")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal ξfSuffix "el") ; special case for emacs lisp
        (load ξfname)
      (if ξprog-name
          (progn
            (message "Running…")
            (async-shell-command ξcmd-str "*tinysong/run-current-file output*"))
        (message "No recognized program file suffix for this file.")))))

;; "http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/"
(defun tinysong/iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (tinysong/project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory ;
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      "
  tell application \"iTerm\"
       activate
       set _session to current session of current terminal
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  " cmd))))


;; just for maxcos
;; https://github.com/syohex/emacs-browser-refresh/blob/master/browser-refresh.el
(defun tinysong/browser-refresh--chrome-applescript ()
  (interactive)
  (do-applescript
   (format
    "tell application \"Google Chrome\"
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
" )))

(define-minor-mode
  shadowsocks-proxy-mode
  :global t
  :init-value nil
  :lighter " SS"
  (if shadowsocks-proxy-mode
      (setq url-gateway-method 'socks)
    (setq url-gateway-method 'native)))


(define-global-minor-mode
  global-shadowsocks-proxy-mode shadowsocks-proxy-mode shadowsocks-proxy-mode
  :group 'shadowsocks-proxy)



;;  ================= erc for macos =======================
;; http://blog.lojic.com/2009/08/06/send-growl-notifications-from-carbon-emacs-on-osx/
(defun tinysong/growl-notification (title message &optional sticky)
  "Send a Growl notification"
  (do-applescript
   (format "tell application \"GrowlHelperApp\" \n
              notify with name \"Emacs Notification\" title \"%s\" description \"%s\" application name \"Emacs.app\" sticky \"%s\"
              end tell
              "
           title
           message
           (if sticky "yes" "no"))))

;; TODO tinysong send notification for linux
(defun tinysong/growl-timer (minutes message)
  "Issue a Growl notification after specified minutes"
  (interactive (list (read-from-minibuffer "Minutes: " "10")
                     (read-from-minibuffer "Message: " "Reminder") ))
  (run-at-time (* (string-to-number minutes) 60)
               nil
               (lambda (minute message)
                 (tinysong/growl-notification "Emacs Reminder" message t))
               minutes
               message))

(defun tinysong/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elfeed feed reader                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;shortcut functions
(defun tinysong/elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))
(defun tinysong/elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))
(defun tinysong/elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

;;write to disk when quiting
(defun tinysong/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
(defun tinysong/elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))


;; (defun tinysong/sync-k8s-to-devops ()
;;   (interactive)
;;   (let (command-str (format "rsync -a --delete --progress \"-e ssh -p 22\" ~/development/golang/src/k8s.io/kubernetes/* root@192.168.0.5:/kubernetes/src/github.com/kubernetes/kubernetes/")))
;;   (defun sync-to-dev ()
;;     "sync .go file to develop machine with external script."
;;     (when (eq major-mode 'go-mode)
;;       (shell-command-to-string command-str)
;;       ))
;;   (add-hook 'after-save-hook 'sync-to-dev)
;;   )

(defun tinysong/hotspots ()
  "helm interface to my hotspots, which includes my locations,
   org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(tinysong/hotspots-sources))))

(defun tinysong/hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
    (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                   ("RSS" . elfeed)
                   ("Blog" . org-octopress)
                   ("Github" . (lambda() (helm-github-stars)))
                   ("Calculator" . (lambda () (helm-calcul-expression)))
                   ("Run current flie" . (lambda () (tinysong/run-current-file)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))
                   ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

(defun tinysong/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq fullpath
        (concat (file-name-directory (buffer-file-name))
                "../images/posts/"
                (file-name-base (buffer-file-name))
                "_"
                basename))
  (setq relativepath
        (concat (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (if (file-exists-p (file-name-directory fullpath))
      (progn
        (setq final-image-full-path (concat fullpath ".png"))
        (call-process "screencapture" nil nil nil "-s" final-image-full-path)
        (if (executable-find "convert")
            (progn
              (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
              (shell-command-to-string resize-command-str)))
        (tinysong//insert-org-or-md-img-link "http://guanghuiqu.qiniudn.com/" relativepath))
    (progn
      (call-process "screencapture" nil nil nil "-s" (concat basename ".png"))
      (tinysong//insert-org-or-md-img-link "./" (concat basename ".png"))))
  (insert "\n"))

(defun tinysong//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))

(defun tinysong/jekyll-serve ()
  (interactive)
  (let* ((default-directory
           (if (string-match "_posts/$" default-directory)
               (directory-parent (directory-parent default-directory))
             (directory-parent default-directory)))
         (buffer (if (get-buffer "*jekyll*")
                     (switch-to-buffer "*jekyll*")
                   (ansi-term "/bin/zsh" "jekyll")))
         (proc (get-buffer-process buffer)))
    (term-send-string proc "rake generate && rake preview\n")
    (sit-for 4)
    (browse-url "http://localhost:4000")))

(defun tinysong/insert-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun tinysong/delete-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ";")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun tinysong/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun tinysong/delete-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ",")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun tinysong/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "song")))

(defmacro tinysong|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "ts/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))



(defun tinysong/dired-rsync ()
  (interactive)
  ;; offer dwim target as the suggestion
  (setq cadicate-list (list (expand-file-name (read-file-name "Rsync to:" (projectile-project-root) "*"))))
  ;; store all selected files into "files" list
  (setq tmtxt/rsync-command "rsync -arvz --delete --progress ")
  (if (string-equal (substring (car cadicate-list) -1) "*")
      (progn (setq tmtxt/rsync-command
                   (concat tmtxt/rsync-command
                           (car cadicate-list) " " rsync-remote-host ":" rsync-remote-path))
             ;; run the async shell command
             (async-shell-command tmtxt/rsync-command "*rsync*")
             (other-window 1)
             )
    ;; rsync all file to destinations
    ;; add all selected file names as arguments to the rsync command
    (dolist (file cadicate-list)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " "))
      ;; append the destination
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command " " rsync-remote-host ":" rsync-remote-path))
      ;; run the async shell command
      (async-shell-command tmtxt/rsync-command "*rsync*")
      ;; finally, switch to that window
      (other-window 1)
      )))
