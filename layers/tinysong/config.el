;; Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 tinysong
;;
;; Author: tinysong <TinySong1226@gmail.com>
;; URL: https://github.com/tinysong/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; (spacemacs|add-company-backends lua-mode)
(spacemacs|add-company-backends :backends company-capf :modes lua-mode)
;; markdown-mode org-mode nxml-mode sh-mode
;; (spacemacs|add-company-backends markdown-mode)
;; (spacemacs|add-company-backends org-mode)
;; (spacemacs|add-company-backends nxml-mode)
;; (spacemacs|add-company-backends sh-mode)
;; (spacemacs|add-company-backends sh-mode)

;;(spacemacs/declare-prefix "ot" "Toggle")

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "Ti")

(add-hook 'term-mode-hook 'ash-term-hooks)


(global-prettify-symbols-mode 1)
;; (setq-default fill-column 110)

(setq recenter-positions '(top middle bottom))
;; delete the selection with a key press
(delete-selection-mode t)


;;add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     ;; haskell-mode
                     ;; ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))


;; tramp, for sudo access
;; very slow!!!!
;; for profiling emacs --debug-init --timed-requires --profile
;; (require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "scp")
(setq tramp-default-user "root"
      tramp-default-host "btsong")

(set-default 'imenu-auto-rescan t)

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-mode))
       auto-mode-alist))

(setq large-file-warning-threshold 100000000)
;;http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/
;;need to install coreutils at first

(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

;; reformat your json file, it requires python
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

;; when save a buffer, the directory is not exsits, it will ask you to create the directory
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))


;; http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

;;  associate xml, xsd, etc with nxml-mode
(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt '("xml" "xsd" "rng" "xslt" "xsl") t) "\\'") 'nxml-mode))
;;;### et the automplete flag to true
(setq nxml-slash-auto-complete-flag t)


;; cleanup recent files
(add-hook 'kill-emacs-hook #'(lambda () (progn (and (fboundp 'recentf-cleanup) (recentf-cleanup))
                                           (and (fboundp 'projectile-cleanup-known-projects) (projectile-cleanup-known-projects)))))

;; change evil initial mode state
(menu-bar-mode t)

;;  add .c or .mm suffix file info c mode
;; auto-mode-alist: https://www.emacswiki.org/emacs/AutoModeAlist
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))


(setq url-show-status nil)

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'endless/org-ispell)

;;set region face for monokai theme
(set-face-attribute 'region nil :background "#696969")


;;Don’t ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;Don’t ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; return nil to write content to file
;;(defun tinysong/untabify-buffer ()
;;  (interactive)
;;  (save-excursion
;;    (untabify (point-min) (point-max)) nil))
;;
;;(add-hook 'c++-mode-hook
;;          '(lambda ()
;;             (add-hook 'write-contents-hooks 'tinysong/untabify-buffer nil t)))
;;
;; (add-hook 'go-mode-hook
;;           '(lambda ()
;;              (add-hook 'write-contents-hooks 'tinysong/untabify-buffer nil t)))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " TinySong - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; https://www.emacswiki.org/emacs/AbbrevMode
(define-abbrev-table 'global-abbrev-table '(

                                            ;; math/unicode symbols
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8en" "@~english")
                                            ("8zh" "@~chinese")
                                            ("8sp" "spacemacs")
                                            ;; email
                                            ("8me" "TinySong1226@gmail.com")

                                            ;; computing tech
                                            ("8wp" "Wikipedia")
                                            ("8ms" "Microsoft")
                                            ("8g" "Google")
                                            ("8it" "IntelliType")
                                            ("8msw" "Microsoft Windows")
                                            ("8win" "Windows")
                                            ("8ie" "Internet Explorer")
                                            ("8ahk" "AutoHotkey")
                                            ("8ks" "kubernetes")
                                            ;; ("82dx" "Cocos2D-X")

                                            ;; signature
                                            ("8ts" "tinysong")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))


(defvar rsync-remote-host "root@192.168.4.145"
  "remote host name.")

(defvar rsync-remote-path "/data/src/"
  "remote path.")

(defvar subcommand "--exclude=bin ")

(with-eval-after-load 'flycheck
  (setq flycheck-display-errors-delay 0.1)
  )


(setq yahoo-weather-location "北京")
