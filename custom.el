;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(beacon-blink-delay 0.3)
 '(cfs--current-profile-name "profile1" t)
 '(compilation-message-face (quote default))
 '(ctags-update-delay-seconds 1024)
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(erc-nick "TinySong")
 '(erc-port 6667)
 '(evil-want-C-i-jump t)
 '(evil-want-Y-yank-to-eol t)
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#3C3D37" t)
 '(flycheck-display-errors-delay 0.2)
 '(golden-ratio-exclude-modes
   (quote
    ("bs-mode" "calc-mode" "ediff-mode" "dired-mode" "gud-mode" "gdb-locals-mode" "gdb-registers-mode" "gdb-breakpoints-mode" "gdb-threads-mode" "gdb-frames-mode" "gdb-inferior-io-mode" "gud-mode" "gdb-inferior-io-mode" "gdb-disassembly-mode" "gdb-memory-mode" "restclient-mode" "speedbar-mode" term-mode)))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(helm-ag-always-set-extra-option nil)
 '(helm-gtags-update-interval-second 35)
 '(helm-ls-git-fuzzy-match t)
 '(helm-ls-git-show-abs-or-relative (quote absolute))
 '(hide-ifdef-initially t)
 '(hide-ifdef-lines t)
 '(hide-ifdef-shadow t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(ivy-height 18)
 '(large-file-warning-threshold nil)
 '(lua-documentation-url "http://www.lua.org/manual/5.3/manual.html")
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-agenda-custom-commands nil)
 '(org-agenda-files nil)
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-clock-idle-time 15)
 '(org-clock-sound t)
 '(org-deadline-warning-days 14)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-pomodoro-play-sounds t)
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (irony calfw bug-hunter tide typescript-mode elfeed-web elfeed-goodies noflet org-brain org-jira confluence xml-rpc evil-lion sayid password-generator editorconfig realgud test-simple loc-changes load-relative company-lua ycmd request-deferred string-inflection go-rename symon browse-at-remote tabbar org-mac-iCal org-caldav theme-changer sunshine rase osx-location slack emojify circe oauth2 websocket zenburn-theme magithub fuzzy elfeed-org winum godoctor mu4e-maildirs-extension mu4e-alert spray pug-mode window-purpose imenu-list hide-comnt graphviz-dot-mode go-guru glsl-mode dockerfile-mode docker tablist docker-tramp wgrep smex ivy-purpose ivy-hydra flyspell-correct-ivy counsel-projectile counsel-dash counsel unfill bing-dict fasd helm-purpose auctex-latexmk sudoku dumb-jump disaster diminish diff-hl ctags-update tern company-go go-mode company-emoji company-auctex company column-enforce-mode color-identifiers-mode cmake-mode clojure-snippets inflections eval-sexp-fu cider pkg-info clojure-mode bind-map bind-key beacon seq yasnippet auto-compile packed auctex anaconda-mode pythonic ace-window ace-pinyin pinyinlib ace-link ace-jump-helm-line helm avy helm-core auto-complete popup 4clojure quelpa package-build names yapfify yaml-mode xterm-color wrap-region which-key visual-regexp-steroids visual-regexp uuidgen typit mmt toc-org stickyfunc-enhance srefactor spacemacs-theme spaceline powerline smeargle shell-pop selectric-mode reveal-in-osx-finder restart-emacs ranger pytest py-isort prodigy persp-mode pcre2el paradox pangu-spacing pacmacs ox-gfm osx-trash osx-dictionary orgit org-tree-slide org-projectile org-pomodoro org-plus-contrib org nodejs-repl mwim mmm-mode markdown-mode magit-gitflow magit-gh-pulls lua-mode lorem-ipsum livid-mode skewer-mode live-py-mode litable zoutline swiper link-hint launchctl keyfreq js2-mode jade-mode info+ ibuffer-projectile hy-mode htmlize hl-todo help-fns+ projectile helm-flx helm-dash helm-company haml-mode gulpjs google-c-style go-eldoc gnuplot gitignore-mode github-search github-clone git-gutter-fringe+ git-gutter-fringe git-gutter+ git-gutter marshal ht flyspell-correct-helm flyspell-correct flycheck-gometalinter flycheck find-file-in-project ivy find-by-pinyin-dired evil-visual-mark-mode evil-vimish-fold vimish-fold evil-unimpaired evil-mc evil-magit magit-popup git-commit with-editor smartparens evil-indent-plus iedit evil-escape evil-ediff evil undo-tree eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emoji-cheat-sheet-plus edn paredit queue peg json-rpc dash-functional web-completion-data makey anzu highlight goto-chg flx gh logito pcache pos-tip guide-key request parent-mode simple-httpd json-snatcher json-reformat multiple-cursors moz ctable orglue epic alert log4e gntp spinner epl hydra async deferred f s chinese-word-at-point dash youdao-dictionary ws-butler window-numbering web-mode web-beautify volatile-highlights vi-tilde-fringe use-package tagedit smooth-scrolling slim-mode scss-mode sass-mode rfringe reveal-in-finder rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pyenv-mode popwin pip-requirements persp-projectile pbcopy page-break-lines ox-reveal org-repo-todo org-present org-octopress org-mac-link org-download org-bullets open-junk-file neotree multi-term moz-controller move-text monokai-theme markdown-toc magit macrostep lispy linum-relative leuven-theme less-css-mode json-mode js2-refactor js-doc indent-guide impatient-mode ido-vertical-mode hungry-delete hl-anything highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-ls-git helm-gtags helm-gitignore helm-github-stars helm-flyspell helm-descbinds helm-css-scss helm-c-yasnippet helm-ag guide-key-tip google-translate golden-ratio github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh-md ggtags geiser fringe-helper flycheck-ycmd flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-args evil-anzu engine-mode emmet-mode elisp-slime-nav elfeed discover-my-major deft dash-at-point cython-mode company-ycmd company-web company-tern company-statistics company-quickhelp company-c-headers company-anaconda command-log-mode coffee-mode cmake-font-lock clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu chinese-fonts-setup buffer-move auto-yasnippet auto-highlight-symbol auto-dictionary align-cljlet aggressive-indent adaptive-wrap ace-jump-mode ac-ispell 2048-game)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eval setenv "PYTHONPATH" "$HOME/.local/lib/python2.7/site-packages/yapf"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(sp-show-pair-from-inside t)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(ycmd-extra-conf-handler (quote load))
 '(ycmd-extra-conf-whitelist (quote ("~/cocos2d-x/*"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F5F5F5" :background "#1B1E1C"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "green"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(git-gutter-fr:added ((t (:foreground "#859900" :weight bold :width extra-expanded))))
 '(helm-ls-git-modified-and-staged-face ((t (:foreground "dark cyan"))))
 '(helm-ls-git-modified-not-staged-face ((t (:foreground "dark cyan"))))
 '(helm-ls-git-renamed-modified-face ((t (:foreground "dark cyan"))))
 '(js2-error ((t (:foreground "light sea green"))))
 '(js2-external-variable ((t (:foreground "gray52"))))
 '(sp-show-pair-match-face ((t (:background "#272822" :foreground "gray" :inverse-video t :weight normal)))))
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
