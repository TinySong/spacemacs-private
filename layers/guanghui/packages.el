;;; packages.el --- guanghui Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq guanghui-packages
      '(
        prodigy
        js2-mode
        nodejs-repl
        ycmd
        discover-my-major
        ace-window
        avy
        4clojure
        ;; persp-mode
        (gulpjs :location (recipe :fetcher github :repo "zilongshanren/emacs-gulpjs"))
        litable
        pangu-spacing
        ))

(defun guanghui/post-init-command-log ()
  (with-eval-after-load 'command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun guanghui/post-init-pangu-spacing ()
  (progn
    ;; add toggle options
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode"
      :evil-leader "ots")
    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

(defun guanghui/init-litable ()
  (use-package litable
    :init))

(defun guanghui/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      )))

(defun guanghui/init-gulpjs ()
  (use-package gulpjs
    :init
    (progn
      (spacemacs/set-leader-keys "ags" 'gulpjs-start-task)
      (spacemacs/set-leader-keys "agr" 'gulpjs-restart-task))))

(defun guanghui/init-4clojure ()
  (use-package 4clojure
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (spacemacs/set-leader-keys "o4q" '4clojure-open-question)
      (spacemacs/set-leader-keys "o4n" '4clojure-next-question)
      (spacemacs/set-leader-keys "o4p" '4clojure-previous-question)
      (spacemacs/set-leader-keys "o4c" '4clojure-check-answers)
      )))

(defun guanghui/post-init-popwin ()
  (progn
    (push "*zilongshanren/run-current-file output*" popwin:special-display-config)
    (delete "*Async Shell Command*" 'popwin:special-display-config)
    ))

(defun guanghui/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (progn
      (setq org-reveal-root "file:///Users/guanghui/.emacs.d/reveal-js"))))

(defun guanghui/init-org-mac-link ()
  (use-package org-mac-link
    :init
    (add-hook 'org-mode-hook
              (lambda ()
                (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))))

(defun guanghui/post-init-avy ()
  (progn
    (global-set-key (kbd "C-s-'") 'avy-goto-char-2)))

(defun guanghui/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun guanghui/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)

      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))

(defun guanghui/post-init-ycmd ()
  (progn
    (setq ycmd-tag-files 'auto)
    (setq ycmd-request-message-level -1)
    (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/Github/ycmd/ycmd/__main__.py")))
    (setq company-backends-c-mode-common '((company-c-headers
                                            company-dabbrev-code
                                            company-keywords
                                            company-gtags :with company-yasnippet)
                                           company-files company-dabbrev ))

    (zilongshanren|toggle-company-backends company-ycmd)
    (eval-after-load 'ycmd
      '(spacemacs|hide-lighter ycmd-mode))

    (spacemacs/set-leader-keys-for-major-mode 'c-mode
      "tb" 'zilong/company-toggle-company-ycmd)
    (spacemacs/set-leader-keys-for-major-mode 'c++-mode
      "tb" 'zilong/company-toggle-company-ycmd)))

(defun guanghui/post-init-lua-mode ()
  (progn
    (when (configuration-layer/package-usedp 'company)
      (push 'company-dabbrev company-backends-lua-mode)
      (push 'company-etags company-backends-lua-mode))
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    (add-hook 'lua-mode-hook 'smartparens-mode)
    (setq lua-indent-level 4)

    (spacemacs/set-leader-keys-for-major-mode 'lua-mode
      "<tab>" 'hs-toggle-hiding
      "gg" 'helm-gtags-dwim
      "gr" 'helm-gtags-find-rtag
      "gs" 'helm-gtags-find-symbol
      "gf" 'helm-gtags-find-files)))

(defun guanghui/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "zilongshanren")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))


(defun guanghui/post-init-lispy ()
  (with-eval-after-load 'lispy
    (progn
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline)

      )))

(defun guanghui/init-hydra ()
  (use-package hydra
    :init
    (progn
      ;; major mode hydra is really cool, don't need to switch mode anymore
      ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
      ;; If the command will change the buffer, they should be put in these groups.
      ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      ;; (bin-key* "<f3>" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (bind-key*  "<f4>" 'hydra-apropos/body)
      )))


(defun guanghui/post-init-nodejs-repl ()
  (progn
    (spacemacs/declare-prefix-for-mode 'js2-mode
                                       "ms" "REPL")
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "sb" 'nodejs-repl-eval-buffer
      "sf" 'nodejs-repl-eval-function
      "sd" 'nodejs-repl-eval-dwim)))

(defun guanghui/post-init-visual-regexp-steroids ()
  (progn
    (define-key global-map (kbd "C-c r") 'vr/replace)
    (define-key global-map (kbd "C-c q") 'vr/query-replace))
  )

(defun guanghui/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

;; (defun guanghui/post-init-org-bullets ()
;;   (setq org-bullets-bullet-list '("🐉" "🐠" "🐬" "🐤")))

(defun guanghui/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda () (zilongshanren/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (zilongshanren/growl-notification "Short Break" "☕🐝 Ready to Go?" t)))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (zilongshanren/growl-notification "Long Break" "☕💪 Ready to Go?" t)))
    ;; (add-hook 'org-pomodoro-finished-hook '(lambda () (zilongshanren/growl-notification "Pomodoro Finished" "* Have a break!" t)))
    ;; (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (zilongshanren/growl-notification "Short Break" "* Ready to Go?" t)))
    ;; (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (zilongshanren/growl-notification "Long Break" " * Ready to Go?" t)))
    ))

(defun guanghui/post-init-js2-mode ()
  (progn
    (setq company-backends-js2-mode '((company-dabbrev-code
                                       company-keywords
                                       company-etags) company-files company-dabbrev))

    (zilongshanren|toggle-company-backends company-tern)


    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "tb" 'zilong/company-toggle-company-tern)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)

    (spacemacs/set-leader-keys-for-major-mode 'web-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)
    (eval-after-load 'js2-mode
      '(progn
         (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
         (define-key js2-mode-map   (kbd "s-.") 'company-tern)))))



;; http://rejeep.github.io/emacs/2013/12/14/prodigy-emacs-service-manager.html
(defun guanghui/post-init-prodigy ()
  (progn
    (prodigy-define-tag
      :name 'jekyll
      :env '(("LANG" "en_US.UTF-8")
             ("LC_ALL" "en_US.UTF-8")))
    ;; define service
    (prodigy-define-service
      :name "Preview cocos2d-x web"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6001")
      :cwd "~/cocos2d-x/web"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Preview cocos2d-html5"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6004")
      :cwd "~/Github/fireball/engine"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Server"
      :command "hexo"
      :args '("server")
      :cwd "~/4gamers.cn"
      :tags '(hexo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Deploy"
      :command "hexo"
      :args '("deploy" "--generate")
      :cwd "~/4gamers.cn"
      :tags '(hexo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Debug Fireball"
      :command "npm"
      :args '("start" "--" "--nologin" "/Users/guanghui/Github/example-cases")
      :cwd "~/Github/fireball/"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Org wiki preview"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "8088")
      :cwd "~/org-notes/public_html"
      :tags '(org-mode)
      :init (lambda () (browse-url "http://localhost:8088"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)))

(defun guanghui/init-moz-controller ()
  (use-package moz-controller
    :init
    (moz-controller-global-mode t)
    :diminish moz-controller-mode))
