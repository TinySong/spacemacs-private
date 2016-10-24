(defconst ts-lang-packages
  '(
    (cc-mode :location built-in)
    company-c-headers
    google-c-style
    lispy
    lua-mode
    python
    ))

(defun ts-lang/post-init-cc-mode ()
  (progn

    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                       ; no additional indent
              ad-do-it)))               ; default behavior


    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (with-eval-after-load 'c++-mode
      (define-key c++-mode-map (kbd "s-.") 'company-ycmd)))
  ;; company backend should be grouped
  )

;; can add c-header list
(defun ts-lang/post-init-company-c-headers ()
  (progn
    (setq company-c-headers-path-system
          (quote
           ("/usr/include/" "/usr/local/include/")))
    (setq company-c-headers-path-user
          (quote
           (".")))))

(defun ts-lang/init-lispy ()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn
      (add-hook 'lispy-mode-hook 'spacemacs/toggle-aggressive-indent-on)
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
      )
    (with-eval-after-load 'lispy
      (progn
        (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
        (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline)

        ))

    ))

(defun ts-lang/post-init-lua-mode ()
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

;; http://blog.csdn.net/csfreebird/article/details/9250989
(defun ts-lang/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun ts-lang/post-init-python ()
  (use-package python
    :init
    (add-hook 'python-mode-hook
              (lambda ()
                (setq python-shell-interpreter "python")
                (setq anaconda-mode-server-script
                      "/usr/local/lib/python2.7/site-packages/anaconda_mode.py")))
    ))
