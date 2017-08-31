;;; packages.el --- ts-tools layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: song <song@songdeMacBook-Pro.local>
;; URL: https://github.com/TinySong/spacemacs-private

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
;; added to `ts-tools-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ts-tools/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ts-tools/pre-init-PACKAGE' and/or
;;   `ts-tools/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ts-tools-packages
  '(
    (mysql :location  built-in)
    elfeed
    elfeed-goodies
    elfeed-org
    elfeed-web
    bug-hunter
    gnus
    ;; treemacs
    ;; calfw
    )
  "The list of Lisp packages required by the ts-tools layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


;;; packages.el ends here

(defun ts-tools/post-init-mysql ()
  (use-package sql-mysql)
  :init
  :config
  (progn
    (setq sql-connection-alist
          '((tenx
             (sql-product 'mysql)
             (sql-port 3306)
             (sql-server "192.168.0.227")
             (sql-user "tenxcloud")
             (sql-password "tenxcloud")
             (sql-database "tenxcloud_2_0")
             )))
    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (toggle-truncate-lines t)))

    )
  ;; (defun ts-tools/tenx-sql ()
  ;;   (interactive)
  ;;   (my-sql-connect 'mysql 'tenx)
  ;;   )
  ;; (defun my-sql-connect (product connection)
  ;;   ;; remember to set the sql-product, otherwise, it will fail for the first time
  ;;   ;; you call the function
  ;;   (setq sql-product product)
  ;;   (sql-connect connection))

  (defun sql-connect-preset (name)
    "Connect to a predefined SQL connection listed in `sql-connection-alist'"
    (eval `(let ,(cdr (assoc name sql-connection-alist))
             (flet ((sql-get-login (&rest what)))
               (sql-product-interactive sql-product)))))

  (defun ts-tools/tenx ()
    (interactive)
    (sql-connect-preset 'tenx))
  )


(defun ts-tools/init-elfeed ()
  (use-package elfeed
    :init (spacemacs/set-leader-keys "af" 'elfeed)
    :ensure t
    :commands elfeed
    :config
    (add-hook 'elfeed-new-entry-hook
              (elfeed-make-tagger :before "4 weeks ago"
                                  :remove 'unread))
    (setq elfeed-search-filter "@4-weeks-old +unread "
          elfeed-search-title-max-width 100)
    (setq rmh-elfeed-org-files (list "~/org-notes/elfeed.org"))
    (progn
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :eval-after-load elfeed-search
        :bindings
        "a"  'elfeed-search-update--force
        "A"  'elfeed-update
        "d"  'elfeed-unjam
        "o"  'elfeed-search-browse-url
        "j"  'next-line
        "k"  'previous-line
        "g"  'beginning-of-buffer
        "G"  'end-of-buffer
        "v"  'set-mark-command
        "<escape>" 'keyboard-quit
        )
      )

    (when (package-installed-p 'hydra)
      (bind-keys :map elfeed-search-mode-map
                 ("\\"   . hydra-elfeed-search/body))
      (bind-keys :map elfeed-show-mode-map
                 ("\\"   . hydra-elfeed-show/body))
      (with-eval-after-load 'hydra
        (progn
          (defhydra hydra-elfeed-common (:color blue)
            ("\\" hydra-master/body "back")
            ("<ESC>" nil "quit"))
          (defhydra hydra-elfeed-search (:hint nil :color blue :inherit (hydra-elfeed-common/heads))
            "
                                                                      ╭────────┐
  Move   Filter     Entries        Tags          Do                   │ Elfeed │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  _p_/_k_    [_s_] live   [_RET_] view     [_r_] read      [_a_] refresh
  ^ ^↑^ ^    [_S_] set    [_o_] browse     [_u_] unread    [_A_] fetch
  ^ ^ ^ ^    [_H_] hydra  [_y_] yank url   [_+_] add       [_d_] unjam
  ^ ^↓^ ^     ^ ^         [_v_] mark       [_-_] remove    [_E_] edit feeds
  _n_/_j_     ^ ^          ^ ^              ^ ^            [_q_] exit
--------------------------------------------------------------------------------
        "
            ("q"    quit-window)
            ("a"    elfeed-search-update--force)
            ("A"    elfeed-update)
            ("d"    elfeed-unjam)
            ("H"    ts-tools/make-and-run-elfeed-hydra)
            ("s"    elfeed-search-live-filter)
            ("S"    elfeed-search-set-filter)
            ("RET"  elfeed-search-show-entry)
            ("o"    elfeed-search-browse-url)
            ("y"    elfeed-search-yank)
            ("v"    set-mark-command)
            ("n"    next-line :color red)
            ("j"    next-line :color red)
            ("p"    previous-line :color red)
            ("k"    previous-line :color red)
            ("r"    elfeed-search-untag-all-unread)
            ("u"    elfeed-search-tag-all-unread)
            ("E"    (lambda() (interactive)(find-file "~/.emacs.d/elfeed.el.gpg")))
            ("+"    elfeed-search-tag-all)
            ("-"    elfeed-search-untag-all))
          (defhydra hydra-elfeed-show (:hint nil :color blue)
            "
                                                                      ╭────────┐
  Scroll       Entries        Tags          Links                     │ Elfeed │
╭─────────────────────────────────────────────────────────────────────┴────────╯
  _S-SPC_    _p_/_k_  [_g_] refresh   [_u_] unread    _S-TAB_
  ^  ↑  ^    ^ ^↑^ ^  [_o_] browse    [_+_] add       ^  ↑  ^
  ^     ^    ^ ^ ^ ^  [_y_] yank url  [_-_] remove    ^     ^
  ^  ↓  ^    ^ ^↓^ ^  [_q_] quit       ^ ^            ^  ↓  ^
   _SPC_     _n_/_j_  [_s_] quit & search^^            _TAB_
--------------------------------------------------------------------------------
        "
            ("q"     elfeed-kill-buffer)
            ("g"     elfeed-show-refresh)
            ("n"     elfeed-show-next :color red)
            ("j"     elfeed-show-next :color red)
            ("p"     elfeed-show-prev :color red)
            ("k"     elfeed-show-prev :color red)
            ("s"     elfeed-show-new-live-search)
            ("o"     elfeed-show-visit)
            ("y"     elfeed-show-yank)
            ("u"     (elfeed-show-tag 'unread))
            ("+"     elfeed-show-tag)
            ("-"     elfeed-show-untag)
            ("SPC"   scroll-up :color red)
            ("S-SPC" scroll-down :color red)
            ("TAB"   shr-next-link :color red)
            ("S-TAB" shr-previous-link :color red)))
        )
      )

    )
  )

(defun ts-tools/init-elfeed-goodies ()
  (use-package elfeed-goodies
    :defer t
    :commands elfeed-goodies/setup
    :init (spacemacs|use-package-add-hook elfeed
            :post-config (progn
                           (elfeed-goodies/setup)
                           (evil-define-key 'evilified elfeed-show-mode-map "o" 'elfeed-goodies/show-ace-link)))))

(defun ts-tools/init-elfeed-org ()
  (use-package elfeed-org
    :defer t
    :if (boundp 'rmh-elfeed-org-files)
    :init (spacemacs|use-package-add-hook elfeed
            :pre-config (elfeed-org))))

(defun ts-tools/init-elfeed-web ()
  (use-package elfeed-web
    :defer t
    :commands elfeed-web-stop
    :init (when elfeed-enable-web-interface
            ;; TODO check if the port is already in use
            ;; hack to force elfeed feature to be required before elfeed-search
            (require 'elfeed)
            (elfeed-web-start))))


(defun ts-tools/init-bug-hunter ()
  (use-package bug-hunter
    :ensure t
    :commands (bug-hunter-file bug-hunter-init-file))
  )


(defun ts-tools/init-calfw ()
  ;; Calfw program displays a calendar view in the Emacs buffer.
  (use-package calfw
    :commands cfw:open-org-calendar
    :defer 0.5
    :ensure t
    :config
    (progn
      (use-package calfw-org)
      ;; Unicode characters
      (setq cfw:fchar-junction ?╋
            cfw:fchar-vertical-line ?┃
            cfw:fchar-horizontal-line ?━
            cfw:fchar-left-junction ?┣
            cfw:fchar-right-junction ?┫
            cfw:fchar-top-junction ?┯
            cfw:fchar-top-left-corner ?┏
            cfw:fchar-top-right-corner ?┓)))
  )

(defun ts-tools/post-init-gnus ()
  (setq user-mail-address	"TinySong1226@gmail.com"
        user-full-name	"timmy song"

        ;; Get mail
        gnus-secondary-select-methods
        '((nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)))

        ;; TODO set flter rule: http://sachachua.com/blog/2008/05/emacs-gnus-organize-your-mail/
        ;; nnimap-split-methods '(
        ;;                        (""))
        ;; Send mail
        message-send-mail-function 'smtpmail-send-it

        ;; Archive outgoing email in Sent folder on imap.gmail.com
        gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail"

        ;; Auth
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                     "TinySong1226@gmail.com" nil))

        ;; SMPT Server config
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587

        ;; set return email address based on incoming email address
        gnus-posting-styles
        '(((header "to" "address@outlook.com")
           (address  "address@outlook.com"))
          ((header "to" "address@gmail.com")
           (address "address@gmail.com")))

        ;; store email in ~/gmail directory
        nnml-directory "~/gmail"
        message-directory "~/gmail"

        ;; Full size images
        mm-inline-large-images 'resize))


(defun ts-tools/init-treemacs ()
  (use-package treemacs
    :ensure t
    :defer t
    :config
    (setq treemacs-header-function            #'treemacs--create-header-projectile
          treemacs-follow-after-init          t
          treemacs-width                      25
          treemacs-indentation                2
          treemacs-git-integration            t
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    :bind
    (:map global-map
          ([f8]        . treemacs-toggle)
          ("<C-M-tab>" . treemacs-toggle)
          ("M-0"       . treemacs-select-window)
          ("C-c 1"     . treemacs-delete-other-windows)
          :map spacemacs-default-map
          ;; ("ft"    . treemacs)
          ("ft" . treemacs-toggle)
          ("fT"    . treemacs-projectile)
          ("f C-t" . treemacs-find-file)))

  )
