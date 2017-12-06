(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")
(defvar org-note-dir ""
  "deft org note locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(setq-default
 org-agenda-dir "~/org-notes"
 org-note-dir "~/hugo-blog/org-blog"
 deft-dir "~/org-notes")

(setq blog-dir "~/hugo-blog"
      public-blog-dir "~/hugo-blog/layout"
      hugo-process "Hugo Server"
      hugo-server-site "http://localhost:1313/")

(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in DIR."
  (let ((orig-dir (gensym)))
    `(progn (setq ,orig-dir default-directory)
            (cd ,DIR) ,@FORMS (cd ,orig-dir))))

(defun deploy-blog ()
  "Run hugo and push changes upstream."
  (interactive)
  (with-dir public-blog-dir
            (shell-command "git rm -rf .")
            (shell-command "git clean -fxd")

            (with-dir blog-dir (->> public-blog-dir
                                    (concat "hugo -d ")
                                    shell-command))

            (shell-command "git add .")
            (--> (current-time-string)
                 (concat "git commit -m \"" it "\"")
                 (shell-command it))
            (magit-push-current-to-upstream nil)))

(defun start-blog-server ()
  "Run hugo server if not already running and open its webpage."
  (interactive)
  (with-dir blog-dir
            (unless (get-process hugo-process)
              (start-process hugo-process nil "hugo" "server"))
            (browse-url hugo-server-site)))

(defun end-blog-server ()
  "End hugo server process if running."
  (interactive)
  (--when-let (get-process hugo-process)
    (delete-process it)))

(spacemacs/set-leader-keys (kbd "ab") 'deploy-blog)
(spacemacs/set-leader-keys (kbd "aa") 'start-blog-server)
(spacemacs/set-leader-keys (kbd "ae") 'end-blog-server)
