(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(setq-default
 org-agenda-dir "~/org-notes"
 deft-dir "~/org-notes")

(setq org-blog-dir "~/4gamers.cn/")
(setq org-octopress-directory-top org-blog-dir)
(setq org-octopress-directory-posts (concat org-blog-dir "source/_posts"))
(setq org-octopress-directory-org-top org-blog-dir)
(setq org-octopress-directory-org-posts (concat org-blog-dir "blog"))
(setq org-octopress-setup-file (concat org-blog-dir "setupfile.org"))
