;; ---
(defun ts-tools/has-cap (s)
  ""
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" s)
    ))


(defun ts-tools/get-hydra-option-key (s)
  "returns single upper case letter (converted to lower) or first"
  (interactive)
  (let ( (loc (ts-tools/has-cap s)))
    (if loc
        (downcase (substring s loc (+ loc 1)))
      (substring s 0 1)
      )))


(defun ts-tools/make-elfeed-cats (tags)
  "Returns a list of lists. Each one is line for the hydra configuratio in the form
     (c function hint)"
  (interactive)
  (mapcar (lambda (tag)
            (let* (
                   (tagstring (symbol-name tag))
                   (c (ts-tools/get-hydra-option-key tagstring))
                   )
              (list c (append '(elfeed-search-set-filter) (list (format "@6-months-ago +%s" tagstring) ))tagstring  )))
          tags))

;; (defalias 'elfeed-toggle-star
;;   (elfeed-expose #'elfeed-search-toggle-all 'star))

(defmacro ts-tools/make-elfeed-hydra ()
  (with-eval-after-load 'elfeed
    `(defhydra hydra-elfeed ()
       "filter"
       ,@(ts-tools/make-elfeed-cats (elfeed-db-get-all-tags))
       ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
       ("M" elfeed-toggle-star "Mark")
       ("A" (elfeed-search-set-filter "@6-months-ago") "All")
       ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
       ("q" ts-tools/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
       ;; ("q" nil "quit" :color blue)
       )
    )
  )

(defun ts-tools/make-and-run-elfeed-hydra ()
  ""
  (interactive)
  (ts-tools/make-elfeed-hydra)
  (hydra-elfeed/body)
  )

(defun ts-tools/elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun tinysong/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))


(defun tinysong/insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))


(defun tinysong/insert-current-date ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun tinysong/insert-author ()
  (interactive)
  "get author command"
  (insert user-full-name))



(require 'cl-lib)

(defun save-chromium-session ()
  "Reads chromium current session and generate org-mode heading with items."
  (interactive)
  (save-excursion
    (let* ((cmd "strings ~/'Library/Application Support/Google/Chrome/Default/Current Session' | 'grep' -E '^https?://' | sort | uniq")
           (ret (shell-command-to-string cmd)))
      (insert
       (concat
        "* "
        (format-time-string "[%Y-%m-%d %H:%M:%S]")
        "\n"
        (mapconcat 'identity
                   (cl-reduce (lambda (lst x)
                                (if (and x (not (string= "" x)))
                                    (cons (concat "  - " x) lst)
                                  lst))
                              (split-string ret "\n")
                              :initial-value (list))
                   "\n"))))))

(defun restore-chromium-session ()
  "Restore session, by openning each link in list with (browse-url).
Make sure to put cursor on date heading that contains list of urls."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\*")
      (forward-line 1)
      (while (looking-at "^[ ]+-[ ]+\\(http.?+\\)$")
        (let* ((ln (thing-at-point 'line t))
               (ln (replace-regexp-in-string "^[ ]+-[ ]+" "" ln))
               (ln (replace-regexp-in-string "\n" "" ln)))
          (browse-url ln))
        (forward-line 1)))))
