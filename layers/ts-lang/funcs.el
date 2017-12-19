;;; funcs.el --- tinysong Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 tinysong
;;
;; Author: tinysong <guanghui8827@gmail.com>
;; URL: https://github.com/tinysong/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl-lib)
(require 'ido)

;;  reload GOPATH for go
(defun ts-lang/load-docker-gopath ()
  (interactive)
  (progn
    (setq gopath (getenv "GOPATH")
          vendor (concat  gopath "/src/github.com/docker/docker/vendor"))
    (setenv "GOPATH" (concat gopath ":" vendor)))
  )

;;  reload GOPATH for go
(defun ts-lang/load-kubernetes-gopath ()
  (interactive)
  (progn
    (setq gopath "~/development/golang"
          vendor (concat  gopath "/src/github.com/kubernetes/kubernetes/vendor"))
    (setenv "GOPATH" (concat gopath ":" vendor)))
  )

(defun ts-lang/insert-go-coments ()
  (interactive)
  (insert (setq comments (concat
                          "// @Title \n"
                          "// @Description \n"
                          "// @Param   key     path    string  true        \"description\"\n"
                          "// @Success 200 success\n"
                          "// @Success 400 invalid request body\n"
                          "// @Failure 403 no authorization\n"
                          "// @Failure 500 internal server error\n"
                          "// @router / [get]\n"
                          ))))
