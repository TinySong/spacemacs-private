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
