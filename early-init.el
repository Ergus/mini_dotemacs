;;; early-init.el --- Emacs 27 early init file  -*- lexical-binding: t; -*-
;; Copyright (C) 2019 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:

;; Profiling since here when in debug-mode
(when init-file-debug
  (profiler-start 'cpu)
  (add-hook 'window-setup-hook #'profiler-stop 0))

(defconst file-name-handler-alist-save file-name-handler-alist)
(defconst my/gc-cons-threshold (* 2 gc-cons-threshold))

(defsubst my/unset-gc ()
  "Disable the gc."
  (setq gc-cons-threshold most-positive-fixnum   ;; Defer Garbage collection
	gc-cons-percentage 1.0))

(defsubst my/restore-gc ()
  "Restore the gc."
  (setq gc-cons-threshold my/gc-cons-threshold
	gc-cons-percentage 0.1))

(setq-default file-name-handler-alist nil
	      message-log-max 16384)
(my/unset-gc)

;; This hook is always added, set to 90 to go to the end
(add-hook 'window-setup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-save)
	    (my/restore-gc)
	    ;; (garbage-collect)
	    (let ((curtime (current-time)))
	      (message "Times: init:%.06f total:%.06f gc-done:%d"
		       (float-time (time-subtract after-init-time before-init-time))
		       (float-time (time-subtract curtime before-init-time))
		       gcs-done)))
	  90)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode   -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(menu-bar-mode   -1)
(tooltip-mode    -1)			;; Tool tip in the echo
(electric-indent-mode -1)

(provide 'early-init)
;;; early-init.el ends here
