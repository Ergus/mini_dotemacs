;;; early-init.el --- Emacs 27 early init file
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

(defconst my/start-time (current-time))

(defmacro mt (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold most-positive-fixnum   ;; Defer Garbage collection
      gc-cons-percentage 1.0)

(add-hook 'window-setup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)
	    (garbage-collect)
	    (message "Load time %.06f" (float-time (time-since my/start-time))))
	  t)

(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)			;; Tool tip in the echo
(electric-indent-mode -1)
;;(flymake-mode    -1)

;;(setq-default package-enable-at-startup nil)
;;-------------------- Some tricks--------------------------

(provide 'early-init)
;;; early-init.el ends here
