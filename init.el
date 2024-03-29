;;; init.el --- Emacs Initialization and Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

(prefer-coding-system 'utf-8)	        ;; Encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default vc-follow-symlinks t	    ;; Open links not open
	      vc-display-status nil
	      ;;tab-always-indent complete  ;; make tab key do indent only
	      ring-bell-function #'ignore
	      user-full-name "Ergus"
	      initial-scratch-message (format ";; Welcome %s!!" user-full-name)

	      inhibit-startup-message t
	      inhibit-startup-screen t
	      ;;tab-width 4		    ;; Tabulador a 4
	      ;;indent-tabs-mode t	    ;; Indent with tabs
	      ;;fill-column 80		    ;; default is 70
	      make-backup-files nil	    ;; Sin copias de seguridad
	      create-lockfiles nil	    ;; No lock files, good for tramp
	      visible-bell nil		    ;; Flash the screen (def)
	      confirm-kill-processes nil    ;; no ask kill processes on exit
	      ;; read-key-delay 0.01           ;; already default
	      recenter-redisplay nil
	      ;;recenter-positions '(top middle bottom)
	      ;; line-move-visual nil
	      backward-delete-char-untabify-method nil ;; Don't untabify on backward delete

	      split-width-threshold 140     ;; Limit for vertical split (default 160)
	      ;; kill-whole-line t
	      load-prefer-newer t
	      ;; mark-even-if-inactive nil    ;; no mark no region
	      mark-ring-max 128             ;; Max number of marks in the ring
	      set-mark-command-repeat-pop t ;; Repeat pop mark with C-SPC
	      next-screen-context-lines 5   ;; Lines of continuity when scrolling
	      fast-but-imprecise-scrolling t
	      scroll-error-top-bottom t	    ;; Move cursor before error scroll
	      scroll-preserve-screen-position t	  ;; Cursor keeps screen pos
	      scroll-margin 1		    ;; Margen al borde
	      scroll-step 1		    ;; Scroll step (better conservatively)
	      scroll-conservatively 1000
	      window-combination-resize t   ;; Windows resize proportional
	      x-wait-for-event-timeout nil  ;; Espera por eventos en X
	      jit-lock-stealth-load 60
	      jit-lock-stealth-time 4
	      inhibit-default-init t	    ;; Avoid emacs default init
	      term-suppress-hard-newline t  ;; Text can resize
	      echo-keystrokes 0.01	    ;; Muestra binds in echo area
	      confirm-kill-emacs nil        ;; No confirm exit emacs
	      disabled-command-function nil
	      auto-save-default nil         ;; No autosave
	      auto-save-list-file-name nil
	      ;; minibuffer interaction
	      minibuffer-message-timeout 1  ;; default 2
	      read-quoted-char-radix 16     ;; Read number of chars with C-q
	      ;; kill-buffer-query-functions nil
	      kill-do-not-save-duplicates t   ;; duplicate kill ring entries
	      kill-ring-max (* kill-ring-max 2)   ;; increase kill ring

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      enable-remote-dir-locals t      ;; Open remote dir locals.
	      suggest-key-bindings t          ;; enable show keybindings in completions

	      truncate-lines t
	      save-interprogram-paste-before-kill t ;; Save clipboard before replace
	      minibuffer-eldef-shorten-default t

	      goto-line-history-local t         ;; Buffer local goto-line history

	      uniquify-buffer-name-style 'post-forward
	      switch-to-buffer-obey-display-actions t ;; switching the buffer respects display actions
	      bookmark-menu-confirm-deletion t    ;; ask confirmation to delete bookmark
	      bookmark-fontify t                  ;; Colorize bookmarked lines with bookmark-face
	      bookmark-save-flag 1                ;; Save bookmarks immediately when added
	      idle-update-delay 0.25              ;; idle to update screen

	      ;; translate-upper-case-key-bindings nil ;; Make keybindings case sensitive
	      outline-minor-mode-use-buttons t      ;; Use buttons to hide/show outlines
	      ;; For when hide-if mode is enabled.
	      hide-ifdef-shadow t
	      hide-ifdef-initially t

	      help-window-select t                  ;; always select help windoes
	      help-window-keep-selected t           ;; Reuse *help* buffer when available
	      history-delete-duplicates t           ;; delete duplicates in commands history)

	      find-library-include-other-files nil  ;; find-library only shows libraries, not random files.
	      ;; Man
	      Man-notify-method 'pushy              ;; Man open links in same window

	      view-read-only t                      ;; buffers visiting files read-only do so in view mode
	      kill-read-only-ok t                   ;; don’t signal an error for killing read-only text.
	      debugger-stack-frame-as-list t        ;; display call stack frames as lists.
	      async-shell-command-display-buffer nil ;;command buffer wait until there is output

	      register-preview-delay 0.0            ;; for register view remvoe delay.
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?\u2502))

;;__________________________________________________________
;; minibuffers

;; These two must be enabled/disabled together
(setq-default ;; enable-recursive-minibuffers t     ;; Enable nesting in minibuffer
	      completion-show-help nil           ;; Don't show help in completion buffer
	      completion-auto-help 'lazy
	      completion-auto-select t
	      completion-wrap-movement t
	      completions-detailed t             ;; show more detailed completions
	      completions-format 'one-column     ;; Vertical completion list
	      completions-max-height 15
	      completion-styles '(substring partial-completion emacs22)
	      ;; M-x show context-local commands
	      read-extended-command-predicate #'command-completion-default-include-p
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      completion-ignore-case t)

;; These two must be enabled/disabled together
;; (setq-default enable-recursive-minibuffers t) ;; Enable nesting in minibuffer
;; (minibuffer-depth-indicate-mode 1)            ;; Mostrar nivel de nesting en minibuffer

;; (setq minibuffer-eldef-shorten-default t)
(add-hook 'minibuffer-setup-hook #'my/unset-gc)
(add-hook 'minibuffer-exit-hook #'my/restore-gc)

;; Arrows up/down search prefix in history like `history-search-backward' in bash
(define-key minibuffer-local-map [down] #'next-complete-history-element)
(define-key minibuffer-local-map [up] #'previous-complete-history-element)

(defun my/completion-setup-hook ()
  "My hook for Completions window."
  (with-current-buffer standard-output
    (setq-local mode-line-format nil)
    (display-line-numbers-mode -1)))

(add-hook 'completion-setup-hook #'my/completion-setup-hook 10)

;;__________________________________________________________
;; Config file not here to not track it
(setq-default custom-file
	      (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  (message "Creating %s" custom-file))
(load custom-file)

;; Personal Lisp dir
(defconst mylisp-dir (expand-file-name "lisp" user-emacs-directory))

(unless (file-exists-p mylisp-dir)
  (make-directory mylisp-dir)
  (message "Creating %s" mylisp-dir))

(add-to-list 'load-path mylisp-dir)

;;__________________________________________________________
;; mode-line
(setq-default mode-line-position-column-line-format '(" (%l,%C)")  ;; column number start on 1
              ;; mode-line-compact t                                  ;; no spaces on ml
	      mode-line-frame-identification " "                   ;; no F1 in term
              mode-line-front-space " "                            ;; no - on the very left
              mode-line-end-spaces " "                             ;; no ---- on the right.
              mode-line-mule-info ""                               ;; no UUU: on the left.
              )

;; Column in modeline
(column-number-mode 1)

;; Line numbers and fill column
(setq-default display-line-numbers-widen t)     ;; keep line numbers inside a narrow
(global-display-line-numbers-mode t)            ;; line numbers on the left
(global-display-fill-column-indicator-mode t)

;; Save history
(savehist-mode t)

;; Compress and delete selection
(auto-compression-mode t)               ;; Uncompress on the fly
(delete-selection-mode t)               ;; Override selection

;; saveplace
(save-place-mode 1)                           ;; Remember point in files
(setq save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
      (replace-regexp-in-string "\\\\)\\$" "\\|^/tmp/.+\\)$"
				save-place-ignore-files-regexp t t))

;;__________________________________________________________
;; The Colors (I want to change this for a real theme, there are maaaaany)

(load-theme 'simple-16)

(if (and (display-graphic-p)
	 (member "Hack" (font-family-list)))
    (set-face-attribute 'default nil :family "Hack" :height 105))

;;__________________________________________________________
;; I don't want confirm exit, not write yes-not either
(setq-default read-file-name-completion-ignore-case t) ;; Ignore case in filename read

(setq-default native-comp-async-report-warnings-errors 'silent
	      bookmark-menu-confirm-deletion t    ;; ask confirmation to delete bookmark
	      )

;; Repeat mode
(setq-default repeat-check-key nil
	      repeat-exit-key (kbd "RET"))

(defmacro my/repeat-keymap (keymap-name keymap &rest defs)
  "Generate a keymap as repeat-map and then add it to a another keymap."
  (declare (indent 2))
  `(with-eval-after-load 'repeat
     (defvar ,keymap-name (make-sparse-keymap)
       "Keymap to repeat winner commands.")
     ,@(let ((sets1) (sets2) (puts) (key) (val))
	 (while defs
	   (setq key (pop defs)
		 val (pop defs))
	   (push `(define-key ,keymap ,(kbd key) ,val) sets1)
	   (push `(define-key ,keymap-name ,(kbd key) ,val) sets2)
	   (push `(put ,val 'repeat-map ',keymap-name) puts))
	 (append sets1 sets2 puts))))

(when (fboundp 'repeat-mode)
  (repeat-mode 1))

(my/repeat-keymap my/next-prev-repeat-map ctl-x-map
  "C-<left>" #'previous-buffer
  "C-<right>" #'next-buffer)

(when (fboundp 'context-menu-mode)
  (context-menu-mode 1))

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (defalias 'yes-or-no-p 'y-or-n-p))

;;__________________________________________________________
;; dabbrev
(setq-default dabbrev-check-all-buffers nil
	      dabbrev-ignored-buffer-regexps "^[ *]")


;;__________________________________________________________
;; xref
(with-eval-after-load 'xref
  (setq-default xref-search-program 'ripgrep
		xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom
		xref-show-xrefs-function #'xref-show-definitions-buffer-at-bottom)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

;;__________________________________________________________
;; Show paren mode
(setq-default auto-revert-verbose nil             ;; not show message when file changes
	      auto-revert-avoid-polling t         ;; use save signal
	      show-paren-delay 0                  ;; Highlight couple parenthesis
	      show-paren-context-when-offscreen t ;; show context in the echo area
	      blink-matching-paren nil
	      recentf-max-saved-items 48     ;; Max items saved
	      recentf-auto-cleanup nil
	      ffap-machine-p-known 'accept        ;; stop ffap from pinging random hosts
	      )
(run-with-idle-timer 1 nil (lambda ()
			     (global-auto-revert-mode t)
			     (show-paren-mode t)
			     (ffap-bindings)
			     (recentf-mode 1)
			     (recentf-cleanup)))

;; Use cycle-spacing instead of just-one-space on M-SPC
(when (version< emacs-version "29")
  (global-set-key [remap just-one-space] #'cycle-spacing))
(global-set-key [remap delete-char] #'delete-forward-char)
(global-set-key [remap count-words-region] #'count-words)  ;; count on whole file or region if active

;; profiler
(add-hook 'profiler-report-mode-hook #'hl-line-mode)


;;__________________________________________________________
;; Isearch

(setq-default search-nonincremental-instead nil    ;; No incremental if enter & empty
	      lazy-highlight-no-delay-length 1     ;; normal delay
	      ;; lazy-highlight-initial-delay 0
	      isearch-allow-scroll t 	           ;; Permit scroll can be 'unlimited
	      isearch-lazy-count t
	      search-ring-max 256
	      regexp-search-ring-max 256
	      isearch-yank-on-move 'shift          ;; Copy text from buffer with meta
	      ;; isearch-wrap-function #'ignore     ;; Look at the emacs-major-version check
	      ;; isearch-wrap-pause t               ;; Disable wrapping nil.
	      isearch-repeat-on-direction-change t ;; Don't go to the other end on direction change
	      isearch-lax-whitespace t
	      isearch-regexp-lax-whitespace t      ;; swiper like fuzzy search
	      search-whitespace-regexp ".*?"
	      ;; Emacs version > 28
	      lazy-highlight-no-delay-length 1     ;; use this instead of lazy-highlight-initial-delay
	      isearch-allow-motion t
	      isearch-forward-thing-at-point '(region symbol sexp word)
	      ;; isearch-motion-changes-direction t
	      )

(with-eval-after-load 'isearch
  (defun my/isearch-exit-other-end ()
    (interactive)
    (when isearch-other-end
      (goto-char isearch-other-end))
    (call-interactively #'isearch-exit))

  (define-key isearch-mode-map (kbd "C-<return>") #'my/isearch-exit-other-end)
  (define-key isearch-mode-map [remap isearch-abort] #'isearch-cancel)
  (define-key isearch-mode-map [remap isearch-delete-char] #'isearch-del-char)
  (define-key search-map "." #'isearch-forward-thing-at-point))

;;__________________________________________________________
;; Occur
(setq-default list-matching-lines-jump-to-current-line t)

(with-eval-after-load 'replace  ;; is where occur resides
  ;; With error follow this is pointless.
  (define-key occur-mode-map (kbd "SPC") #'occur-mode-display-occurrence)
  (add-hook 'occur-hook (lambda ()
			  (beginning-of-line)
			  (recenter nil t)))

  (add-hook 'occur-mode-hook (lambda nil
			       (hl-line-mode 1)
			       (display-line-numbers-mode -1)
			       (switch-to-buffer-other-window "*Occur*")
			       ;; (next-error-follow-minor-mode 1)
			       ))

  (add-hook 'occur-mode-find-occurrence-hook
	    (lambda nil
	      (let ((win (get-buffer-window "*Occur*")))
		(when (and win
			   (eq this-command #'occur-mode-goto-occurrence))
		  (quit-restore-window win)
		  (isearch-done))))))

;;__________________________________________________________
;; imenu
(setq-default imenu-use-markers nil
	      imenu-auto-rescan t
	      imenu-max-item-length 256)
;;__________________________________________________________
;; ssh

(setq-default compilation-scroll-output 'first-error
	      compilation-always-kill t)


(setq-default tramp-auto-save-directory
	      (expand-file-name "tramp-autosave-dir" user-emacs-directory)
	      tramp-default-method "scp"                     ;; default scp
	      ;; tramp-change-syntax 'simplified
	      remote-file-name-inhibit-cache 60              ;; default 10
	      tramp-completion-reread-directory-timeout 120  ;; default 10
	      password-cache-expiry 3600                     ;; cache passwords for 1 hour
	      tramp-use-scp-direct-remote-copying t          ;; copy directly between remote hosts
	      tramp-verbose (if init-file-debug 10 3)        ;; Default 3 always
	      ;; tramp-persistency-file-name "~/.emacs.d/tramp" ;; already default
	      tramp-use-ssh-controlmaster-options nil
	      tramp-completion-use-auth-sources nil          ;; no use auth
	      )

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-process-environment
               (format "DISPLAY=%s" (getenv "DISPLAY"))))

(with-eval-after-load 'term
  (defun my/term-mode-hook ()
    "My term mode hook."
    (setq-local mouse-yank-at-point t
		transient-mark-mode nil)
    (display-fill-column-indicator-mode -1)
    (auto-fill-mode -1))

  (add-hook 'term-mode-hook #'my/term-mode-hook))

;;__________________________________________________________
;; tabs and tabbar
(setq-default tab-bar-tab-hints t  ;; show tab numbers
	      tab-bar-close-last-tab-choice 'tab-bar-mode-disable
	      tab-bar-show 1)

;;__________________________________________________________
;; gdb rectangles
(setq-default gdb-many-windows nil
	      gdb-show-main t)

;;__________________________________________________________
;; ispell
(setq-default ispell-following-word t
	      ispell-quietly t)

;;__________________________________________________________
;; xterm mouse
(unless (or (display-graphic-p)
	    (string-equal (getenv "TERM") "linux"))
  (xterm-mouse-mode t))

(if (fboundp 'mouse-wheel-mode)
    (progn
      (setq-default mouse-wheel-scroll-amount '(3             ;; No modifier
						((control) . 6)
						((meta) . hscroll)
						((shift) . text-scale)) ;; in terminal does not work
		    mouse-wheel-tilt-scroll t          ;; horizontal scrolling with touchpad
		    mouse-wheel-progressive-speed nil
		    mouse-scroll-delay 0)
      (mouse-wheel-mode 1))                    ;; Explicit call mouse-wheel-mode AFTER setting mouse-wheel-scroll-amount

  ;; Else set them manually, will be overridden latter.
  (global-set-key [mouse-4] #'scroll-down-command)
  (global-set-key [mouse-5] #'scroll-up-command))

(global-set-key [remap scroll-up-command] #'scroll-up-line)
(global-set-key [remap scroll-down-command] #'scroll-down-line)
;;__________________________________________________________
;; Ediff
(setq-default ediff-window-setup-function #'ediff-setup-windows-plain
	      ediff-split-window-function #'split-window-horizontally)
(eval-after-load 'winner
  '(add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;;__________________________________________________________
;; smerge
(setq-default smerge-diff-buffer-name "*smerge-diff*"
	      smerge-command-prefix "\C-cs")

(defun my/enable-smerge-maybe ()
  "Auto-enable `smerge-mode' when merge conflict is detected."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook #'my/enable-smerge-maybe)

;;__________________________________________________________
;; My program's mode hooks

;; elec-pair
(eval-after-load 'elec-pair
  '(add-to-list 'electric-pair-pairs '(?< . ?>) t))

(defun my/common-hook ()
  "Enable electric-pair-local-mode"
  (setq-local show-trailing-whitespace t  ;; Show trailing whitespaces
	      indicate-empty-lines t)      ;; Show empty lines at end of file)
  (electric-pair-local-mode 1))

(add-hook 'prog-mode-hook #'my/common-hook)
(add-hook 'text-mode-hook #'my/common-hook)
(add-hook 'conf-mode-hook #'my/common-hook)

(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (call-interactively 'back-to-indentation)
    (and (<= oldpos (point))
	 (/= (line-beginning-position) oldpos)
	 (call-interactively 'beginning-of-line))))

(global-set-key [remap move-beginning-of-line] #'my/smart-beginning-of-line)

;;__________________________________________________________
;; C common mode (for all c-like languages)
;; This is a minor mode to do indent with tabs and align with spaces.

(defun ms-space-for-alignment-hook ()
  "Make the current line use tabs for indentation and spaces for alignment.

It is intended to be called from the hook
`c-special-indent-hook'.  It assumes that `indent-tabs-mode' is
non-nil and probably assumes that `c-basic-offset' is the same as
`tab-width'."
  (when (and indent-tabs-mode
	     (= c-basic-offset tab-width))
    (save-excursion
      (let* ((indent-pos (progn (back-to-indentation) (point)))
	     (indent-col (current-column))
	     (syn-elt (car c-syntactic-context))
	     (syn-sym (c-langelem-sym syn-elt)))
	(when (memq syn-sym '(arglist-cont-nonempty
			      stream-op
			      template-args-cont)) ;; <==============
	  (let* ((syn-anchor (c-langelem-pos syn-elt))
		 (anchor-col (progn (goto-char syn-anchor)
				    (back-to-indentation)
				    (current-column))))
	    ;;
	    (goto-char indent-pos)
	    (delete-horizontal-space)
	    (insert-char ?\t (/ anchor-col tab-width))
	    (insert-char ?\  (- indent-col (current-column)))))))
    (when (= (current-column) 0)
      (back-to-indentation))))

(define-minor-mode c-ms-space-for-alignment-mode
  "Enable indent with tabs align with spaces."
  :global nil
  :init-value nil
  (if c-ms-space-for-alignment-mode
      (add-hook 'c-special-indent-hook #'ms-space-for-alignment-hook nil t)
    (remove-hook 'c-special-indent-hook #'ms-space-for-alignment-hook t)))

;;====================
;; cc-mode
(setq-default c-default-style '((java-mode . "java")
				(awk-mode . "awk")
				(other . "mylinux")))

(with-eval-after-load 'cc-mode
  (c-add-style "mylinux"
	       '("linux"
		 (tab-width . 4)
		 (c-basic-offset . 4)
		 (indent-tabs-mode . t)
		 (fill-column . 80)
		 ;; (c-hanging-semi&comma-criteria my/c-semi&comma)
		 (c-hanging-semi&comma-criteria . nil)
		 (c-cleanup-list empty-defun-braces ;; {}
				 brace-else-brace   ;; } else {
				 brace-elseif-brace ;; } else if {
				 defun-close-semi   ;; }; after class
				 )
		 (c-hanging-braces-alist (defun-open before after)
					 (brace-list-open)
					 (brace-entry-open)
					 (substatement-open after)
					 (namespace-open after)
					 (namespace-close before)
					 (block-close . c-snug-do-while)
					 (arglist-cont-nonempty)
					 (class-open after)
					 (class-close before))
		 (c-offsets-alist (inline-open . 0)
				  (comment-intro . 0)
				  (arglist-close . 0)
				  ;;(innamespace . [0])
				  ;;(access-label '-)
				  )))

  (defun my/c-mode-common-hook ()
    "my/c-mode-common common."
    (c-toggle-auto-newline 1)
    (c-toggle-cpp-indent-to-body 1)
    (c-ms-space-for-alignment-mode 1)
    ;; (hide-ifdef-mode 1)
    (subword-mode 1))

  (add-hook 'c-mode-common-hook #'my/c-mode-common-hook))

;;__________________________________________________________
;; sh mode
(defvaralias 'sh-basic-offset 'tab-width)

(add-hook 'sh-mode-hook (lambda nil
			  (setq-local indent-tabs-mode t
				      tab-width 4)))
;;__________________________________________________________
;; Move split keybindings
(setq-default windmove-display-no-select t)

(easy-mmode-defmap ctl-x-0-map
  `(("0" . delete-window)
    ([left] . windmove-delete-left)
    ([right] . windmove-delete-right)
    ([up] . windmove-delete-up)
    ([down] . windmove-delete-down))
  "The base keymap for `highlight changes'.")

(global-set-key (kbd "C-x 0") ctl-x-0-map)

(define-key ctl-x-map [left] #'windmove-left)
(define-key ctl-x-map [right] #'windmove-right)
(define-key ctl-x-map [down] #'windmove-down)
(define-key ctl-x-map [up] #'windmove-up)

(define-key ctl-x-map [S-left] #'windmove-swap-states-left)
(define-key ctl-x-map [S-right] #'windmove-swap-states-right)
(define-key ctl-x-map [S-down] #'windmove-swap-states-down)
(define-key ctl-x-map [S-up] #'windmove-swap-states-up)

(define-key ctl-x-4-map [left] #'windmove-display-left)
(define-key ctl-x-4-map [right] #'windmove-display-right)
(define-key ctl-x-4-map [up] #'windmove-display-up)
(define-key ctl-x-4-map [down] #'windmove-display-down)

;;__________________________________________________________
;; Undo and redo
(setq-default undo-only t)                  ;; undo does not go throw redo entries
(global-set-key (kbd "C-M-/") #'undo-redo)  ;; for gui in tty "C-M-/" == "C-M-_"

;;__________________________________________________________
;; Winner mode
(setq-default winner-dont-bind-my-keys t)
(winner-mode t)

(my/repeat-keymap winner-repeat-map ctl-x-4-map
  "u"  #'winner-undo
  "r"  #'winner-redo)

;;__________________________________________________________
;; Eldoc

(setq-default eldoc-idle-delay 2                   ;; default 0.5
	      eldoc-print-after-edit t             ;; only show after edit
	      eldoc-echo-area-display-truncation-message nil) ;; Not verbose when truncated

;;__________________________________________________________
;; Transpose (REINSERT)

(when (boundp 'ctl-x-map)
  (defun my/untranspose-words (arg)
    (interactive "*p")
    (transpose-words (- arg)))

  (defun my/untranspose-chars (arg)
    (interactive "*p")
    (transpose-chars (- arg)))

  (my/repeat-keymap transpose-repeat-map ctl-x-map
    "C-M-<left>" #'my/untranspose-words
    "C-M-<right>" #'transpose-words
    "M-<left>" #'my/untranspose-chars
    "M-<right>" #'transpose-chars))

;;__________________________________________________________
;; Abbrev mode
(abbrev-mode t)

;;__________________________________________________________
;; ibuffer
;;(defalias 'list-buffers 'ibuffer)
(global-set-key [remap list-buffers] #'ibuffer)
(setq-default ibuffer-default-sorting-mode 'alphabetic)

(add-hook 'ibuffer-mode-hook #'hl-line-mode 1)

;;__________________________________________________________
;; dired

;; Old alternative for dired-kill-when-opening-new-dired-buffer option.
(setq-default dired-recursive-copies 'top   ;; Always ask recursive copy
	      dired-recursive-deletes 'top  ;; Always ask recursive delete
	      dired-dwim-target t	    ;; Copy in split mode with p
	      dired-auto-revert-buffer t
	      dired-listing-switches "-alh"
	      dired-kill-when-opening-new-dired-buffer t ;; only works for emacs > 28
	      dired-mouse-drag-files t
	      dired-isearch-filenames 'dwim
	      dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open")
					     ("\\.jpe?g\\'" "xdg-open")
					     ("\\.png\\'" "xdg-open")
					     ("\\.gif\\'" "xdg-open")))

(with-eval-after-load 'dired
  (require 'dired-x)
  (define-key dired-mode-map [mouse-2] #'dired-mouse-find-file)
  (add-hook 'dired-mode-hook #'hl-line-mode))

(provide 'init)
;;; init.el ends here
