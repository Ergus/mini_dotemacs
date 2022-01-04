;;; init.el --- Emacs Initialization and Configuration
;; Copyright (C) 2018-2020 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

(setq-default package-quickstart t)

(setq-default display-line-numbers-widen t) ;; keep line numbers inside a narrow
(global-display-line-numbers-mode t)	;; line numbers on the left
(global-display-fill-column-indicator-mode t)

(savehist-mode t)			;; Historial
(auto-compression-mode t)		;; Uncompress on the fly

(size-indication-mode t)		;; Muestra el el tamanno en modeline
(delete-selection-mode t)		;; Sobreescribe seleccion al pegar

(prefer-coding-system 'utf-8)	        ;; Encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(column-number-mode t)			;; Numero de la columna
(line-number-mode t)			;; Numero de linea modeline

(save-place-mode 1)                           ;; Remember point in files
(setq-default save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
	      (replace-regexp-in-string "\\\\)\\$" "\\|^/tmp/.+\\)$"
					save-place-ignore-files-regexp t t))

(setq-default vc-follow-symlinks t	    ;; Open links not open
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

	      ;; split-width-threshold 160    ;; Limite para split vertical
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

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      suggest-key-bindings t          ;; enable show keybindings in completions

	      truncate-lines t
	      save-interprogram-paste-before-kill t ;; Save clipboard before replace
	      minibuffer-eldef-shorten-default t

	      ;; M-x show context-local commands
	      read-extended-command-predicate #'command-completion-default-include-p
	      completions-detailed t              ;; show more detailed completions
	      goto-line-history-local t         ;; Buffer local goto-line history

	      uniquify-buffer-name-style 'post-forward
	      switch-to-buffer-obey-display-actions t ;; switching the buffer respects display actions
	      bookmark-menu-confirm-deletion t    ;; ask confirmation to delete bookmark
	      bookmark-fontify t                  ;; Colorize bookmarked lines with bookmark-face

	      translate-upper-case-key-bindings nil ;; Make keybindings case sensitive
	      outline-minor-mode-use-buttons t      ;; Use buttons to hide/show outlines
	      ;; hideif mode
	      hide-ifdef-shadow t
	      hide-ifdef-initially t
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?\u2502))

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

(add-to-list 'load-path "~/.emacs.d/lisp/")

;;__________________________________________________________
;; The Colors (I want to change this for a real theme, there are maaaaany)

(load-theme 'simple-16)

(set-face-attribute 'default nil :family "Hack" :height 105)

;;__________________________________________________________
;; I don't want confirm exit, not write yes-not either
(setq-default read-file-name-completion-ignore-case t) ;; Ignore case in filename read

(if (< emacs-major-version 28)
    (progn
      (defalias 'yes-or-no-p 'y-or-n-p) ;; Reemplazar "yes" por "y" en el prompt
      (setq-default completion-auto-help 'lazy) ;; already default
      )

  ;; Functionalities for emacs >= 28
  (setq-default use-short-answers t    ;; use y-or-n
		;; native comp error not in minibuffer
		native-comp-async-report-warnings-errors 'silent
		bookmark-menu-confirm-deletion t    ;; ask confirmation to delete bookmark
		;;bookmark-fontify t                ;; Colorize bookmarked lines with bookmark-face
		completion-auto-help nil            ;; 'lazy completions on second tab
		repeat-check-key nil
		)

  (repeat-mode 1)                      ;; Repeat mode
  (fido-vertical-mode 1)
  (context-menu-mode 1)
  )


;;__________________________________________________________
;; Show paren mode
(setq-default auto-revert-verbose nil       ;; not show message when file changes
	      auto-revert-avoid-polling t   ;; use save signal
	      show-paren-delay 0            ;; Highlight couple parenthesis
	      blink-matching-paren nil
	      recentf-auto-cleanup 10)      ;; don't clean recentf on startup, but when idle

(run-with-idle-timer 1 nil (lambda ()
			     (global-auto-revert-mode t)
			     (show-paren-mode t)
			     (ffap-bindings)
			     (recentf-mode 1)))

;; Use cycle-spacing instead of just-one-space on M-SPC
(global-set-key [remap just-one-space] #'cycle-spacing)

;; profiler
(add-hook 'profiler-report-mode-hook #'hl-line-mode)


;;__________________________________________________________
;; Isearch

(setq-default search-nonincremental-instead nil    ;; No incremental if enter & empty
	      lazy-highlight-initial-delay 0
	      isearch-allow-scroll t 	           ;; Permit scroll can be 'unlimited
	      isearch-lazy-count t
	      search-ring-max 256
	      regexp-search-ring-max 256
	      isearch-yank-on-move 'shift          ;; Copy text from buffer with meta
	      ;; isearch-wrap-function #'ignore     ;; Look at the emacs-major-version check
	      ;; isearch-wrap-pause t               ;; Disable wrapping nil.
	      isearch-repeat-on-direction-change t ;; Don't go to the other end on direction change
	      isearch-regexp-lax-whitespace t      ;; swiper like fuzzy search
	      search-whitespace-regexp ".*?"
	      ;; Emacs version > 28
	      lazy-highlight-no-delay-length 1     ;; use this instead of lazy-highlight-initial-delay
	      isearch-allow-motion t
	      ;; isearch-motion-changes-direction t
	      )

(with-eval-after-load 'isearch
  (define-key isearch-mode-map
    [remap isearch-delete-char] #'isearch-del-char)

  (when (< emacs-major-version 28)
    ;; On emacs >= 28 isearch-allow-motion does this, so it is not needed.
    (define-key isearch-mode-map (kbd "M-<") #'isearch-beginning-of-buffer)
    (define-key isearch-mode-map (kbd "M->") #'isearch-end-of-buffer)
    )

  (defun my/isearch-exit-other-end ()
    (interactive)
    (when isearch-other-end
      (goto-char isearch-other-end))
    (call-interactively #'isearch-exit))

  (define-key isearch-mode-map (kbd "M-RET") #'my/isearch-exit-other-end)
  )


;;__________________________________________________________
;; imenu
(setq-default imenu-use-markers nil
	      imenu-auto-rescan t
	      imenu-max-item-length 256)

;;__________________________________________________________
;; ssh
(setq-default compilation-scroll-output 'first-error
	      compilation-always-kill t
	      tramp-auto-save-directory "~/.emacs.d/tramp-autosave-dir"
	      tramp-default-method "rsync"
	      ;;tramp-default-method "ssh"
	      ;;tramp-change-syntax 'simplified
	      tramp-use-ssh-controlmaster-options nil
	      remote-file-name-inhibit-cache 120
	      tramp-completion-reread-directory-timeout t
	      tramp-persistency-file-name "~/.emacs.d/tramp")
;;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

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
;; minibuffers

;; (setq minibuffer-eldef-shorten-default t)
(add-hook 'minibuffer-setup-hook #'my/unset-gc)

(add-hook 'minibuffer-exit-hook #'my/restore-gc)

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
      (setq-default mouse-wheel-scroll-amount '(1             ;; No modifier
						((control) . 5)
						((meta) . hscroll)
						((shift) . text-scale)) ;; in terminal does not work
		    mouse-wheel-tilt-scroll t          ;; horizontal scrolling with touchpad
		    mouse-wheel-progressive-speed nil
		    mouse-scroll-delay 0)
      (mouse-wheel-mode 1))                    ;; Explicit call mouse-wheel-mode AFTER setting mouse-wheel-scroll-amount

  ;; Else set them manually, will be overridden latter.
  (keymap-global-set "<mouse-4>" #'scroll-down-command)
  (keymap-global-set "<mouse-5>" #'scroll-up-command))

(global-set-key [remap scroll-up-command] #'scroll-up-line)
(global-set-key [remap scroll-down-command] #'scroll-down-line)
;;__________________________________________________________
;; Ediff
(setq-default ediff-window-setup-function #'ediff-setup-windows-plain
	      ediff-split-window-function #'split-window-horizontally)
(with-eval-after-load 'winner
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

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
  (setq-local show-trailing-whitespace t)
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
    (back-to-indentation)))

(define-minor-mode c-ms-space-for-alignment-mode
  "Enable indent with tabs align with spaces."
  :global nil
  :init-value nil
  (if c-ms-space-for-alignment-mode
      (when (and indent-tabs-mode
		 (= c-basic-offset tab-width))
	(add-hook 'c-special-indent-hook #'ms-space-for-alignment-hook nil t))
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
    (hide-ifdef-mode 1)
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

(define-key ctl-x-map "0" ctl-x-0-map)

(global-set-key [M-left] #'windmove-left)
(global-set-key [M-right] #'windmove-right)
(global-set-key [M-down] #'windmove-down)
(global-set-key [M-up] #'windmove-up)

(global-set-key [M-S-left] #'windmove-swap-states-left)
(global-set-key [M-S-right] #'windmove-swap-states-right)
(global-set-key [M-S-down] #'windmove-swap-states-down)
(global-set-key [M-S-up] #'windmove-swap-states-up)

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
;; winner
(setq-default winner-dont-bind-my-keys t)
(winner-mode t)
(define-key ctl-x-4-map "u"  #'winner-undo)
(define-key ctl-x-4-map "r"  #'winner-redo)

(with-eval-after-load 'repeat
  (defvar winner-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map "u" #'winner-undo)
      (define-key map "r" #'winner-redo)
      map)
    "Keymap to repeat winner commands.")
  (put #'winner-undo 'repeat-map 'winner-repeat-map)
  (put #'winner-redo 'repeat-map 'winner-repeat-map))

;;__________________________________________________________
;; Eldoc

(setq-default eldoc-idle-delay 2                   ;; default 0.5
	      eldoc-print-after-edit t             ;; only show after edit
	      eldoc-echo-area-display-truncation-message nil) ;; Not verbose when truncated

;;__________________________________________________________
;; Transpose
(define-key ctl-x-map [C-M-left] (lambda (arg) (interactive "*p") (transpose-words (- arg))))
(define-key ctl-x-map [C-M-right] #'transpose-words)
(define-key ctl-x-map [M-left] (lambda (arg) (interactive "*p") (transpose-chars (- arg))))
(define-key ctl-x-map [M-right] #'transpose-chars)

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
(setq-default dired-recursive-copies 'top   ;; Always ask recursive copy
	      dired-recursive-deletes 'top  ;; Always ask recursive delete
	      dired-dwim-target t	    ;; Copy in split mode with p
	      dired-auto-revert-buffer t
	      dired-listing-switches "-alh"
	      dired-kill-when-opening-new-dired-buffer t ;; only works for emacs > 28
	      )

;; Old alternative for dired-kill-when-opening-new-dired-buffer option.
(when (< emacs-major-version 28)
  (with-eval-after-load 'dired
    (require 'dired-x)
    (put 'dired-find-alternate-file 'disabled nil)
    (define-key dired-mode-map [remap dired-find-file] #'dired-find-alternate-file)  ; was dired-advertised-find-file
    (define-key dired-mode-map [remap dired-up-directory] ; was dired-up-directory
      (lambda nil
	(interactive)
	(find-alternate-file "..")))))

(provide 'init)
;;; init.el ends here
