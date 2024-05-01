;;; init.el --- Emacs Initialization and Configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; Version: 0.1
;; Package-Requires: (())
;; Keywords:
;; URL:
;;; Commentary:
;;; Code:

;;__________________________________________________________
;; Internal Options

(prefer-coding-system 'utf-8)	        ;; Encoding
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq-default ;; tab-always-indent 'complete   ;; make tab key do indent only
	      ;; tab-first-completion 'word
	      ring-bell-function #'ignore
	      user-full-name "Ergus"
	      initial-scratch-message (format ";; Welcome %s!!" user-full-name)

	      inhibit-startup-message t
	      inhibit-startup-screen t
	      ;;tab-width 4                 ;; Tabulador a 4
	      ;;indent-tabs-mode t          ;; Indent with tabs
	      ;;fill-column 80              ;; default is 70
	      make-backup-files nil         ;; Sin copias de seguridad
	      create-lockfiles nil          ;; No lock files, good for tramp
	      visible-bell nil              ;; Flash the screen (def)
	      confirm-kill-processes nil    ;; no ask kill processes on exit
	      ;; read-key-delay 0.01           ;; already default
	      recenter-redisplay nil
	      ;;recenter-positions '(top middle bottom)
	      ;; line-move-visual nil       ;; move cursor visual lines
	      backward-delete-char-untabify-method nil ;; Don't untabify on backward delete

	      split-width-threshold 140     ;; Limit for vertical split (default 160)
	      ;; kill-whole-line t
	      load-prefer-newer t
	      ;; mark-even-if-inactive nil     ;; no mark no region
	      mark-ring-max 128             ;; Max number of marks in the ring
	      set-mark-command-repeat-pop t ;; Repeat pop mark with C-SPC
	      next-screen-context-lines 5   ;; Lines of continuity when scrolling
	      fast-but-imprecise-scrolling t
	      redisplay-skip-fontification-on-input t ;; Skip ‘fontification_functions‘ when there is input pending.
	      jit-lock-defer-time 0                   ;; similar to redisplay-skip-fontification-on-input
	                                              ;; This should make input smoother

	      x-stretch-cursor t                  ;; Draw cursor as wide as the gliph below
	      scroll-error-top-bottom t           ;; Move cursor before error scroll
	      scroll-preserve-screen-position t   ;; Cursor keeps screen pos
	      scroll-margin 1                     ;; Margen al borde
	      scroll-step 1                       ;; Scroll step (better conservatively)
	      scroll-conservatively 1000
	      window-combination-resize t         ;; Windows resize proportional
	      x-wait-for-event-timeout nil        ;; Not wait for events in X (when built with X)
	      pgtk-wait-for-event-timeout nil     ;; Not wait for events in pgtk
	      ;; jit-lock-stealth-load 60           ;; load of fontification (def: 200)
	      jit-lock-stealth-nice 0.2           ;; Time between fortifications (def: 0.5)
	      jit-lock-stealth-time 2             ;; Time to wait before fortifications (def: nil)
	      inhibit-default-init t              ;; Avoid emacs default init
	      term-suppress-hard-newline t        ;; Text can resize
	      echo-keystrokes 0.001                ;; Unfinished bindings in the echo area
	      confirm-kill-emacs nil              ;; No confirm exit emacs
	      disabled-command-function nil
	      auto-save-default nil               ;; No autosave
	      auto-save-list-file-name nil
	      ;; minibuffer interaction
	      ;; minibuffer-message-timeout 1     ;; default 2
	      read-quoted-char-radix 16           ;; Read number of chars with C-q
	      ;; kill-buffer-query-functions nil     ;; Functions to call before quering a buffer (nil default)
	                                          ;; Default asks if process running.
	      kill-do-not-save-duplicates t       ;; duplicate kill ring entries
	      kill-ring-max (* kill-ring-max 2)   ;; increase kill ring

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      enable-remote-dir-locals t              ;; Open remote dir locals in remote files.

	      suggest-key-bindings t                  ;; Ivy ya hace lo que esta opcion
	      truncate-lines t
	      ;; auto-hscroll-mode 'current-line         ;; scroll horizontally 1 line not all
	      save-interprogram-paste-before-kill t   ;; Save clipboard before replace
	      minibuffer-eldef-shorten-default t

	      ;; M-x show context-local commands
	      use-short-answers t                     ;; Use y or n to exit and other shorter answers.
	      ;; y-or-n-p-use-read-key t                 ;; use readkey and not minibuffer for y or n answers
	      goto-line-history-local t               ;; Buffer local goto-line history
	      switch-to-buffer-obey-display-actions t ;; switching the buffer respects display actions
	      bookmark-menu-confirm-deletion t        ;; ask confirmation to delete bookmark
	      bookmark-fontify t                      ;; Colorize bookmarked lines with bookmark-face
	      bookmark-save-flag 1                    ;; Save bookmarks immediately when added

	      register-use-preview t                  ;; newer interface to show registers
	      idle-update-delay 0.25                  ;; idle to update screen

	      ;; translate-upper-case-key-bindings nil ;; Make keybindings case sensitive (inhibit binding translation)
	      outline-minor-mode-use-buttons t      ;; Use buttons to hide/show outlines

	      help-window-select t                  ;; always select help windoes
	      help-window-keep-selected t           ;; Reuse *help* buffer when available
	      history-delete-duplicates t           ;; delete duplicates in commands history
	      history-length 200
	      find-library-include-other-files nil  ;; find-library only shows libraries, not random files.
	      view-read-only t                      ;; buffers visiting files read-only do so in view mode
	      kill-read-only-ok t                   ;; don’t signal an error for killing read-only text.
	      debugger-stack-frame-as-list t        ;; display call stack frames as lists.
	      async-shell-command-display-buffer nil ;;command buffer wait until there is output
	      shell-kill-buffer-on-exit t
	      large-file-warning-threshold nil
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?\u2502))  ;; also works 2503, it is wider
;;__________________________________________________________
;; use-package

(setq-default package-native-compile t
	      package-quickstart t)

(defmacro my/repeat-keymap (keymap-name keymap &rest defs)
  "Generate a keymap as repeat-map and then add it to a another keymap.

This is inteded to add the same bindings to a keymap like `C-x
M-<left>' and repeat with M-<left>."
  (declare (indent 2))
  `(progn
     ;; This also takes care of an even number of arguments
     (defvar-keymap ,keymap-name ,@defs)
     ,@(let ((sets) (puts) (key) (val))
	 (while defs
	   (setq key (pop defs)
		 val (pop defs))

	   (unless (eq key :doc)
	     (push `(keymap-set ,keymap ,key ,val) sets)
	     (push `(put ,val 'repeat-map ',keymap-name) puts)))
	 (append sets puts))))

(if init-file-debug
    (setq-default debug-on-error t
		  native-comp-async-report-warnings-errors t)

  (setq-default native-comp-async-report-warnings-errors 'silent))

(defun my/find-init ()
  "Open the init file to edit it."
  (interactive)
  (find-file user-init-file))

;;__________________________________________________________
;; Config file not here to not track it
(setq-default custom-file
	      (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  (message "Creating %s" custom-file))
(load custom-file)

;;__________________________________________________________
;; Some internal packages to defer them

;; Modeline
(setq-default mode-line-position-column-line-format '(" (%l,%C)")  ;; column number start on 1
              ;; mode-line-compact t                                  ;; no spaces on ml
	      mode-line-frame-identification " "                   ;; no F1 in term
              mode-line-front-space " "                            ;; no - on the very left
              mode-line-end-spaces " "                             ;; no ---- on the right.
              mode-line-mule-info "")                               ;; no UUU: on the left.

(column-number-mode t)               ;; Column number
(line-number-mode t)                 ;; Line number
(size-indication-mode t)             ;; Size in modeline

;; Line numbers and fill column
(setq-default display-line-numbers-widen t)     ;; keep line numbers inside a narrow
(global-display-line-numbers-mode t)            ;; line numbers on the left
(global-display-fill-column-indicator-mode t)

;; Save history
(savehist-mode t)

;; Compress and delete selection
(auto-compression-mode t)               ;; Uncompress on the fly
(delete-selection-mode t)               ;; Override selection

;; imenu
(setq-default imenu-use-markers nil
	      imenu-auto-rescan t
	      imenu-max-item-length 256)
(with-eval-after-load 'imenu
  (add-hook 'imenu-after-jump-hook #'pulse-momentary-highlight-one-line))

;; uniquify
(setq-default uniquify-buffer-name-style 'forward) ;; default 'post-forward-angle-brackets

;; saveplace
(save-place-mode 1)                           ;; Remember point in files
(setq save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
      (replace-regexp-in-string "\\\\)\\$" "\\|^/tmp/.+\\)$"
				save-place-ignore-files-regexp t t))

;; show-parent
(setq-default show-paren-delay 0
	      show-paren-context-when-offscreen t ;; show context in the echo area
	      ;; show-paren-when-point-inside-paren t
	      blink-matching-paren nil)      ;; not show matching parent in echo when closing

;; autorevert
(setq-default ffap-machine-p-known 'accept   ;; stop ffap from pinging random hosts
	      ffap-require-prefix t          ;; require prefix for ffap
	      dired-at-point-require-prefix t ;; Como ffap-require-prefix para directorios
	      auto-revert-verbose nil        ;; not show message when file changes
	      auto-revert-mode-text ""
	      auto-revert-avoid-polling t)   ;; don't do pooling for autorevert (use notifications).)

(run-with-idle-timer 1 nil (lambda ()
			     (ffap-bindings)
			     (global-auto-revert-mode 1))) ;; Autoload files changed in disk

;; recentf
(setq-default recentf-max-saved-items 48     ;; Max items saved
	      recentf-auto-cleanup nil)      ;; Make cleanup when idle for 10 seconds. (default 'mode)
(with-eval-after-load 'recentf
  (recentf-mode 1)
  (run-with-idle-timer 10 nil #'recentf-cleanup))


(defun my/common-hook ()
  "Enable electric-pair-local-mode"
  (setq-local show-trailing-whitespace t  ;; Show trailing whitespaces
	      indicate-empty-lines t      ;; Show empty lines at end of file
	      )
  (electric-pair-local-mode 1))

(add-hook 'prog-mode-hook #'my/common-hook)
(add-hook 'text-mode-hook #'my/common-hook)
(add-hook 'conf-mode-hook #'my/common-hook)

;; hl-line
(keymap-global-set "M-s h L" #'hl-line-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; winner
(setq-default winner-dont-bind-my-keys t)
(winner-mode t)

;; There is already a winner-repeat-map with different bindings
;; so I cannot use the same, becaus eas it is loaded latter,
;; it will be overwritten
(my/repeat-keymap my/winner-repeat-map ctl-x-4-map
  :doc "Repeat map for `winner' commands."
  "u"  #'winner-undo
  "r"  #'winner-redo)

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; eldoc
(setq-default eldoc-idle-delay 2                              ;; default 0.5
	      eldoc-print-after-edit t                        ;; only show after edit
	      eldoc-minor-mode-string nil                     ;; nothing in the modeline
	      eldoc-echo-area-display-truncation-message nil) ;; Not verbose when truncated)
(with-eval-after-load 'eldoc
  (global-eldoc-mode -1))   ;; This is enabled by default, disable it

(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)

;; gdb
(setq-default gdb-debug-log-max nil   ;; no limit log
	      gdb-many-windows nil
	      gdb-show-main t)

;; hideif mode
(setq-default hide-ifdef-shadow t
	      hide-ifdef-initially t)

;; vc
(setq-default vc-follow-symlinks t          ;; Open links not open
	      vc-handled-backends '(Git Hg) ;; Only git or mercurial
	      vc-display-status nil         ;; No info on the modeline.
	      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
					   vc-ignore-dir-regexp
					   tramp-file-name-regexp))

;; Context Menu
(when (fboundp context-menu-mode)
  (context-menu-mode 1))

;; Man
(setq-default Man-notify-method 'pushy)

;; completion
(setq-default completion-show-help nil              ;; Don't show help header in completion buffer
	      completion-auto-help 'visible         ;; Update completions when visible and no hide
	      completion-auto-select 'second-tab    ;; Show completions on second tab (default nil)
	      ;; minibuffer-visible-completions t      ;; Jury completions selection
	      completion-auto-wrap t                ;; wrap movement
	      completions-detailed t                ;; show more detailed completions
	      ;; completions-format 'one-column        ;; Vertical completion list
	      completions-max-height 15
	      completion-styles '(substring partial-completion emacs22)
	      ;; M-x show context-local commands
	      read-extended-command-predicate #'command-completion-default-include-p
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      completion-ignore-case t

	      completion-auto-deselect t            ;; De-select completions on write
	      ;;completions-sort 'historical          ;; alphabetical + historical (needs emacs 29)
	      )

;; project
(setq-default project-vc-include-untracked nil
	      project-mode-line t)

;; These two must be enabled/disabled together
;; (setq-default enable-recursive-minibuffers t) ;; Enable nesting in minibuffer
;; (minibuffer-depth-indicate-mode 1)            ;; Mostrar nivel de nesting en minibuffer

(add-hook 'minibuffer-setup-hook #'my/unset-gc)
(add-hook 'minibuffer-exit-hook #'my/restore-gc)

;; Arrows up/down search prefix in history like `history-search-backward' in bash
(keymap-set minibuffer-local-map "<down>" #'next-complete-history-element)
(keymap-set minibuffer-local-map "<up>" #'previous-complete-history-element)

(defun my/completion-setup-hook ()
  "My hook for Completions window."
  (with-current-buffer standard-output
    (setq-local mode-line-format nil)
    (display-line-numbers-mode -1)))

(add-hook 'completion-setup-hook #'my/completion-setup-hook 10)

(defconst my/display-buffer-at-bottom
  '((display-buffer-reuse-mode-window display-buffer-at-bottom)
    (dedicated . t)
    (window-height . 0.3)
    (window-width . 1.0)
    (preserve-size . (t . t))
    (inhibit-same-window . t))
  "Windows configuration for display-buffer-alist")

;;__________________________________________________________
;; Isearch

(setq-default search-nonincremental-instead nil    ;; No incremental if enter & empty
	      lazy-highlight-no-delay-length 1     ;; normal delay
	      ;; lazy-highlight-initial-delay 0       ;; old config replaced by lazy-highlight-no-delay-length
	      isearch-allow-scroll t               ;; Permit scroll can be 'unlimited
	      isearch-lazy-count t
	      search-ring-max 256
	      regexp-search-ring-max 256
	      isearch-yank-on-move 'shift          ;; Copy text from buffer with meta
	      ;; isearch-wrap-function #'ignore       ;; Look at the emacs-major-version check
	      ;; isearch-wrap-pause t                 ;; Disable wrapping nil.
	      isearch-repeat-on-direction-change t ;; Don't go to the other end on direction change
	      isearch-lax-whitespace nil
	      ;; isearch-regexp-lax-whitespace t      ;; swiper like fuzzy search
	      ;; search-whitespace-regexp ".*?"
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

  (keymap-set isearch-mode-map "C-RET" #'my/isearch-exit-other-end)
  (keymap-set isearch-mode-map "C-<return>" #'my/isearch-exit-other-end)
  (keymap-set isearch-mode-map "<remap> <isearch-delete-char>" #'isearch-del-char)
  (keymap-set search-map "." #'isearch-forward-thing-at-point))

(setq-default list-matching-lines-jump-to-current-line t)

;; Occur
(with-eval-after-load 'replace  ;; is where occur resides
  ;; With error follow this is pointless.
  (keymap-set occur-mode-map "SPC" #'occur-mode-display-occurrence)
  (add-hook 'occur-hook (lambda ()
			  (beginning-of-line)
			  (recenter nil t)))

  (add-hook 'occur-mode-hook (lambda ()
			       (setq-local window-size-fixed 'height)
			       (hl-line-mode 1)
			       (display-line-numbers-mode -1)
			       (switch-to-buffer-other-window "*Occur*")
			       ;; (next-error-follow-minor-mode 1)
			       ))

  (add-hook 'occur-mode-find-occurrence-hook
	    (lambda ()
	      (let ((win (get-buffer-window "*Occur*")))
		(when (and win
			   (eq this-command #'occur-mode-goto-occurrence))
		  (quit-restore-window win)
		  (isearch-done)))))

  (add-to-list 'display-buffer-alist `("*Occur*" . ,my/display-buffer-at-bottom)))

;;__________________________________________________________
;; The Colors I am using my own theme
(load-theme 'simple-16)

(defun my/set-font ()
  "Conditionally set the Hack font."
  (cond
   ((member "Hack" (font-family-list))
    (set-face-attribute 'default nil :family "Hack" :height 105))
   ((member "Cascadia Mono" (font-family-list))
    (set-face-attribute 'default nil :family "Cascadia Mono" :height 105))))

(cond
 ((daemonp) (add-hook 'server-after-make-frame-hook
		      (lambda ()
			(when (display-graphic-p)
			  (my/set-font)))))
 ((display-graphic-p) (my/set-font)))

(defalias 'my/named-color 'simple-16-theme-color)

;;__________________________________________________________
;;Packages options
;;__________________________________________________________

;;__________________________________________________________
;; Some bindings
(keymap-global-set "<remap> <delete-char>" #'delete-forward-char) ;; delete respecting with C-d
(keymap-global-set "<remap> <count-words-region>" #'count-words)  ;; count on whole file or region if active


;;__________________________________________________________
;; compile
(setq-default compilation-scroll-output 'first-error
	      compilation-always-kill t)

;;; Display compilation buffer at buttom
(add-to-list 'display-buffer-alist `("*compilation*" . ,my/display-buffer-at-bottom))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook
	    (lambda ()
	      (setq window-size-fixed 'width))))

;;__________________________________________________________
;; ssh
(setq-default tramp-auto-save-directory
	      (expand-file-name "tramp-autosave-dir" user-emacs-directory)
	      tramp-default-method "ssh"                   ;; Already default
	      remote-file-name-inhibit-cache 60            ;; Default 10
	      tramp-completion-reread-directory-timeout 120;; Default 10
	      password-cache-expiry 3600                   ;; Cache for 1 hour
	      tramp-use-scp-direct-remote-copying t        ;; copy directly between remote hosts
	      remote-file-name-inhibit-locks t          ;; I know that different Emacs sessions are not modifying the same remote file
	      tramp-verbose (if init-file-debug 10 3)      ;; Default 3 always
	      ;; tramp-use-ssh-controlmaster-options nil      ;; use system control master.
	      tramp-use-connection-share nil
	      tramp-completion-use-auth-sources nil        ;; not use auth-sources in tramp
	      )

(with-eval-after-load 'tramp
  ;; Tramp don't read the auth-sources on sudo-edit
  (connection-local-set-profile-variables 'my/tramp-profile
					  '((auth-sources . nil)))
  (connection-local-set-profiles '(:application tramp :protocol "sudo") 'my/tramp-profile)

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-process-environment
               (format "DISPLAY=%s" (getenv "DISPLAY"))))

;;__________________________________________________________
;; splitting
(setq-default windmove-display-no-select t) ;; select windows after displaying it

(defvar-keymap my/0-map
  :doc "The base keymap for manage windows."
  "0" #'delete-window
  "<left>" #'windmove-delete-left
  "<right>" #'windmove-delete-right
  "<up>" #'windmove-delete-up
  "<down>" #'windmove-delete-down)


;; Direct shortcut without prefix.
(keymap-set ctl-x-map "<left>" #'windmove-left)
(keymap-set ctl-x-map "<right>" #'windmove-right)
(keymap-set ctl-x-map "<down>" #'windmove-down)
(keymap-set ctl-x-map "<up>" #'windmove-up)

(keymap-set ctl-x-map "S-<left>" #'windmove-swap-states-left)
(keymap-set ctl-x-map "S-<right>" #'windmove-swap-states-right)
(keymap-set ctl-x-map "S-<down>" #'windmove-swap-states-down)
(keymap-set ctl-x-map "S-<up>" #'windmove-swap-states-up)

(keymap-set ctl-x-4-map "<left>" #'windmove-display-left)
(keymap-set ctl-x-4-map "<right>" #'windmove-display-right)
(keymap-set ctl-x-4-map "<down>" #'windmove-display-down)
(keymap-set ctl-x-4-map "<up>" #'windmove-display-up)

(keymap-set ctl-x-map "0" (cons "windmove-delete" my/0-map))

;;__________________________________________________________
;; tab-bar
(setq-default tab-bar-tab-hints t  ;; show tab numbers
	      tab-bar-close-last-tab-choice 'tab-bar-mode-disable ;; When close last
	      tab-bar-show 1)

(defvar-keymap my/tmux-like-keymap
  :doc "A keymap that emulates some of the tmux bindings."
  "i" #'tab-new
  "k" #'tab-close
  "1" #'tab-close-other
  "r" #'tab-rename
  "0" (cons "windmove-delete" my/0-map)
  "v" #'split-window-below
  "h" #'split-window-right
  "b" #'switch-to-buffer-other-tab
  "d" #'dired-other-tab

  "<left>" #'windmove-left
  "<right>" #'windmove-right
  "<down>" #'windmove-down
  "<up>" #'windmove-up)

;; Add repeatable bindings to my/tmux-like-keymap
(my/repeat-keymap my/tmux-repeat-map my/tmux-like-keymap
  :doc "Repeat map for tmux prefix"
  "C-<left>" #'tab-previous
  "C-<right>" #'tab-next
  "M-S-<left>" #'tab-bar-move-tab-backward
  "M-S-<right>" #'tab-bar-move-tab

  "S-<left>" #'windmove-swap-states-left
  "S-<right>" #'windmove-swap-states-right
  "S-<down>" #'windmove-swap-states-down
  "S-<up>" #'windmove-swap-states-up)

;; C-z provides a similar behavior to my tmux config
(keymap-global-set "C-z" my/tmux-like-keymap)

;;__________________________________________________________
;; Two options for diffs
(setq-default
 ediff-window-setup-function #'ediff-setup-windows-plain
 ;; default ediff-split-window-function #'split-window-vertically
 ediff-split-window-function  (lambda (&optional arg)
				(if (> (frame-width) 150)
				    (split-window-horizontally arg)
				  (split-window-vertically arg))))
(with-eval-after-load 'winner
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo))

;;__________________________________________________________
;; eshell mouse

(defun my/with-face (str &rest face-plist)
    (propertize str 'face face-plist))

(defun my/eshell-prompt-function ()
  "Personalized Eshell prompt."
  (concat
   (my/with-face (concat (user-login-name) "@" (system-name))
	      :foreground (simple-16-theme-color green))
   (my/with-face (concat ":" (abbreviate-file-name (eshell/pwd)))
	      :foreground (simple-16-theme-color blue))
   (if (= (file-user-uid) 0) " #" " $")
   `,(my/with-face "\n>" :foreground (simple-16-theme-color cyan))
   " "))

(setq-default eshell-history-append t   ;; No override eshell history; append
	      eshell-prompt-function #'my/eshell-prompt-function
	      eshell-highlight-prompt nil)

;;__________________________________________________________
;; xterm mouse
(setq-default mouse-drag-mode-line-buffer t)

(unless (or (display-graphic-p)
	    (string-equal (getenv "TERM") "linux"))
  (xterm-mouse-mode t))                    ;; mover el cursor al click

(if (fboundp #'mouse-wheel-mode)
    (progn
      (setq-default mouse-wheel-scroll-amount '(3             ;; No modifier
						((control) . 9)
						((meta) . hscroll)
						((shift) . text-scale)) ;; in terminal does not work
		    mouse-wheel-tilt-scroll t          ;; horizontal scrolling with touchpad
		    mouse-wheel-progressive-speed nil
		    mouse-scroll-delay 0)
      (mouse-wheel-mode 1))                    ;; Explicit call mouse-wheel-mode AFTER setting mouse-wheel-scroll-amount

  ;; Else set them manually, will be overridden latter.
  (keymap-global-set "<mouse-4>" #'scroll-down-command)
  (keymap-global-set "<mouse-5>" #'scroll-up-command))

(keymap-global-set "<remap> <scroll-up-command>" #'scroll-up-line)
(keymap-global-set "<remap> <scroll-down-command>" #'scroll-down-line)

;;__________________________________________________________
;; Redefine and remap some commands.

(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (call-interactively 'back-to-indentation)
    (and (<= oldpos (point))
	 (/= (line-beginning-position) oldpos)
	 (call-interactively 'beginning-of-line))))

(keymap-global-set "<remap> <move-beginning-of-line>" #'my/smart-beginning-of-line)

;;__________________________________________________________
;; Undo
(setq-default undo-only t)               ;; undo does not go throw redo entries
(keymap-global-set "C-M-/" #'undo-redo)  ;; For gui; in tty "C-M-/" == "C-M-_"
;; (global-set-key (kbd "C-M-_") #'undo-redo) already set by default

(defvar-keymap undo-redo-repeat-map
  :doc "Keymap to repeat undo-redo key sequences."
  "u" #'undo
  "r" #'undo-redo)
(put #'undo 'repeat-map 'undo-redo-repeat-map)
(put #'undo-redo 'repeat-map 'undo-redo-repeat-map)

;;__________________________________________________________
;; Flyspell (Orthography)
(setq-default ispell-following-word t ;;Check word around point not only before
	      ispell-quietly t)       ;; Suppress messages in ispell-word


;; Flyspell
(setq-default flyspell-use-meta-tab nil       ;; Not correct with M-TAB
	      flyspell-mode-line-string nil   ;; Not show Fly in modeline
	      flyspell-delay 1                ;; default 3
	      flyspell-sort-corrections t     ;; Alphabetically sort corrections
	      flyspell-issue-welcome-flag nil ;; no message on start
	      flyspell-issue-message-flag nil ;; no message when checking
	      )

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'text-mode-hook #'turn-on-flyspell)

(with-eval-after-load 'flyspell
  (defvar-keymap flyspell-basic-map
    :doc "The base keymap for `flyspell-mode'."
    "r" #'flyspell-region
    "b" #'flyspell-buffer
    "n" #'flyspell-goto-next-error)

  (setf (cdr flyspell-mode-map) nil)  ;; clear yas minor map
  (keymap-set flyspell-mode-map "C-c f" (cons "flyspell" flyspell-basic-map)))


;;__________________________________________________________
;; Completions

(setq-default eglot-events-buffer-config
	      '(:size 2000000 :format lisp)
	      eglot-send-changes-idle-time 1.0 eglot-extend-to-xref
	      t eglot-ignored-server-capabilities
	      '(:inlayHintProvider :documentRangeFormattingProvider
				   :documentOnTypeFormattingProvider))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((cuda-mode) "clangd"))
  (add-hook 'eglot-managed-mode-hook
	    (lambda nil
	      (when (eldoc--supported-p)
		(eldoc-mode (if (eglot-managed-p) 1 -1))))))

;;====================
;; cc-mode
(setq-default c-default-style '((java-mode . "java")
				(awk-mode . "awk")
				(other . "linux"))
	      c-doc-comment-style 'doxygen)

(with-eval-after-load 'cc-mode
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
;; repeat-mode
(setq-default repeat-check-key nil
	      repeat-exit-key (kbd "RET"))
(repeat-mode 1)

;; Bind repeat to next/prev buffer
(my/repeat-keymap my/next-prev-repeat-map ctl-x-map
  :doc "Repeat map for `next|prev-buffer' commands."
  "C-<left>" #'previous-buffer
  "C-<right>" #'next-buffer)

;;__________________________________________________________
;; Flymake

(setq-default flymake-no-changes-timeout 1.0
	      flymake-wrap-around nil
	      ;; flymake-show-diagnostics-at-end-of-line t  ;; I want to try that
	      flymake-mode-line-format nil)

(with-eval-after-load 'flymake
  ;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (defvar-keymap flymake-basic-map
    :doc "The base keymap for `flymake-mode'."
    "d" #'flymake-show-diagnostic
    "b" #'flymake-show-buffer-diagnostics
    "l" #'flymake-switch-to-log-buffer)

  (my/repeat-keymap flymake-repeat-map flymake-basic-map
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error)

  (keymap-set flymake-mode-map "C-c k" (cons "flymake" flymake-basic-map)))

;;__________________________________________________________
;; Email mode for mutt
;;__________________________________________________________
;; message-mode
(setq-default send-mail-function #'smtpmail-send-it
	      smtpmail-stream-type  'ssl
	      smtpmail-smtp-service 465
	      smtpmail-debug-info init-file-debug   ;; show delivery info
	      ;; smtpmail-debug-verb init-file-debug   ;; instruct the server to be verbose
	      message-default-mail-headers "Cc: \nBcc: \n"
	      message-kill-buffer-on-exit t
	      message-send-mail-function #'message-use-send-mail-function
	      mail-header-separator ""
	      )

(with-eval-after-load 'message-mode
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t))

(add-to-list 'auto-mode-alist '("/neomut" . message-mode))
(add-to-list 'auto-mode-alist '("neomutt-Ergus-" . message-mode))
(add-to-list 'auto-mode-alist '("draft" . message-mode))


;;__________________________________________________________
;; Python mode
;; There is a tree-sitter module for this, I will try that

(setq-default python-shell-interpreter "ipython"
              python-shell-interpreter-args "--simple-prompt"
	      ;;python-shell-prompt-detect-failure-warning nil
	      python-check-command "pyflakes"
	      flycheck-python-flake8-executable "flake8")

(eval-after-load 'python
  '(keymap-set python-mode-map "C-c C-z" #'python-shell))

;;__________________________________________________________
;; Dired-mode settings (file manager)
(setq-default dired-recursive-copies 'top                  ;; Always ask recursive copy
	      dired-recursive-deletes 'top                 ;; Always ask recursive delete
	      dired-dwim-target 'dired-dwim-target-recent  ;; Copy in split mode with p
	      dired-auto-revert-buffer (lambda (dirname)
					 (and (not (file-remote-p dirname))
					      (dired-directory-changed-p dirname))) ;; auto revert dired
	      ;; dired-listing-switches "-alh"                ;; commands to ls
	      dired-listing-switches "-agho --group-directories-first"
	      dired-isearch-filenames 'dwim
	      dired-hide-details-hide-symlink-targets nil  ;; don't hide linkk targets
	      dired-maybe-use-globstar t                   ;; use shell's globstar
	      dired-kill-when-opening-new-dired-buffer t   ;; kill when opening a new directory.
	      dired-mouse-drag-files t
	      dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open")
					     ("\\.jpe?g\\'" "xdg-open")
					     ("\\.png\\'" "xdg-open")
					     ("\\.gif\\'" "xdg-open")))

(with-eval-after-load 'dired
  (require 'dired-x)
  (keymap-set dired-mode-map "<mouse-2>" #'dired-mouse-find-file)
  (add-hook 'dired-mode-hook #'hl-line-mode))

;;__________________________________________________________
;; ibuffer
(setq-default ibuffer-default-sorting-mode 'alphabetic  ;; can use recency)
	      ibuffer-use-other-window t
	      ibuffer-jump-offer-only-visible-buffers t)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)


(with-eval-after-load 'xref
  (setq-default xref-search-program 'ripgrep
		xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom
		xref-show-xrefs-function #'xref-show-definitions-buffer-at-bottom)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references t))

(defvar-keymap my/xref-basic-map
    :doc "The base keymap for `xref'."
    "d" #'xref-find-definitions
    "4" #'xref-find-definitions-other-window
    "a" #'xref-find-apropos
    "r" #'xref-find-references
    "TAB" #'completion-at-point)

(my/repeat-keymap my/xref-repeat-map my/xref-basic-map
    "p" #'xref-go-back
    "n" #'xref-go-forward)

(put #'xref-find-definitions 'repeat-map my/xref-repeat-map)
(put #'xref-find-references 'repeat-map my/xref-repeat-map)

(keymap-global-set "C-c x" (cons "xref" my/xref-basic-map))

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

(with-eval-after-load 'smerge-mode
  (my/repeat-keymap smerge-repeat-map smerge-basic-map
    "n" #'smerge-next
    "p" #'smerge-prev
    "a" #'smerge-keep-all
    "b" #'smerge-keep-base
    "l" #'smerge-keep-lower
    "u" #'smerge-keep-upper))

(add-hook 'find-file-hook #'my/enable-smerge-maybe)
(add-hook 'magit-diff-visit-file-hook #'my/enable-smerge-maybe)

;;__________________________________________________________
;; Enable tree-sitter for some modes by default if the tree-sitter
;; directory exists

(when (file-exists-p (expand-file-name "tree-sitter" user-emacs-directory))

  (setq-default toml-ts-mode-indent-offset 4
		cmake-ts-mode-indent-offset 4
		json-ts-mode-indent-offset 4)

  (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

  (add-to-list 'auto-mode-alist
               '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.\\(ba\\)?sh\\'" . bash-ts-mode))

  (add-to-list 'auto-mode-alist
               '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
                 . dockerfile-ts-mode))

  (add-hook 'c++-ts-mode-hook (lambda ()
				(c-ts-mode-set-style 'linux)
				(setq-local tab-width 4
					    c-ts-mode-indent-offset tab-width)))

  ;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))
  )

;;__________________________________________________________

(provide 'init)

;;; init.el ends here
