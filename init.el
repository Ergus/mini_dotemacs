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
	      read-process-output-max       1048576
	      ;; read-key-delay 0.01           ;; already default
	      recenter-redisplay nil
	      ;;recenter-positions '(top middle bottom)
	      ;; line-move-visual nil       ;; move cursor visual lines
	      backward-delete-char-untabify-method nil ;; Don't untabify on backward delete

	      split-width-threshold 140     ;; Limit for vertical split (default 160)
	      ;; kill-whole-line t
	      kill-region-dwim 'emacs-word
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
	      frame-inhibit-implied-resize t      ;; test this for tilling windows manager
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
	      kill-buffer-quit-windows t          ;; Kill and quit
	      quit-restore-window-no-switch t     ;; No show previous buffer but quit on kill buffer
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
	      ;;large-file-warning-threshold nil    ;; no warning when the file is too big
	      proced-enable-color-flag t            ;; colors in proced
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?\u2502))  ;; also works 2503, it is wider

(defmacro my/repeat-keymap (newkeymap keymap &rest defs)
  "Generate a NEWKEYMAP and then add it to a another KEYMAP.

This is inteded to add the same bindings to a keymap like `C-x
M-<left>' and repeat with M-<left>."
  (declare (indent 2))
  `(progn
     (defvar-keymap ,newkeymap :repeat t ,@defs)
     ;; Add the keys with same suffix to `keymap'
     ,@(let ((sets))
	 (while-let ((key (car defs))
		     (value (cadr defs)))
	   (unless (eq key :doc)
	     (push `(keymap-set ,keymap ,key ,value) sets))
	   (setq defs (cddr defs)))
	 (nreverse sets))))

(if init-file-debug
    (progn
      (require 'use-package-ensure)
      (setq-default use-package-always-ensure t
		    use-package-enable-imenu-support t
		    use-package-verbose t
		    use-package-expand-minimally nil
		    use-package-compute-statistics t
		    debug-on-error t
		    native-comp-async-report-warnings-errors t))

  (setq-default use-package-always-ensure nil
		use-package-enable-imenu-support nil
		use-package-verbose nil
		use-package-expand-minimally t
		native-comp-async-report-warnings-errors 'silent))

(defun my/find-init ()
  "Open the init file to edit it."
  (interactive)
  (find-file user-init-file))

(defun my/treesit-install-grammar (lang source)
  "Attempt to install a grammar if not available already"
  (setf (alist-get lang treesit-language-source-alist)  ;; Add the grammar source entry
	`(,source nil nil nil nil))

  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

;;__________________________________________________________
;; Config file not here to not track it
(setq-default custom-file
	      (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file)
  (message "Creating %s" custom-file))
(load custom-file)

;;__________________________________________________________
;; Keybindings

;; which-key
(setq-default which-key-idle-delay 2.0
	      which-key-show-early-on-C-h t
	      which-key-idle-secondary-delay 0.01
	      which-key-side-window-max-height 0.15
	      ;; which-key-dont-use-unicode t
	      which-key-is-verbose init-file-debug
	      which-key-show-remaining-keys t
	      which-key-lighter nil
	      ;;which-key-popup-type 'minibuffer
	      ;;which-key-show-prefix 'mode-line
	      which-key-separator ": ")

(which-key-mode t)
(which-key-add-key-based-replacements
    "C-x r" "rectangle||register"
    "C-x n" "narrow"
    "C-x p" "project"
    "C-x RET" "coding-system"
    "C-x @" "event-apply-modifier"
    "C-x ESC" "repeat-command"
    "C-x 8" "unicode"
    "C-x x" "buffer"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x w" "window"
    "C-x C-k" "kmacro")

;; sidebar
(defvar-keymap my/sidebar-map
  :doc "Keymap to toggle sidebars.")
(keymap-global-set "C-c b" (cons "sidebars" my/sidebar-map))

(defvar-keymap my/ctrl-c-c
  :doc "The base keymap for `C-c c'."
  "l" #'find-library)
(keymap-global-set "C-c c" (cons "my/ctrl-c-c" my/ctrl-c-c))

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
	      imenu-flatten 'annotation
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

(ffap-bindings)
(recentf-mode 1)
(global-auto-revert-mode 1) ;; Autoload files changed in disk

;; recentf
(setq-default recentf-max-saved-items 48     ;; Max items saved
	      recentf-auto-cleanup nil)      ;; Make cleanup when idle for 10 seconds. (default 'mode)

(with-eval-after-load 'recentf
  (run-with-idle-timer 10 nil #'recentf-cleanup))

;;__________________________________________________________
;; Profiler
;; Shows the function in spaceline
(setq-default which-func-update-delay 0.2)  ;; Substitutes idle-update-delay

;; Show the tab indicator symbol in whitespace mode
(with-eval-after-load 'whitespace
  (setq whitespace-style '(faces tab-mark missing-newline-at-eof)
	whitespace-display-mappings `((tab-mark ?\t [,(make-glyph-code ?» 'whitespace-tab) ?\t] ))
	))

(defun my/common-hook ()
  "Enable electric-pair-local-mode"
  (setq-local show-trailing-whitespace t  ;; Show trailing whitespaces
	      indicate-empty-lines t      ;; Show empty lines at end of file
	      )
  (whitespace-mode 1)
  (electric-pair-local-mode 1))


(add-hook 'prog-mode-hook #'my/common-hook)
(add-hook 'text-mode-hook #'my/common-hook)
(add-hook 'conf-mode-hook #'my/common-hook)

;; hl-line
(keymap-global-set "M-s h L" #'hl-line-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; hilit-chg changes
(defvar-keymap highlight-changes-map
  :doc "The base keymap for `highlight changes'."
  "c" #'highlight-changes-mode
  "v" #'highlight-changes-visible-mode
  "r" #'highlight-changes-remove-highlight
  "n" #'highlight-changes-next-change
  "p" #'highlight-changes-previous-change
  "f" #'highlight-changes-rotate-faces
  "b" #'highlight-compare-buffers
  "d" #'highlight-compare-with-file)
(keymap-global-set "M-s h c" (cons "highlight-changes" highlight-changes-map))

;; winner
(setq-default winner-dont-bind-my-keys t)
(add-hook 'window-setup-hook #'winner-mode)


;; There is already a winner-repeat-map with different bindings
;; so I cannot use the same, becaus eas it is loaded latter,
;; it will be overwritten
(my/repeat-keymap my/winner-repeat-map ctl-x-4-map
  :doc "Repeat map for `winner' commands."
  "u"  #'winner-undo
  "r"  #'winner-redo)

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; abbrev
(which-key-add-key-based-replacements "C-x a" "abbrev")

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
	      gdb-restore-window-configuration-after-quit t
	      gdb-show-main t)

;; hideif mode
(setq-default hide-ifdef-shadow t
	      hide-ifdef-initially t)

;; vc
(setq-default vc-follow-symlinks t          ;; Open links not open
	      vc-handled-backends '(Git Hg) ;; Only git or mercurial
	      vc-display-status nil         ;; No info on the modeline.
	      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" ;; disable vc on remotes
					   vc-ignore-dir-regexp
					   tramp-file-name-regexp))
(which-key-add-key-based-replacements "C-x v" "vc")

;; Context Menu
(context-menu-mode 1)

;; Man
(setq-default Man-notify-method 'pushy)

;; dabbrev
(autoload #'dabbrev-filter-elements "dabbrev")

(setq-default dabbrev-check-all-buffers nil
	      dabbrev-ignored-buffer-regexps '("\\`[ *]"))

;; completion
(setq-default completion-show-help nil              ;; Don't show help header in completion buffer
	      completion-auto-help 'visible         ;; Update completions when visible and no hide
	      completion-auto-select 'second-tab    ;; Show completions on second tab (default nil)
	      ;; minibuffer-visible-completions t      ;; Jury completions selection
	      completion-auto-wrap t                ;; wrap movement
	      completions-detailed t                ;; show more detailed completions
	      completions-format 'one-column        ;; Vertical completion list
	      completions-max-height 15
	      completion-styles '(substring partial-completion emacs22)
	      ;; M-x show context-local commands
	      read-extended-command-predicate #'command-completion-default-include-p
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t
	      completion-ignore-case t

	      completion-auto-deselect t            ;; De-select completions on write
	      completions-sort 'historical          ;; alphabetical + historical

	      minibuffer-completion-auto-choose nil ;; no insert completions in minib
	      )

;; project
(setq-default project-vc-include-untracked nil
	      project-mode-line nil ;; Disabled due to performance issue with tramp.
	      )

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
  ;;  (keymap-set isearch-mode-map "<remap> <isearch-abort>" #'isearch-exit)
  (keymap-set isearch-mode-map "<remap> <isearch-delete-char>" #'isearch-del-char)

  (keymap-set search-map "." #'isearch-forward-thing-at-point)
  (which-key-add-key-based-replacements "M-s h" "highlight"))

(setq-default list-matching-lines-jump-to-current-line t)

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
    (set-face-attribute 'default nil :family "Hack" :height 110))
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
(setq-default compilation-scroll-output nil
	      compilation-context-lines t   ;; Don't scroll compilation buffer
	      compilation-always-kill t)

;;; Display compilation buffer at buttom
(add-to-list 'display-buffer-alist `((major-mode . compilation-mode) . ,my/display-buffer-at-bottom))

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
	      ;; tramp-ssh-controlmaster-options (concat
	      ;; 				       "-o ControlPath=~/.ssh/%%r@%%h-%%p "
	      ;; 				       "-o ControlMaster=auto -o ControlPersist=yes")
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
  :doc "The base keymap for `highlight changes'."
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

;; See also the window-layout-* family of functions
(keymap-set ctl-x-map "S-<left>" #'windmove-swap-states-left)
(keymap-set ctl-x-map "S-<right>" #'windmove-swap-states-right)
(keymap-set ctl-x-map "S-<down>" #'windmove-swap-states-down)
(keymap-set ctl-x-map "S-<up>" #'windmove-swap-states-up)

(keymap-set ctl-x-4-map "<left>" #'windmove-display-left)
(keymap-set ctl-x-4-map "<right>" #'windmove-display-right)
(keymap-set ctl-x-4-map "<down>" #'windmove-display-down)
(keymap-set ctl-x-4-map "<up>" #'windmove-display-up)

(keymap-set ctl-x-map "0" (cons "windmove-delete" my/0-map))

(which-key-add-key-based-replacements
    "C-x w f" "flip"
    "C-x w r" "rotate layout"
    "C-x w o" "rotate buffers"
    "C-x w ^" "break")

;;__________________________________________________________
;; tab-bar
(setq-default tab-bar-tab-hints t  ;; show tab numbers
	      tab-bar-close-last-tab-choice 'tab-bar-mode-disable ;; When close last
	      tab-bar-show 1)
(which-key-add-key-based-replacements "C-x t" "tab-bar")  ;; by default

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
  "S-<up>" #'windmove-swap-states-up
  "v" #'split-window-below
  "h" #'split-window-right
  "V" #'split-root-window-below
  "H" #'split-root-window-right
  "B" #'tab-window-detach

  "r" #'balance-windows)

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
	      eshell-highlight-prompt nil
	      eshell-buffer-name "*Eshell*")

;;__________________________________________________________
;; xterm mouse
(setq-default mouse-drag-mode-line-buffer t)

(unless (or (display-graphic-p)
	    (string-equal (getenv "TERM") "linux"))
  (tty-tip-mode 1)
  (xterm-mouse-mode t))  ;; mover el cursor al click

(if (fboundp #'mouse-wheel-mode)
    (progn
      (setq-default mouse-wheel-scroll-amount '(3             ;; No modifier
						((control) . 9)
						((meta) . hscroll)
						((shift) . text-scale)) ;; in terminal does not work
		    mouse-wheel-scroll-amount-horizontal 5 ;; Faster horizontal scroll
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
(setq-default flyspell-delay-use-timer t      ;; New flyspell behavior
	      flyspell-use-meta-tab nil       ;; Not correct with M-TAB
	      flyspell-mode-line-string nil   ;; Not show Fly in modeline
	      flyspell-delay 1                ;; default 3
	      flyspell-sort-corrections t     ;; Alphabetically sort corrections
	      flyspell-issue-welcome-flag nil ;; no message on start
	      flyspell-issue-message-flag nil ;; no message when checking
	      )

(add-hook 'prog-mode-delay-hook #'flyspell-prog-mode)
(add-hook 'text-mode-delay-hook #'turn-on-flyspell)

(with-eval-after-load 'flyspell
  (defvar-keymap flyspell-basic-map
    :doc "The base keymap for `flyspell-mode'."
    "r" #'flyspell-region
    "b" #'flyspell-buffer
    "n" #'flyspell-goto-next-error)

  (setf (cdr flyspell-mode-map) nil)  ;; clear yas minor map
  (keymap-set flyspell-mode-map "C-c f" (cons "flyspell" flyspell-basic-map)))

;;====================
;; cc-mode
(setq-default c-default-style '((java-mode . "java")
				(awk-mode . "awk")
				(other . "linux")))

;;__________________________________________________________
;; sh mode
(defvaralias 'sh-basic-offset 'tab-width)

(add-hook 'sh-mode-hook (lambda nil
			  (setq-local indent-tabs-mode t
				      tab-width 4)))

;;__________________________________________________________
;; ruby-mode
;; Remap ruby-mode with the tree-sitter alternative
(setq-default ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.rjs\\'" . ruby-mode))

;;__________________________________________________________
;; xml-mode
(add-to-list 'auto-mode-alist
	     '("\\.\\(ipe\\|qrc\\|svn\\)\\'" . xml-mode))
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
;; Chequeo de syntaxis

(setq-default flymake-no-changes-timeout 1.0
	      flymake-wrap-around nil
	      flymake-show-diagnostics-at-end-of-line 'short  ;; I want to try that, t shows all diagnostics
	      flymake-mode-line-format nil
	      flymake-margin-indicators-string '((error "!" compilation-error)
						 (warning "!" compilation-warning)
						 (note "!" compilation-info)))

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

(with-eval-after-load 'message-mode
  (auto-fill-mode t)
  (mail-abbrevs-setup)
  (flyspell-mode t))

(add-to-list 'auto-mode-alist '("/neomut" . message-mode))
(add-to-list 'auto-mode-alist '("neomutt-Ergus-" . message-mode))
(add-to-list 'auto-mode-alist '("draft" . message-mode))

;;__________________________________________________________
;; Latex mode

(setq-default TeX-source-correlate-start-server t
	      TeX-auto-save t
	      TeX-parse-self t
	      LaTeX-babel-hyphen nil
	      TeX-master nil         ;; Multidocument
	      LaTeX-indent-level 4
	      LaTeX-item-indent 0)

(add-hook 'TeX-mode-hook (lambda ()
			   (LaTeX-math-mode 1)
			   ;; (auto-fill-mode 1)           ;; It causes issues and M-q saves the day.
			   (TeX-source-correlate-mode 1))) ;; open PDF in the edditing page
(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))

;; reftex
(setq-default reftex-cite-prompt-optional-args t   ; Prompt for empty optional arguments in cite
	      reftex-cite-format 'biblatex
	      reftex-plug-into-AUCTeX t
	      reftex-insert-label-flags t
	      reftex-save-parse-info t
	      reftex-enable-partial-scans t
	      reftex-use-multiple-selection-buffers t)

(add-hook 'TeX-mode-hook #'turn-on-reftex)

(eval-after-load 'reftex '(reftex-isearch-minor-mode))

;;__________________________________________________________
;;bibtex mode set use biblatex
(setq-default bibtex-dialect 'biblatex)
(add-to-list 'auto-mode-alist '("\\.bib\\'" . bibtex-mode))

;;__________________________________________________________
;; Python mode
;; There is a tree-sitter module for this, I will try that

(setq-default python-shell-interpreter "ipython"
              python-shell-interpreter-args "--simple-prompt"
	      ;;python-shell-prompt-detect-failure-warning nil
	      python-check-command "pyflakes"
	      ;; flycheck-python-flake8-executable "flake8"
	      python-indent-block-paren-deeper t
	      ;; python-shell-dedicated 'project  ;; This is better to set in a dir-locals file
	      python-shell-completion-native-enable t
	      python-forward-sexp-function nil
	      )

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
(setq-default ibuffer-default-sorting-mode 'recency  ;; can use alphabetic)
	      ibuffer-use-other-window t                ;; ibuffer in other windows
	      ibuffer-jump-offer-only-visible-buffers t
	      ibuffer-human-readable-size t)

(keymap-global-set "<remap> <list-buffers>" #'ibuffer)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)
(eval-after-load 'ibuffer
  '(which-key-add-keymap-based-replacements ibuffer--filter-map "G" "Groups"))

;;__________________________________________________________
;; xref
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
;; diff and smerge

;; smerge
(setq-default smerge-diff-buffer-name "*smerge-diff*"
	      smerge-command-prefix "\C-cs")

(defun my/enable-smerge-maybe ()
  "Auto-enable `smerge-mode' when merge conflict is detected."
  (when (or (not large-file-warning-threshold)
	    (< (buffer-size) large-file-warning-threshold))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
	(smerge-mode 1)))))

(with-eval-after-load 'smerge-mode
  (my/repeat-keymap smerge-repeat-map smerge-basic-map
    "n" #'smerge-next
    "p" #'smerge-prev
    "a" #'smerge-keep-all
    "b" #'smerge-keep-base
    "l" #'smerge-keep-lower
    "u" #'smerge-keep-upper)

  (which-key-add-keymap-based-replacements smerge-mode-map
    "C-c s" "smerge"
    "C-c s =" "smerge-diff"))

(add-hook 'find-file-hook #'my/enable-smerge-maybe)

;;__________________________________________________________
;; Move current line up and down C-M-<arrow> and duplicate

(defun my/untranspose-words (arg)
  (interactive "*p")
  (transpose-words (- arg)))

(defun my/untranspose-chars (arg)
  (interactive "*p")
  (transpose-chars (- arg)))

(my/repeat-keymap transpose-repeat-map ctl-x-map
  :doc "The keymap for `transpose-repeat' commands."
  "C-M-<left>" #'my/untranspose-words
  "C-M-<right>" #'transpose-words
  "M-<left>" #'my/untranspose-chars
  "M-<right>" #'transpose-chars)

;;__________________________________________________________
;; Enable tree-sitter for some modes by default if the tree-sitter
;; directory exists

(when (file-readable-p (expand-file-name "tree-sitter" user-emacs-directory))

  (setq-default toml-ts-mode-indent-offset 4
		cmake-ts-mode-indent-offset 4
		json-ts-mode-indent-offset 4
		rust-ts-mode-indent-offset 4
		go-ts-mode-indent-offset 4
		treesit-font-lock-level 4
		;; use these two to debug when developing, but they
		;; are too verbose
		treesit--indent-verbose t
		treesit--font-lock-verbose nil)

  (defvaralias 'c-ts-mode-indent-offset 'tab-width)

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
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.\\(ba\\)?sh\\'" . bash-ts-mode))

  (add-to-list 'auto-mode-alist
               '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
                 . dockerfile-ts-mode))

  ;; C/C++/Cuda modes
  (setq-default c-ts-mode-indent-style 'linux
		c-ts-mode-enable-doxygen nil)

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  (add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))

  (defun my/c-ts-indent-rules-generate (mode)
    "Rules generator for c, c++ and cuda modes"
    `(((node-is ")") parent-bol 0)
      ;; Arguments inside a function definition and call
      ((parent-is "argument_list") parent-bol c-ts-mode-indent-offset)
      ((parent-is "parameter_list") parent-bol c-ts-mode-indent-offset)
      ;; indent { after struct declaration
      ;;((node-is "^initializer_list$") parent-bol 0)
      ;; Open { for scope (code block)
      ((n-p-gp "compound_statement" "compound_statement" nil) parent-bol c-ts-mode-indent-offset)
      ;; Open { after if, while, else... etc
      ((node-is "compound_statement") standalone-parent 0)
      ;; switch-case
      ((node-is "case") standalone-parent c-ts-mode-indent-offset)
      ;; pragmas
      ((lambda (node parent _)
	 (and (string-match-p "#pragma" (or (treesit-node-text node t) ""))
	      (string-match-p "preproc_call" (or (treesit-node-type node) ""))
	      (string-match-p "compound_statement" (or (treesit-node-type parent) ""))))
       parent-bol c-ts-mode-indent-offset)
      ;; pragma multiline
      ((and no-node (parent-is "preproc_arg")) parent-bol c-ts-mode-indent-offset)
      ;; C++ specific
      ,@(when (not (eq mode 'c))
	  `(;;labels and closing } in a class
            ((n-p-gp ,(rx (or "}" "access_specifier")) "field_declaration_list" nil) parent-bol 0)
	    ;; Initialization list in a class : and rest of elements in constructor
	    ((n-p-gp "field_initializer_list" "function_definition" nil) standalone-parent c-ts-mode-indent-offset)
	    ((parent-is "field_initializer_list") parent-bol 0)
	    ;; Everything inside a class
	    ((node-is "field_declaration_list") parent-bol 0) ;; open {
	    ((parent-is "field_declaration_list") parent-bol c-ts-mode-indent-offset) ;; everything inside
	    ;; Opening { in namespace
            ((parent-is "namespace_definition") standalone-parent 0)
	    ;; try-catch fix
	    ((node-is "catch_clause") parent-bol 0)
	    ))))

  (defun my/c-ts-base-mode-hook ()
    "Hook to improve indentation in c, c++ and cuda (remapped) modes."
    (let ((name (treesit-parser-language treesit-primary-parser)))
      (message "Generate indent rules for %s" name)
      (setq-local tab-width 4)
      ;; These are the same rules, but one uses the override variable
      ;; and the other modifies the original `treesit-simple-indent-rules'
      (treesit-simple-indent-add-rules name (my/c-ts-indent-rules-generate name))
      ;; (setq-local treesit-simple-indent-override-rules
      ;; 		  `((,name . ,(my/c-ts-indent-rules-generate name))))
      ))
  (add-hook 'c-ts-base-mode-hook #'my/c-ts-base-mode-hook)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs `(((cuda-mode :language-id "cpp")
					   (cuda-ts-mode :language-id "cpp"))
					  . ,(eglot-alternatives
					      '("clangd" "ccls")))))

  (defun my/rust-arguments-indent (node-t parent-t grand-parent-t)
    ;;(message "HEE: %s | %s | %s" node-t parent-t grand-parent-t)
    (and (string-match-p "arguments" (treesit-node-type parent-t))
	 (string-equal "."
	  (treesit-node-type (treesit-node-next-sibling (treesit-node-parent parent-t))))))

  ;; Rust mode
  (defun my/rust-ts-mode-hook ()
    "Hook to improve indentation in rust mode"
    (setq-local tab-width 4)

    (setq-local treesit-simple-indent-override-rules
		`((rust . (((and (parent-is "function_item")
				 (node-is "block")) parent-bol 0)
			   ;; Extra indentation if the arguments are followed by a .
			   ;; As rust seems to like concatenation syntax
			   ((and (node-is ")")
				 my/rust-arguments-indent) parent-bol rust-ts-mode-indent-offset)
			   (my/rust-arguments-indent parent-bol ,(* 2 rust-ts-mode-indent-offset))
			   ;; Indentation for tuples
			   ((and (node-is ")")
				 (parent-is "tuple_expression")) parent-bol 0)
			   ((parent-is "tuple_expression") parent-bol rust-ts-mode-indent-offset)
			   ;; Indent the where in generics
			   ((node-is "where_clause")  parent-bol 0))))))
  (add-hook 'rust-ts-mode-hook #'my/rust-ts-mode-hook)

  )


;;__________________________________________________________

(provide 'init)

;;; init.el ends here

