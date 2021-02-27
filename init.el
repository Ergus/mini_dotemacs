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

(setq-default auto-revert-verbose nil       ;; not show message when file changes
	      auto-revert-avoid-polling t)  ;; use save signal
(global-auto-revert-mode t)		;; Autoload files changed in disk

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

(save-place-mode 1)                     ;; Remember point in files

(setq-default vc-follow-symlinks t	    ;; Open links not open
	      ;;tab-always-indent complete  ;; make tab key do indent only
	      initial-scratch-message ";; Welcome Jimmy!!"
	      ring-bell-function #'ignore
	      user-full-name "Jimmy Aguilar Mena"
	      inhibit-startup-message t
	      inhibit-startup-screen t
	      ;;tab-width 4		    ;; Tabulador a 4
	      ;;indent-tabs-mode t	    ;; Indent with tabs
	      ;;fill-column 80		    ;; default is 70
	      make-backup-files nil	    ;; Sin copias de seguridad
	      create-lockfiles nil	    ;; No lock files, good for tramp
	      visible-bell nil		    ;; Flash the screen (def)
	      display-line-numbers-width 4  ;; Minimum line number width
	      confirm-kill-processes nil    ;; no ask kill processes on exit
	      read-key-delay 0.01
	      recenter-redisplay nil
	      ;;recenter-positions '(top middle bottom)
	      line-move-visual nil
	      backward-delete-char-untabify-method nil ;; Don't untabify on backward delete

	      ;; split-width-threshold 160  ;; Limite para split vertical
	      ;; kill-whole-line t
	      ;; load-prefer-newer t
	      ;; mark-even-if-inactive nil	    ;; no mark no region
	      next-screen-context-lines 5           ;; Lines of continuity when scrolling
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
	      minibuffer-message-timeout 1
	      read-quoted-char-radix 16     ;; Read number of chars with C-q
	      kill-buffer-query-functions nil

	      eval-expression-print-length nil
	      eval-expression-print-level nil
	      tab-bar-show 1
	      suggest-key-bindings t

	      uniquify-buffer-name-style 'post-forward
	      )

;; Vertical window divider
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?\u2502))

;; These two must be enabled/disabled together
;; (setq enable-recursive-minibuffers t) ;; Enable nesting in minibuffer
;; (minibuffer-depth-indicate-mode 1)    ;; Mostrar nivel de nesting en minibuffer

(fido-mode t)
(setq-default completion-auto-help nil)
;; show choices verticall
;; (setq icomplete-separator "\n")
;; (setq icomplete-hide-common-prefix nil)
;; (setq icomplete-in-buffer t)

(ffap-bindings)

;;__________________________________________________________
;; Config file not here to not track it
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

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

(defmacro named-color (colorname)
  "Get color by name COLORNAME from `my/colors' alist."
  (simple-16-theme-color colorname))


;;__________________________________________________________
;; I don't want confirm exit, not write yes-not either
(if (version< emacs-version "28.0")
    (defalias 'yes-or-no-p 'y-or-n-p) ;; Reemplazar "yes" por "y" en el prompt
  (setq-default use-short-answers t))


;;__________________________________________________________
;; Show paren mode
(setq-default show-paren-delay 0
	      blink-matching-paren nil)
(show-paren-mode t)	  ;; Highlight couple parenthesis

;; Use cycle-spacing instead of just-one-space on M-SPC
(global-set-key [remap just-one-space] #'cycle-spacing)

;;__________________________________________________________
;; Isearch

(setq-default search-nonincremental-instead nil  ;; No incremental if enter & empty
	      lazy-highlight-initial-delay 0
	      isearch-allow-scroll t 	         ;; Permit scroll can be 'unlimited
	      isearch-lazy-count t
	      search-ring-max 64
	      regexp-search-ring-max 64
	      isearch-yank-on-move 'shift)       ;; Copy text from buffer with meta

(with-eval-after-load 'isearch
  (define-key isearch-mode-map
    [remap isearch-delete-char] #'isearch-del-char)

  (defun my/goto-match-beginning ()
    (when (and isearch-forward
	       (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))

  (add-hook 'isearch-mode-end-hook #'my/goto-match-beginning))

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
    (setq-local mouse-yank-at-point t)
    (setq-local transient-mark-mode nil)
    (display-fill-column-indicator-mode -1)
    (auto-fill-mode -1))

  (add-hook 'term-mode-hook #'my/term-mode-hook))

;;__________________________________________________________
;; tabs and tabbar
(setq-default tab-bar-tab-hints t  ;; show tab numbers
	      tab-bar-close-last-tab-choice 'tab-bar-mode-disable)

;;__________________________________________________________
;; minibuffers

;; (setq minibuffer-eldef-shorten-default t)
(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (setq gc-cons-threshold my/gc-cons-threshold)))

;;__________________________________________________________
;; gdb rectangles

(setq-default gdb-many-windows nil
	      gdb-show-main t)

;;__________________________________________________________
;;	Seleccionar con el mouse

(xterm-mouse-mode t) ;; mover el cursor al click
(setq-default mouse-sel-mode t ;; Mouse selection
	      mouse-scroll-delay 0)

(set-mouse-color "white")		;; Flechita del mouse en blanco

(when (fboundp 'mouse-wheel-mode)
  (setq-default mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control)))
		mouse-wheel-progressive-speed nil)
  (mouse-wheel-mode t))			;; scrolling con el mouse

(if (fboundp 'mouse-wheel-mode)
    (mouse-wheel-mode t))		;; scrolling con el mouse

(defun my/scroll-up-command (&optional arg)
  (interactive "^p")
  (scroll-up-command arg))

(defun my/scroll-down-command (&optional arg)
  (interactive "^p")
  (scroll-down-command arg))

(global-set-key [remap scroll-up-command] #'my/scroll-up-command)
(global-set-key [remap scroll-down-command] #'my/scroll-down-command)

;;__________________________________________________________
;; Ediff
(setq-default ediff-window-setup-function #'ediff-setup-windows-plain
	      ediff-split-window-function #'split-window-horizontally)

;;__________________________________________________________
;; My program's mode hooks

(with-eval-after-load 'prog-mode
  (defun my/prog-mode-hook ()
    "Some hooks only for prog mode."
    ;;(electric-indent-mode t)	;; On by default
    (electric-pair-local-mode 1)	;; Autoannadir parentesis
    (which-function-mode 1)	;; Shows the function in spaceline

    ;;(define-key global-map (kbd "RET") 'newline-and-indent)
    ;;(electric-indent-local-mode t)
    (setq-local show-trailing-whitespace t))

  (add-hook 'prog-mode-hook #'my/prog-mode-hook))

(defun my/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (<= oldpos (point))
	 (/= (line-beginning-position) oldpos)
	 (beginning-of-line))))

(global-set-key [remap move-beginning-of-line] #'my/smart-beginning-of-line)

;;__________________________________________________________
;; C common mode (for all c-like languajes)

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

;; (defun my/c-semi&comma ()
;;   (assq 'class-close c-syntactic-context)
;;   )

(setq-default c-default-style
	      '((java-mode . "java")
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
    "My hook for C and C++."
    (c-toggle-auto-newline 1)
    (c-toggle-cpp-indent-to-body 1)
    (c-ms-space-for-alignment-mode 1)
    (message "Loaded my/c-mode-common"))

  (add-hook 'c-mode-common-hook #'my/c-mode-common-hook))

;;__________________________________________________________
;; sh mode

(with-eval-after-load 'sh-script
  (defvaralias 'sh-basic-offset 'tab-width)
  (defun my/sh-mode-hook ()
    "My term mode hook."
    (setq-local indent-tabs-mode t))

  (add-hook 'sh-mode-hook #'my/sh-mode-hook))

;;__________________________________________________________
;; Move split keybindings
(global-set-key (kbd "C-x <left>")  #'windmove-left)
(global-set-key (kbd "C-x <right>") #'windmove-right)
(global-set-key (kbd "C-x <up>")    #'windmove-up)
(global-set-key (kbd "C-x <down>")  #'windmove-down)

;;__________________________________________________________
;; Undo
(global-set-key [remap undo] #'undo-only)
(global-set-key (kbd "C-M-_") #'undo-redo)


;;__________________________________________________________
;; Winner mode
(setq-default winner-dont-bind-my-keys t)
(winner-mode t)
(global-set-key (kbd "C-x w r")  #'winner-undo)
(global-set-key (kbd "C-x w u")  #'winner-redo)

;;__________________________________________________________
;; Eldoc

(setq-default eldoc-idle-delay 2                   ;; default 0.5
	      eldoc-print-after-edit t             ;; only show after edit
	      eldoc-echo-area-display-truncation-message nil) ;; Not verbose when truncated

;;__________________________________________________________
;; Transpose
(global-set-key (kbd "C-M-<left>")
		(lambda () (interactive) (transpose-words -1)))

(global-set-key (kbd "C-M-<right>") #'transpose-words)

(global-set-key (kbd "M-<left>")
		(lambda () (interactive) (transpose-chars -1)))

(global-set-key (kbd "M-<right>") #'transpose-chars)

;;__________________________________________________________
;; Abbrev mode
(abbrev-mode t)

;;__________________________________________________________
;; ibuffer
;;(defalias 'list-buffers 'ibuffer)
(global-set-key [remap list-buffers] #'ibuffer)
(setq-default ibuffer-default-sorting-mode 'alphabetic)

;;__________________________________________________________
;; dired
(setq-default dired-recursive-copies 'top   ;; Always ask recursive copy
	      dired-recursive-deletes 'top  ;; Always ask recursive delete
	      dired-dwim-target t	   ;; Copy in split mode with p
	      dired-auto-revert-buffer t
	      dired-listing-switches "-alh")


(with-eval-after-load 'dired
  (defun my/dired-hook ()
    "My dired hook."
    (require 'dired-x)
    (put 'dired-find-alternate-file 'disabled nil)
    (define-key dired-mode-map [remap dired-find-file] #'dired-find-alternate-file)  ; was dired-advertised-find-file
    (define-key dired-mode-map [remap dired-up-directory] ; was dired-up-directory
		(lambda ()
		  (interactive)
		  (find-alternate-file ".."))))

  (add-hook 'dired-load-hook #'my/dired-hook))

(provide 'init)
;;; init.el ends here
