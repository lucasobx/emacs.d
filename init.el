;;; init.el --- Emacs --- -*- lexical-binding: t; -*-
;;; =====================================================================
;;; Commentary:
;;; Code:

;; increases the garbage collection threshold
(setq gc-cons-threshold #x40000000)

;; set the maximum output size for reading process output
(setq read-process-output-max (* 1024 1024 4))

;; disables the default package manager
(setq package-enable-at-startup nil)

;; elpaca package manager
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
	(elpaca-use-package-mode))

;;; =====================================================================
;;; Emacs

(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (delete-by-moving-to-trash t)
  (create-lockfiles nil)
  (display-line-numbers-type 'relative)
  (history-length 25)
  (indent-tabs-mode nil)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 2)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (read-extended-command-predicate ;; filter M-x commands
   'command-completion-default-include-p)

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode -1)
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode 1)
  (file-name-shadow-mode 1)
  (column-number-mode t)
  (delete-selection-mode 1)
  (global-display-line-numbers-mode t)
  (tooltip-mode -1)
  (add-to-list 'default-frame-alist '(width . 128))
  (add-to-list 'default-frame-alist '(height . 36))
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (defun display-startup-echo-area-message ()
    (message ""))

  :config
  ;; ignore system buffers during navigation
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; save manual customizations to a separate file
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; fonts
  ;; (set-face-attribute 'default nil :font "PragmataPro Liga" :height 140)
  (set-face-attribute 'default nil
                      :font "TX-02" :height 142 :slant 'normal :width 'condensed)

  ;; other settings
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  (setq global-auto-revert-non-file-buffers t)
  (put 'narrow-to-region 'disabled nil)
  (setq split-width-threshold 0)
  (global-visual-line-mode t)
  (electric-pair-mode t)

  :bind
  ("C-c i" . (lambda () (interactive)
               (find-file (locate-user-emacs-file "init.el"))))

  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("C-<tab>" . other-window))

;;; =====================================================================
;;; UI

(use-package doric-themes
  :ensure t
  :config
  (load-theme 'doric-dark)
  (set-face-attribute 'default nil :background "#101010")
  (dolist (face '(font-lock-comment-face
                  font-lock-comment-delimiter-face
                  font-lock-type-face
                  font-lock-variable-name-face
                  font-lock-keyword-face
                  font-lock-function-name-face
                  font-lock-constant-face
                  font-lock-string-face))
    (set-face-attribute face nil :slant 'normal :weight 'normal))
  (set-face-attribute 'font-lock-builtin-face nil :slant 'normal)
  (set-face-attribute 'font-lock-variable-name-face nil :foreground "#a0c0d0")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#8b7099")
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#8b7099")
  (set-face-attribute 'font-lock-string-face nil :foreground "#9fbfe7"))

;; (use-package punpun-themes
;;   :ensure t
;;   :config
;;   (load-theme 'punpun-dark t)
;;   (set-face-attribute 'font-lock-variable-name-face nil :slant 'normal)
;;   (set-face-attribute 'font-lock-comment-face nil :foreground "#454545" :weight 'normal)
;;   (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#454545" :weight 'normal)
;;   (set-face-attribute 'font-lock-variable-name-face nil :foreground "#8b7099")
;;   (set-face-attribute 'font-lock-string-face nil :foreground "#7b94b3"))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

 (use-package doom-modeline
  :ensure t
  :demand t
  :custom
  (doom-modeline-height 30)
  :config
  ;; faces
  (set-face-attribute 'mode-line nil
                      :font "TX-02 Retina" :height 132 :slant 'normal :width 'semi-condensed)
  (set-face-attribute 'mode-line-inactive nil
                      :font "TX-02 Retina" :height 132 :slant 'normal :width 'semi-condensed)
  (set-face-attribute 'doom-modeline-urgent nil :weight 'normal)
  (set-face-attribute 'doom-modeline-warning nil :weight 'normal)
  (set-face-attribute 'doom-modeline-info nil :weight 'normal)
  (set-face-attribute 'doom-modeline-highlight nil :weight 'normal)
  (set-face-attribute 'doom-modeline-buffer-modified nil :weight 'normal)
  (set-face-attribute 'doom-modeline-buffer-major-mode nil :weight 'normal :slant 'normal)
  ;; modeline options
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-total-line-number t)
  (setq nerd-icons-scale-factor 1.0)
  (doom-modeline-mode t))

(use-package indent-guide
  :ensure t
  :defer t
  :hook
  (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))

;;;; =====================================================================
;;;; Navigation

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
      `((avy-goto-char-2 . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
        (avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  :bind
  ("C-f" . avy-goto-char-2)
  ("C-:" . avy-goto-char-timer))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package restart-emacs
  :ensure t
  :bind (("C-x M-c" . restart-emacs)))

;;; =====================================================================
;;; LSP

(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info))))

(use-package eglot
  :ensure nil
  :hook ((lua-mode . eglot-ensure)
         (lua-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               `((lua-mode lua-ts-mode) . ("lua-language-server"))))

(use-package lua-mode :ensure t)

(use-package lua-ts-mode
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;; =====================================================================
;;; Completion

(use-package vertico :ensure t :init (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (set-face-attribute 'marginalia-documentation nil :underline nil)
  (set-face-attribute 'marginalia-key nil :underline nil)
  (set-face-attribute 'marginalia-file-name nil :underline nil)
  (set-face-attribute 'marginalia-size nil :underline nil)
  (set-face-attribute 'marginalia-modified nil :underline nil)
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

(use-package consult
  :ensure t
  :after vertico)
  ;; :bind
  ;; ("C-c M-x" . consult-mode-command)
  ;; ("C-x b" . consult-buffer)
  ;; ("C-c s d" . consult-find)
  ;; ("C-c s c" . consult-locate)
  ;; ("C-c g"   . consult-grep)
  ;; ("C-c s G" . consult-git-grep)
  ;; ("C-c s r" . consult-ripgrep)
  ;; ("C-c l"   . consult-line)
  ;; ("C-c s L" . consult-line-multi)
  ;; ("C-c s k" . consult-keep-lines)
  ;; ("C-c s u" . consult-focus-lines))

(use-package embark
  :ensure t
  :defer t)

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :custom
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-min-width 30)
  (corfu-border-width 3)
  (corfu-popupinfo-max-width 50)
  (corfu-popupinfo-max-height 11)
  :init
  (global-corfu-mode t)
  (corfu-popupinfo-mode)
  :config
  (set-face-attribute 'corfu-default nil
                      :family "PragmataPro Liga"
                      :height 132)
  (set-face-attribute 'corfu-popupinfo nil
                      :family "PragmataPro Liga"
                      :height 132)
  (set-face-attribute 'corfu-border nil :background "#332d38"))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; =====================================================================
;;; Keybindings

(use-package which-key
  :ensure nil
  :init
  (which-key-mode t)
  :config
  (setq which-key-idle-delay 0.3
        which-key-add-column-padding 1
        which-key-min-display-lines 6
        which-key-separator " → ")
  (which-key-add-key-based-replacements
    "C-c i" "init.el"
    "C-x e" "select to end"
    "C-x a" "select to start"))

(use-package devil
  :ensure (devil :host github :repo "fbrosda/devil" :branch "which-key-support")
  :demand t
  :custom
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  :config
  (global-devil-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;;; =====================================================================
;;; Productivity

(use-package org :ensure nil :defer t)

;;; =====================================================================
;;; Misc

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h x" . helpful-command)
   ("C-c d" . helpful-at-point)
   ("C-h F" . helpful-function)))

;;; init.el ends here
