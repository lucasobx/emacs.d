;;; init.el --- Emacs --- -*- lexical-binding: t; -*-

;;; =====================================================================
;;; Commentary:
;;;
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
  (add-to-list 'default-frame-alist '(height . 42))
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (defun display-startup-echo-area-message ()
    (message ""))

  :config
  ;; ignore system buffers during navigation
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; save manual customizations to a separate file instead of cluttering `init.el'.
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)
  
  (set-face-attribute 'default nil :font "PragmataPro Liga" :height 140)
  
  ;; (set-face-attribute 'default nil
  ;;                     :font "TX-02"
  ;;                     :height 148
  ;;                     :weight 'normal
  ;;                     :slant 'normal
  ;;                     :width 'extra-condensed)
  
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  (setq global-auto-revert-non-file-buffers t)
  (put 'narrow-to-region 'disabled nil)
  (setq split-width-threshold 0)
  (global-visual-line-mode t)
  (electric-pair-mode t)

  :bind
  ("C-c i" . (lambda () (interactive)
               (find-file (locate-user-emacs-file "init.el"))))

  ("C-x a" . (lambda () (interactive)
               (set-mark-command nil)
               (move-beginning-of-line nil)))

  ("C-x e" . (lambda () (interactive)
               (set-mark-command nil)
               (move-end-of-line nil)))

  ("<escape>" . keyboard-escape-quit)
  ("C-=" . text-scale-increase)
  ("C--" . text-scale-decrease)
  ("C-<tab>" . other-window)
  ("C-c d" . duplicate-line)
  ("C-;" . comment-line))

;;; =====================================================================
;;; UI

;; bespoke-themes
(use-package bespoke-themes
  :elpaca (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  (setq bespoke-set-theme 'dark)
  ;; (load-theme 'bespoke t)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :weight 'normal)
  (set-face-attribute 'font-lock-comment-face nil :weight 'normal))

;; punpun-themes
(use-package punpun-themes
  :ensure t
  :config
  ;; (load-theme 'punpun-dark t)
  (let ((gray "#454545"))
    (set-face-attribute 'font-lock-comment-face nil :foreground gray :weight 'normal)
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground gray :weight 'normal)))

;; doric-themes
(use-package doric-themes
  :ensure t
  :config
  (load-theme 'doric-dark)
  (let ((background-color "#0A0A0A"))
    (set-face-attribute 'default nil :background background-color))
  (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'normal)
  (set-face-attribute 'font-lock-builtin-face nil :slant 'normal)
  (set-face-attribute 'font-lock-type-face nil :slant 'normal))

;; ligature
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode
                          '("!=" "!==" "!≡" "!≡≡" "!=<" "#(" "#_" "#{" "#?" "##"
                            "#_(" "#[" "%=" "&%" "&&" "&+" "&-" "&/" "&=" "&&&"
                            "$>" "(|" "*>" "++" "+++" "+=" "+>" "++=" "--" "-<"
                            "-<<" "-=" "->" "->>" "---" "-->" "-+-" "-\\/" "-|>" "-<|"
                            "->-" "-<-" "-|" "-||" "-|:" ".=" "//=" "/=" "/==" "/-\\"
                            "/-:" "/->" "/=>" "/-<" "/=<" "/=:" ":=" ":≡" ":=>"
                            ":-\\" ":=\\" ":-/" ":=/" ":-|" ":=|" ":|-" ":|=" "<$>"
                            "<*" "<*>" "<+>" "<-" "<<=" "<=>" "<>" "<|>" "<<-" "<|"
                            "<=<" "<~" "<~~" "<<~" "<$" "<+" "<!>" "<@>" "<#>" "<%>"
                            "<^>" "<&>" "<?>" "<.>" "</>" "<\\>" "<\">" "<:>" "<~>" "<**>"
                            "<<^" "<=" "<->" "<!--" "<--" "<~<" "<==>" "<|-" "<||" "<<|"
                            "<-<" "<-->" "<<==" "<==" "<-\\" "<-/" "<=\\" "<=/" "=<<"
                            "==" "===" "==>" "=>" "=~" "=>>" "=~=" "==>>" "=>=" "=<="
                            "=<" "==<" "=<|" "=/=" "=/<" "=|" "=||" "=|:" ">-" ">>-"
                            ">>=" ">=>" ">>^" ">>|" ">!=" ">->" ">==" ">=" ">/=" ">-|"
                            ">=|" ">-\\" ">=\\" ">-/" ">=/" ">λ=" "?." "^=" "^<<" "^>>"
                            "\\=" "\\==" "\\/=" "\\-/" "\\-:" "\\->" "\\=>" "\\-<" "\\=<"
                            "\\=:" "|=" "|>=" "|>" "|+|" "|->" "|-->" "|=>" "|==>" "|>-"
                            "|<<" "||>" "|>>" "|-" "||-" "||=" "|)" "|]" "|-:" "|=:"
                            "|-<" "|=<" "|--<" "|==<" "~=" "~>" "~~>" "~>>" "[[" "[|"
                            "_|_" "]]" "≡≡" "≡≡≡" "≡:≡" "≡/" "≡/≡" ";;" ";;;" ";;;;" ";;;;;"
                            "=>>>" "<<<=" "=!=" "<---" "<--->" "--->"
                            "- [v]" "- [x]" "- [-]" "- [ ]"))
  (global-ligature-mode t))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; doom-modeline
 (use-package doom-modeline
  :ensure t
  :demand t
  :custom
  (doom-modeline-height 30)
  :config
  ;; faces
  ;; (set-face-attribute 'mode-line nil :font "TX-02 SemiLight" :height 110)
  (set-face-attribute 'mode-line nil :font "PragmataPro Liga" :height 132)
  ;; (set-face-attribute 'mode-line-inactive nil :font "TX-02 SemiLight" :height 110)
  (set-face-attribute 'mode-line-inactive nil :font "PragmataPro Liga" :height 132)
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

;; indent-guide
(use-package indent-guide
  :ensure t
  :defer t
  :hook
  (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))

;;;; =====================================================================
;;;; Navigation

;; dired
(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-lah --group-directories-first")
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     (".*" "open" "xdg-open")))
  (dired-kill-when-opening-new-dired-buffer t))

;; avy (jump anywhere on screen based on characters, lines or words)
(use-package avy
  :ensure t
  :init
  (setq avy-keys-alist
      `((avy-goto-char-2 . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))
        (avy-goto-char-timer . (?j ?k ?l ?f ?s ?d ?e ?r ?u ?i))))
  :bind
  ("C-f" . avy-goto-char-2)
  ("C-:" . avy-goto-char-timer))

;; move-text
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; restart-emacs
(use-package restart-emacs
  :ensure t
  :bind (("C-x M-c" . restart-emacs)))

;;; =====================================================================
;;; LSP

;; flymake (syntax checking)
(use-package flymake
  :ensure nil
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info))))

;; eglot
(use-package eglot
  :ensure nil
  :hook ((lua-mode . eglot-ensure)
         (lua-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               `((lua-mode lua-ts-mode) . ("/home/linuxbrew/.linuxbrew/bin/lua-language-server"))))

;; lua-mode
(use-package lua-mode :ensure t)

;; lua-ts-mode
(use-package lua-ts-mode
  :ensure nil
  :config
  (setq treesit-font-lock-level 4))

;; treesit-auto
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

;;; =====================================================================
;;; Completion

;; vertico (completion system)
(use-package vertico :ensure t :init (vertico-mode))

;; marginalia (add annotations to the minibuffer completions)
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

;; orderless (fuzzy completion)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;; consult
(use-package consult
  :ensure t
  :after vertico
  :bind
  ;; C-c bindings in `mode-specific-map'
  ("C-c M-x" . consult-mode-command)
  ;; C-x bindings in `ctl-x-map'
  ("C-x b" . consult-buffer)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-find)
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines))

;; embark
(use-package embark
  :ensure t
  :defer t)

;; embark-consult
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; corfu (completion popup)
(use-package corfu
  :ensure t
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :custom
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 50)
  (corfu-min-width 50)
  :init
  (global-corfu-mode t)
  (corfu-popupinfo-mode))

;; nerd-icons-corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; =====================================================================
;;; Keybindings

;; which-key
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
    "C-c d" "duplicate line"
    "C-x e" "select to end"
    "C-x a" "select to start"))

;; devil-mode
(use-package devil
  :ensure (devil :host github :repo "fbrosda/devil" :branch "which-key-support")
  :demand t
  :custom
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  :bind
  ;;([remap describe-key] . devil-describe-key)
  :config
  (global-devil-mode))

;;; =====================================================================
;;; Productivity

;; org-mode
(use-package org :ensure nil :defer t)

;;; =====================================================================
;;; Misc

;; helpful
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-c C-d" . helpful-at-point)
   ("C-h F" . helpful-function)))

;;; init.el ends here
