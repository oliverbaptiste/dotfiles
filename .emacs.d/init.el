;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom-file.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Prevent autosave mess
(make-directory "~/.emacs.d/autosaves/" t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Load Emacs 28+ Modus theme
(load-theme 'modus-operandi t)

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Overwrite selected text
(delete-selection-mode t)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-hl-line-mode t) ;; Highlight current line

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
  
(straight-use-package 'use-package)

(use-package company
  :straight t
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; Provide instant autocompletion.
  (setq company-idle-delay 0.3)

  ;; Use company mode everywhere.
  (global-company-mode t))

;; Display possible completions at all places
(use-package ido-completing-read+
  :straight t
  :config
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-mode t)
  (ido-everywhere t)
  ;; This allows partial matches, e.g. "uzh" will match "Ustad Zakir Hussain"
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  ;; Includes buffer names of recently opened files, even if they're not open now.
  (setq ido-use-virtual-buffers t)
  :diminish nil)

;; Enhance M-x to allow easier execution of commands
(use-package smex
  :straight t
  ;; Using counsel-M-x for now. Remove this permanently if counsel-M-x works better.
  :disabled t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  :bind ("M-x" . smex))

(use-package edit-indirect
  :straight t)
  
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package go-mode
  :straight t
  :mode
  (("\\.go\\'" . go-mode)))

(use-package groovy-mode
  :straight t)

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package pandoc-mode
  :straight t)
  
(use-package racket-mode
  :straight t)

(use-package sml-mode
  :straight t)
  
(use-package web-mode
  :straight t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package which-key
  :straight t
  :config
  (which-key-mode))
