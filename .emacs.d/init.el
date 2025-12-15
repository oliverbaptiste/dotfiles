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

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Overwrite selected text
(delete-selection-mode t)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(load-theme 'modus-operandi t) ;; Load Emacs 28+ Modus theme
(global-hl-line-mode t) ;; Highlight current line

;; PACKAGE INSTALLATION SETUP

;; Initialize package.el
(require 'package)

;; Declare package repos
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")))
(package-initialize)

;; Declare packages
(setq my-packages
      '(edit-indirect
	editorconfig
	groovy-mode
	magit
	markdown-mode
	racket-mode
	sml-mode
	web-mode))

;; Install packages
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Enable EditorConfig
(editorconfig-mode t)
