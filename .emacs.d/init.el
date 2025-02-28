;; Mac keybindings
(cond ((eq system-type 'darwin)
       (setq mac-command-modifier 'super
	     mac-option-modifier 'meta)
       (global-set-key (kbd "s--") (kbd "C-x C--")) ;; Decrease font size
       (global-set-key (kbd "s-=") (kbd "C-x C-=")) ;; Increase font size
       (global-set-key (kbd "s-0") (kbd "C-x C-0")) ;; Reset font size
       (global-set-key (kbd "s-z") (kbd "C-/")) ;; Undo
       (global-set-key (kbd "s-x") (kbd "C-w")) ;; Cut
       (global-set-key (kbd "s-c") (kbd "M-w")) ;; Copy
       (global-set-key (kbd "s-v") (kbd "C-y")) ;; Paste
       (global-set-key (kbd "s-a") (kbd "C-x h")) ;; Select All
       (global-set-key (kbd "s-s") (kbd "C-x C-s")) ;; Save
 ))

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

;; Set default font
(set-face-attribute 'default nil
		    :family "JetBrains Mono"
		    :height 130
		    :weight 'normal
		    :width 'normal)

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Highlight current line
(global-hl-line-mode t)

;; Overwrite selected text
(delete-selection-mode t)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; PACKAGE INSTALLATION SETUP

;; Initialize package.el
(require 'package)

;; Declare package repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Declare packages
(setq my-packages
      '(edit-indirect
	editorconfig
	elpher
	groovy-mode
	magit
	markdown-mode
	web-mode))

;; Install packages
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Load color theme if using Emacs GUI
(if (display-graphic-p)
    (load-theme 'modus-operandi t))

;; Enable EditorConfig
(editorconfig-mode t)
