;; Move all custom-* stuff into custom-file.el.
;; Do not put them in init.el.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Prevent autosave mess
(make-directory "~/.emacs.d/autosaves/" t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))

;; Enable a color theme
(load-theme 'modus-operandi) ;; Light mode
;; (load-theme 'modus-vivendi) ;; Dark mode

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Disable tool bar
(tool-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Overwrite selected text
(delete-selection-mode t)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun system-has-gls ()
  "Check if the system has 'gls' installed."
  (stringp (shell-command-to-string "command -v gls")))

(when (eq system-type 'darwin) ;; Check if on macOS (Darwin)
  (if (system-has-gls)
      ;; If 'gls' is available, use it for dired listing
      (setq insert-directory-program "gls"
            dired-listing-switches "-al --group-directories-first")
    ;; If 'gls' is not available, disable 'ls-lisp' for dired listing
    (setq dired-use-ls-dired nil)))

;; PACKAGE INSTALLATION SETUP
;; Source: https://stackoverflow.com/questions/55038594/setting-up-emacs-on-new-machine-with-init-el-and-package-installation

;; Declare package repos
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; Initialize package.el
(require 'package)
(package-initialize)
(package-refresh-contents) ;; After first run, comment out this line

;; Declare packages
(setq my-packages
      '(edit-indirect
	groovy-mode
	markdown-mode
	web-mode))

;; Install packages
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))
