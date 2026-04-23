;; Store automatic customization options elsewhere
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

;; On graphical display
(if (display-graphic-p)
    (load-theme 'modus-operandi) ;; default to light theme 
  ;; On text terminal
  (load-theme 'modus-vivendi) ;; default to dark theme
  (menu-bar-mode -1)) ;; and hide menu bar

;; Overwrite selected text
(delete-selection-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Enable which-key
(which-key-mode t)           

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

