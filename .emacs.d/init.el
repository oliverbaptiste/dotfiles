;; Enable a color theme
(load-theme 'modus-operandi)

;; Move all custom-* stuff into custom-file.el.
;; Do not put them in init.el.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Prevent autosave mess
;; Sources:
;; https://www.victorquinn.com/emacs-prevent-autosave-mess
;; https://snarfed.org/gnu_emacs_backup_files
(make-directory "~/.emacs.d/autosaves/" t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Disable tool bar
(tool-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Overwrite selected text
;; Source: http://pragmaticemacs.com/emacs/overwrite-selected-text/
(delete-selection-mode t)

;; Add line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
