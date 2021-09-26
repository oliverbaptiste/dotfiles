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

;; Set up package system and add MELPA repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Load themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load Dracula theme
(load-theme 'dracula t)
(if (display-graphic-p)
    (enable-theme 'dracula)
  (disable-theme 'dracula))

;; Do not show the startup screen
(setq inhibit-startup-message t)

;; Disable tool bar
(tool-bar-mode -1)

;; Highlight current line
(global-hl-line-mode t)

;; Change default typeface
(set-face-attribute 'default nil
		                :family "SF Mono"
		                :height 160
		                :weight 'normal
		                :width 'normal)

;; Add line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Scratch buffer defaults to org-mode with no message
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

;; Disable electric-indent in org-mode
(add-hook 'org-mode-hook
	        (lambda () (electric-indent-local-mode -1)))

;; Indent using 2 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Do not indent source blocks in org-mode
(setq org-edit-src-content-indentation 0)

;; css-mode setup
(setq css-indent-offset 2)

;; js-mode setup
(setq js-indent-level 2)

;; web-mode setup
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for web-mode."
  (setq web-mode-markup-indent-offset 2
	      web-mode-css-indent-offset 2
	      web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
	      web-mode-offsetless-elements '("html" "head" "body" "script")
	      web-mode-style-padding 0
	      web-mode-script-padding 0
	      web-mode-attr-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; Put autosave files (#foo#) and backup files (foo~) in ~/.emacs.d/.
 ;; Source: https://snarfed.org/gnu_emacs_backup_files
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 ;; create the autosave dir if necessary, since emacs won't.
 (make-directory "~/.emacs.d/autosaves/" t)
 
 '(custom-safe-themes
   '(default))
 '(package-selected-packages '(markdown-mode groovy-mode web-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
