;; Default typeface
(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 150
                    :weight 'normal
                    :width 'normal)

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

;; Scratch buffer defaults to org-mode with no message
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

;; Use MELPA for package archive
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; Make sure that use-package is loaded
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Move all custom-* stuff into custom-file.el.
;; Do not put them in init.el.
(setq custom-file "~/.emacs.d/custom-file.el")

;; Prevent autosave mess
;; Source: https://www.victorquinn.com/emacs-prevent-autosave-mess
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))

;; Load themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Load Dracula theme
(load-theme 'dracula t)

;; Enable Dracula theme for GUI Emacs only
(if (display-graphic-p)
    (enable-theme 'dracula)
  (disable-theme 'dracula))

;; Do not show the startup screen
(setq inhibit-startup-message t)

;; Disable tool bar
(tool-bar-mode -1)

;; Disable menu bar in non-GUI Emacs
;; Source: https://emacs.stackexchange.com/questions/29441/how-do-i-disable-menu-bar-mode-only-for-tty-frames
(unless (display-graphic-p)
  (menu-bar-mode -1))

;; Highlight current line
(global-hl-line-mode t)

;; Overwrite selected text
;; Source: http://pragmaticemacs.com/emacs/overwrite-selected-text/
(delete-selection-mode t)

;; Add line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; START TABS CONFIG
;; Source: https://dougie.io/emacs/indentation/

;; Create a variable for our preferred tab width
(setq custom-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun disable-tabs ()
  (setq indent-tabs-mode nil)
  (setq tab-width custom-tab-width))
(defun enable-tabs ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Disable Tabs in programming modes
(add-hook 'prog-mode-hook 'disable-tabs)

;; Disable electric-indent in org-mode
(add-hook 'org-mode-hook
	  (lambda () (electric-indent-local-mode -1)))

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; Do not indent source blocks in org-mode
(setq org-edit-src-content-indentation 0)

;; PACKAGES
(use-package groovy-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package edit-indirect
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package sml-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode
  (
   ".html?$"
   )
  :config
  (setq
   web-mode-attr-indent-offset custom-tab-width
   web-mode-css-indent-offset custom-tab-width
   web-mode-code-indent-offset custom-tab-width
   web-mode-markup-indent-offset custom-tab-width
   web-mode-offsetless-elements '("html" "head" "body" "script")
   web-mode-script-padding 0
   web-mode-style-padding 0
   ))
