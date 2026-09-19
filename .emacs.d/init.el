;; -*- lexical-binding: t; -*-

;; Store customization options in a separate file
(setq custom-file
      (locate-user-emacs-file "custom-file.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Organize auto-save & backup files
(make-directory "~/.emacs.d/autosaves/" t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))

;; Don't show startup screen
(setq inhibit-startup-message t)

(load-theme 'newcomers-presets t)

;; Highlight current line
(global-hl-line-mode t)

;; Install and set up packages
(require 'package)
(package-initialize)

;; On graphical display
(if (display-graphic-p)
    (progn
      ;; Show menu bar
      (menu-bar-mode 1)
      ;; Use light theme
      (load-theme 'modus-operandi t))
  ;; In text terminal
  (progn
    ;; Hide menu bar
    (menu-bar-mode -1)
    ;; Use dark theme
    (use-package dracula-theme
      :ensure t
      :config
      (load-theme 'dracula t))))

(use-package edit-indirect
  :ensure t)

(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'"))
              
(use-package sml-mode
  :ensure t
  :mode ("\\.sml\\'" . sml-mode))

(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'" . racket-mode))

(use-package web-mode
  :ensure t
  :mode
  (("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.vue\\'" . web-mode)))

(provide 'init)
;;; init.el ends here

