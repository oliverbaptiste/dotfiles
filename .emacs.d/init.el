;;; init.el --- Emacs settings for ogab@ogab-air-15.local -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(make-directory "~/.emacs.d/autosaves/" t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/\\1" t)))

(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups/")))

(setq custom-file
      (locate-user-emacs-file "custom-file.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(load-theme 'newcomers-presets t)

(if (display-graphic-p)
    (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t)
  (menu-bar-mode -1)
  (xterm-mouse-mode t))

(global-hl-line-mode t)

(require 'package)
(package-initialize)

(use-package edit-indirect
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package sml-mode
  :ensure t
  :mode ("\\.sml\\'" . sml-mode))

(provide 'init)
;;; init.el ends here
