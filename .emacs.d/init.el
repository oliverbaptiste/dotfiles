;; Set up default fonts
(cond
 ((find-font (font-spec :name "Fira Mono"))
  (set-frame-font "Fira Mono-14"))
 ((find-font (font-spec :name "Source Code Pro"))
  (set-frame-font "Source Code Pro-14"))
 ((find-font (font-spec :name "SF Mono"))
  (set-frame-font "SF Mono-14"))
 ((find-font (font-spec :name "Menlo"))
  (set-frame-font "Menlo")))

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

;; Highlight current line
(global-hl-line-mode t)

;; Overwrite selected text
;; Source: http://pragmaticemacs.com/emacs/overwrite-selected-text/
(delete-selection-mode t)

;; Add line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Emacs package management with straight.el and use-package
;; Source: https://jeffkreeftmeijer.com/emacs-straight-use-package/

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; Install packages
(use-package edit-indirect
  :straight t)
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))
(use-package elpher
  :straight t)
(use-package groovy-mode
  :straight t)
(use-package magit
  :straight t)
(use-package markdown-mode
  :straight t)
(use-package web-mode
  :straight t)
