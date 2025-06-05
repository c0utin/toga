;; MELPA setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ========================
;; Appearance & Theme
;; ========================
(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

;; ========================
;; Clipboard integration
;; ========================
(use-package xclip
  :config
  (xclip-mode 1))

(defun my/copy-to-clipboard (beg end)
  "Copy region to system clipboard using xclip."
  (interactive "r")
  (when (use-region-p)
    (call-process-region beg end "xclip" nil nil nil "-selection" "clipboard")
    (message "Copied to clipboard")))

(defun my/paste-from-clipboard ()
  "Paste from system clipboard using xclip."
  (interactive)
  (let ((clipboard-text (shell-command-to-string "xclip -o -selection clipboard")))
    (insert clipboard-text)))

(global-set-key (kbd "C-c y") 'my/copy-to-clipboard)
(global-set-key (kbd "C-c p") 'my/paste-from-clipboard)

;; ========================
;; Git
;; ========================
(use-package magit)

;; ========================
;; Programming languages
;; ========================
(use-package zig-mode)

(use-package rust-mode)

(use-package go-mode)

(use-package js2-mode
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-mode)
         ("\\.ts\\'"  . js2-mode))
  :config
  (setq js2-basic-offset 2))

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'" "\\.drv\\'")
  :commands (nix-repl nix-shell-unpack nix-shell-configure nix-shell-build)
  :config
  ;; dummy definitions if missing
  (unless (fboundp 'nix-read-attr)
    (defun nix-read-attr (&rest _) (error "nix-read-attr not implemented")))
  (unless (fboundp 'nix-read-file)
    (defun nix-read-file (&rest _) (error "nix-read-file not implemented"))))

;; ========================
;; UI Tweaks
;; ========================
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode)

;; ========================
;; Input method
;; ========================
(activate-input-method "latin-1-postfix")
(setq default-input-method "latin-1-postfix")

;; ========================
;; Dired enhancements
;; ========================
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; ========================
;; File settings
;; ========================
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
