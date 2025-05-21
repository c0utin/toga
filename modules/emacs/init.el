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

;; Pkts
(use-package evil
  :config
  (evil-mode 1))

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package magit)

;; Clipboard settings
;; Enable xclip to sync with system clipboard
(use-package xclip
  :config
  (xclip-mode 1))

;; Functions for manual copy/paste to system clipboard
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

;; General keybindings with evil + leader
(use-package general
  :config
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "y" 'my/copy-to-clipboard
   "p" 'my/paste-from-clipboard))

;; languages
(use-package zig-mode)

(use-package rust-mode)

(use-package go-mode)

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"  . js2-mode)   ; Force .js \u2192 js2-mode
         ("\\.jsx\\'" . js2-mode)   ; JSX support
         ("\\.ts\\'"  . js2-mode))  ; TypeScript (if already working)
  :config
  (setq js2-basic-offset 2))        ; Optional: Set indentation

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))

;; UI Tweaks
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode)

;; input method
(activate-input-method "latin-1-postfix")
(setq default-input-method "latin-1-postfix")

;; DIRED
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
