;;; MELPA and package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; Remap Super key as Meta
(setq x-super-keysym 'meta)

;;; Theme
(use-package color-theme-modern
  :config
  (load-theme 'word-perfect t))

;; (use-package gruber-darker-theme
;;   :config
;;   (load-theme 'gruber-darker t))

;;; Git integration
(use-package magit)

;;; Clipboard integration
(use-package xclip
  :if (display-graphic-p) ;; Only enable xclip in graphical mode
  :config
  (xclip-mode 1))

;; Function to copy text to system clipboard
(defun my/copy-to-clipboard (beg end)
  "Copy the selected region to the system clipboard."
  (interactive "r")
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties beg end)))
      (with-temp-buffer
        (insert text)
        (if (fboundp 'gui-set-selection)
            (gui-set-selection 'CLIPBOARD text)
          ;; fallback to xclip if gui method not available
          (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection" "clipboard"))))
    (message "Copied to clipboard.")))

;; Function to paste from system clipboard
(defun my/paste-from-clipboard ()
  "Paste from system clipboard into buffer."
  (interactive)
  (let ((text (if (fboundp 'gui-get-selection)
                  (gui-get-selection 'CLIPBOARD)
                (shell-command-to-string "xclip -o -selection clipboard"))))
    (insert text)))

;; Keybindings for clipboard actions (use standard Emacs convention: C-c + key)
(global-set-key (kbd "C-c y") #'my/copy-to-clipboard)
(global-set-key (kbd "C-c p") #'my/paste-from-clipboard)

;;; Language support
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

;;; UI preferences
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;;; Input method (for typing special characters)
(activate-input-method "latin-1-postfix")
(setq default-input-method "latin-1-postfix")

;;; Auto-refresh dired
(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; Disable lock, backup, and autosave files
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
