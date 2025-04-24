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

(use-package xclip
  :config
  (xclip-mode 1)
  (defun my/copy-to-system ()
    "Copy region to system clipboard using xclip."
    (interactive)
    (when (use-region-p)
      (call-process-region (region-beginning) (region-end)
                           "xclip" nil nil nil "-selection" "clipboard")
      (message "Copied to system clipboard")))
  (defun my/paste-from-system ()
    "Paste text from system clipboard using xclip."
    (interactive)
    (insert (shell-command-to-string "xclip -o -selection clipboard")))
  (global-set-key (kbd "C-c c") 'my/copy-to-system)
  (global-set-key (kbd "C-c v") 'my/paste-from-system))

(use-package zig-mode)

;; UI Tweaks
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode)

;; Clipboard settings
(setq select-enable-clipboard t)
(setq select-enable-primary t)

