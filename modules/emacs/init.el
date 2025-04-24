(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode)

;; melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;VIM
(require 'evil)
(evil-mode 1)

;;global
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("e13beeb34b932f309fb2c360a04a460821ca99fe58f69e65557d6c1b10ba18c7"
     default))
 '(package-selected-packages '(evil gruber-darker-theme magit xclip zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;xclip
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

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
(global-set-key (kbd "C-c v") 'my/paste-from-system)

