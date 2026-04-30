;;; my/awesome-tray/config.el -*- lexical-binding: t; -*-

;; Configuration for hide-mode-line and awesome-tray packages

(use-package! hide-mode-line
  :config
  (setq hide-mode-line-excluded-modes '())
  (add-hook! '(treemacs-mode-hook
               treemacs-git-mode-hook)
             #'hide-mode-line-mode)

  (defun +my-hide-special-mode-line-h ()
    (when (or (string-match-p "^\\*Treemacs" (buffer-name))
              (derived-mode-p 'magit-mode 'magit-diff-mode 'magit-status-mode 'magit-log-mode))
      (hide-mode-line-mode)))

  (dolist (hook '(treemacs-mode-hook
                  magit-mode-hook
                  magit-diff-mode-hook
                  magit-status-mode-hook
                  magit-log-mode-hook))

    (add-hook! hook #'+my-hide-special-mode-line-h)))


(use-package! awesome-tray
  :after hide-mode-line
  :hook (doom-after-init . awesome-tray-mode)
  :config
  ;; Configuration for awesome-tray appearance
  (setq awesome-tray-mode-line-active-color "#5B6268")
  (setq awesome-tray-mode-line-height 0.1) ; Set height to minimal
  (setq awesome-tray-active-modules
        '("evil" "buffer-name" "file-path" "git" "mode-name"))
  (add-hook! 'doom-load-theme-hook #'awesome-tray-mode #'hide-mode-line-mode))
