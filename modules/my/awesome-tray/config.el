;;; my/awesome-tray/config.el -*- lexical-binding: t; -*-

;; Configuration for hide-mode-line and awesome-tray packages

(use-package! hide-mode-line
  :config
  (setq hide-mode-line-excluded-modes '())
  (add-hook! '(treemacs-mode-hook
               treemacs-git-mode-hook)
             #'hide-mode-line-mode)

  ;; magit-diff/status/log 都继承 magit-mode，只需挂 magit-mode-hook
  (add-hook! 'magit-mode-hook
    (defun +my-hide-magit-mode-line-h ()
      (hide-mode-line-mode))))


(use-package! awesome-tray
  :after hide-mode-line
  :hook (doom-after-init . awesome-tray-mode)
  :config
  ;; Configuration for awesome-tray appearance
  (setq awesome-tray-mode-line-active-color
        (if (display-graphic-p) "#5B6268" "color-241"))
  (when (display-graphic-p)
    (setq awesome-tray-mode-line-height 0.1))
  (setq awesome-tray-active-modules
        '("evil" "buffer-name" "file-path" "git" "mode-name"))
  (add-hook! 'doom-load-theme-hook #'awesome-tray-mode #'hide-mode-line-mode))