;;; my/awesome-tray/config.el -*- lexical-binding: t; -*-

;; Configuration for hide-mode-line and awesome-tray packages

(use-package! hide-mode-line
  :config
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

    (add-hook hook #'+my-hide-special-mode-line-h))

  (add-hook! 'magit-post-refresh-hook #'hide-mode-line-mode))


(use-package! awesome-tray
  :config
  ;; Configuration for awesome-tray appearance
  (setq awesome-tray-mode-line-active-color "#5B6268")
  (setq awesome-tray-mode-line-height 0.1) ; Set height to minimal
  (setq awesome-tray-active-modules
        '("evil" "buffer-name" "file-path" "git" "mode"))

  ;; Update modules when awesome-tray is already enabled
  (add-hook 'doom-load-theme-hook #'awesome-tray-update)

  ;; Fix "double line" and "height" issues (critical step)
  ;; Doom themes add :box to mode-line, we need to remove it
  (defun fix-awesome-tray-modeline-face ()
    (set-face-attribute 'mode-line nil
                        :height 0.1        ; minimal height
                        :box nil           ; 【key】remove original box/border
                        :underline nil     ; remove underline (if you want complete hiding)
                        :overline nil      ; remove overline
                        :background awesome-tray-mode-line-active-color)

    (set-face-attribute 'mode-line-inactive nil
                        :height 0.1
                        :box nil
                        :underline nil
                        :overline nil
                        :background awesome-tray-mode-line-active-color))

  ;; Apply fix when loading awesome-tray
  (add-hook 'awesome-tray-mode-hook #'fix-awesome-tray-modeline-face)

  ;; Enable the plugin
  (awesome-tray-mode 1))

;; Hook to ensure both modes are active when window configuration changes
(add-hook! 'window-configuration-change-hook #'hide-mode-line-mode #'awesome-tray-mode)
