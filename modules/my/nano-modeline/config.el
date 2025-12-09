;;; my/nano-modeline/config.el -*- lexical-binding: t; -*-

(use-package! nano-modeline
  :demand t
  :config
  ;; 清空默认的 mode-line 格式
  (setq-default mode-line-format nil)

  ;; 确保没有其他地方设置 header-line
  (setq-default header-line-format nil)

  ;; 添加钩子来确保 header-line 被禁用
  (defun my/force-mode-line ()
    "Force mode-line and disable header-line"
    (setq-local header-line-format nil)
    (when (bound-and-true-p nano-modeline-mode)
      (setq-local mode-line-format nano-modeline-format)))

  ;; 应用 nano-modeline 到各种模式
  (add-hook 'prog-mode-hook            (lambda () (nano-modeline-prog-mode) (my/force-mode-line)))
  (add-hook 'text-mode-hook            (lambda () (nano-modeline-text-mode) (my/force-mode-line)))
  (add-hook 'org-mode-hook             (lambda () (nano-modeline-org-mode) (my/force-mode-line)))
  (add-hook 'elfeed-show-mode-hook     (lambda () (nano-modeline-elfeed-entry-mode) (my/force-mode-line)))
  (add-hook 'elfeed-search-mode-hook   (lambda () (nano-modeline-elfeed-search-mode) (my/force-mode-line)))
  (add-hook 'term-mode-hook            (lambda () (nano-modeline-term-mode) (my/force-mode-line)))
  (add-hook 'messages-buffer-mode-hook (lambda () (nano-modeline-message-mode) (my/force-mode-line)))
  (add-hook 'org-capture-mode-hook     (lambda () (nano-modeline-org-capture-mode) (my/force-mode-line)))
  (add-hook 'org-agenda-mode-hook      (lambda () (nano-modeline-org-agenda-mode) (my/force-mode-line)))

  ;; 默认启用 nano-modeline text mode
  (nano-modeline-text-mode t)
  (setq nano-modeline-window-dedicated-symbol '("● " . "")))
  
