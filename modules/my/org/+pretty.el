;;; my/org/+pretty.el -*- lexical-binding: t; -*-

;; (use-package! org-superstar
;;   :hook ((org-mode . org-superstar-mode)))

(use-package! valign
  :hook ((org-mode . valign-mode)
         (markdown-mode-hook . valign-mode))
  :config
  ;; 基础美化设置，与 valign 兼容
  (setq org-highlight-latex-and-related '(native script entities)
        org-pretty-entities t
        org-hide-emphasis-markers t)

  ;; 自定义美化功能
  (defun +my-org-pretty-mode-setup ()
    "设置自定义的 org 美化模式"
    (setq-local org-pretty-entities t
                org-hide-emphasis-markers t)

    ;; 启用安全的美化功能
    (when (featurep 'org-faces)
      (set-face-attribute 'org-level-1 nil :weight 'bold :height 2)
      (set-face-attribute 'org-level-2 nil :weight 'bold :height 1.1)
      (set-face-attribute 'org-level-3 nil :weight 'bold :height 1.05)
      (set-face-attribute 'org-level-4 nil :weight 'bold)
      (set-face-attribute 'org-level-5 nil :weight 'bold)
      (set-face-attribute 'org-level-6 nil :weight 'bold)
      (set-face-attribute 'org-level-7 nil :weight 'bold)
      (set-face-attribute 'org-level-8 nil :weight 'bold)))

  ;; 添加到 org-mode 钩子
  (add-hook 'org-mode-hook #'+my-org-pretty-mode-setup)

  ;; 优化表格显示
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local org-table-formula-header-flag t
                          org-table-auto-align t
                          org-table-fix-formulas-flag t))))

(after! org-modern
  ;; 彻底关闭表格部分
  (setq org-modern-table nil
        org-modern-table-horizontal nil
        org-modern-table-vertical nil))
