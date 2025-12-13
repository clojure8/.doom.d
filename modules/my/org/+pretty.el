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
