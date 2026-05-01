;;; my/org/+pretty.el -*- lexical-binding: t; -*-

;; 使用 valign 对齐表格（org 和 markdown）
(use-package! valign
  :hook ((org-mode . valign-mode)
         (markdown-mode . valign-mode))
  :config
  (setq org-highlight-latex-and-related '(native script entities)
        org-pretty-entities t
        org-hide-emphasis-markers t)

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local org-table-formula-header-flag t
                          org-table-auto-align t
                          org-table-fix-formulas-flag t))))

;; org-modern 美化（替代 org-superstar，避免冲突）
(after! org-modern
  (setq org-modern-table nil
        org-modern-table-horizontal nil
        org-modern-table-vertical nil
        org-modern-star '("◉" "○" "✸" "✿" "◈")
        org-modern-list '((43 . "•") (45 . "–") (42 . "➤"))
        org-modern-priority t
        org-modern-block t
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("DONE" :inverse-video t :inherit org-done))))

;; Typora 风格预览：光标进入时才显示隐藏的标记符号
(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.0))

;; 全局美化
(after! org
  (setq org-ellipsis " ▾"
        org-hide-leading-stars t
        org-image-actual-width '(600)
        org-link-descriptive t
        org-pretty-entities-include-sub-superscripts t)

  (add-hook 'org-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (setq-local truncate-lines t))))
