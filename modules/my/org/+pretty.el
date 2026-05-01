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

;; 全局美化
(after! org
  (setq org-ellipsis " ▾"
        org-hide-leading-stars t
        org-image-actual-width '(600))

  ;; 类 Typora 居中阅读：文本居中，宽度约为屏幕 2/3
  (defvar my/org-fill-column
    (/ (* (display-pixel-width) 2) (* 3 (frame-char-width)))
    "Org mode fill-column, approximately 2/3 of screen width.")

  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local fill-column my/org-fill-column)
              (display-line-numbers-mode 0)
              (visual-line-mode 1)
              (visual-fill-column-mode 1)
              (setq visual-fill-column-center-text t
                    visual-fill-column-fringes-outside-margins t
                    left-fringe-width 0
                    right-fringe-width 0))))

;; 修复 org-table 在 visual-line-mode 下的换行变形
(after! org
  (advice-add 'org-table-align :after
              (lambda (&rest _)
                (when (and (bound-and-true-p visual-line-mode)
                           (bound-and-true-p valign-mode))
                  (valign-mode -1)
                  (valign-mode 1)))))
