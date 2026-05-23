;;; my/org/+pretty.el -*- lexical-binding: t; -*-

;; 使用 valign 对齐表格（org 和 markdown），仅 GUI 下启用（依赖像素对齐）
(use-package! valign
  :hook ((org-mode . (lambda () (when (display-graphic-p) (valign-mode 1))))
         (markdown-mode . (lambda () (when (display-graphic-p) (valign-mode 1)))))
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

  ;; 类 Typora 居中阅读
  (defvar my/org-body-width-ratio (/ 2.0 3)
    "Ratio of screen width used for org body text.")

  (defun my/org-margin-width ()
    "Calculate margin char width for centering at 2/3 screen."
    (when (display-graphic-p)
      (let* ((char-width (frame-char-width))
             (frame-width (frame-pixel-width))
             (body-pixels (truncate (* frame-width my/org-body-width-ratio)))
             (margin-pixels (/ (- frame-width body-pixels) 2)))
        (/ margin-pixels char-width))))

  (defun my/org-center-buffer ()
    "Set window margins to center buffer content."
    (when (display-graphic-p)
      (let ((margin (my/org-margin-width)))
        (set-window-margins (selected-window) margin margin))))

  (defun my/org-restore-centering ()
    "Restore centered margins for all org windows."
    (dolist (win (get-buffer-window-list nil nil t))
      (with-selected-window win (my/org-center-buffer))))

  ;; 光标在表格上时自动取消居中（全宽显示表格），离开时恢复居中
  (defun my/org-table-adjust-margin ()
    "Remove margins when on a table line, restore otherwise."
    (when (and (derived-mode-p 'org-mode) (display-graphic-p))
      (if (org-at-table-p)
          (set-window-margins (selected-window) 0 0)
        (my/org-center-buffer))))

  (add-hook 'org-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (setq-local truncate-lines t)
              (my/org-center-buffer)
              (add-hook 'post-command-hook #'my/org-table-adjust-margin nil t)
              (add-hook 'window-configuration-change-hook
                        #'my/org-restore-centering nil t))))
