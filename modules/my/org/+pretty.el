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
        org-image-actual-width '(600)
        org-link-descriptive t
        org-pretty-entities-include-sub-superscripts t)

  ;; 类 Typora 居中阅读：用 margin 居中，不压缩表格
  (defvar my/org-body-width-ratio (/ 2.0 3)
    "Ratio of screen width used for org body text.")

  (defun my/org-center-buffer ()
    "Set window margins to center buffer content at 2/3 screen width.
Unlike visual-fill-column-mode, this won't compress wide tables."
    (when (display-graphic-p)
      (let* ((char-width (frame-char-width))
             (frame-width (frame-pixel-width))
             (body-pixels (truncate (* frame-width my/org-body-width-ratio)))
             (margin-pixels (/ (- frame-width body-pixels) 2))
             (margin-chars (/ margin-pixels char-width)))
        (set-window-margins (selected-window) margin-chars margin-chars))))

  (defun my/org-center-all-windows (&rest _)
    "Apply centering to all windows displaying an org buffer."
    (dolist (win (window-list))
      (when (with-current-buffer (window-buffer win)
              (derived-mode-p 'org-mode))
        (with-selected-window win (my/org-center-buffer)))))

  (add-hook 'org-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (setq-local truncate-lines t)
              (my/org-center-buffer)
              ;; 窗口大小变化时重新计算 margin
              (add-hook 'window-configuration-change-hook
                        #'my/org-center-all-windows nil t))))

;; Typora 风格预览：光标进入时才显示隐藏的标记符号
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t      ; *bold* /italic/ =code= ~verbatim~
        org-appear-autolinks t          ; [[link][desc]]
        org-appear-autosubmarkers t     ; subscript/superscript markers
        org-appear-delay 0.0))          ; 即时显示，无延迟
