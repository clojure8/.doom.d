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

  ;; 类 Typora 居中阅读：只对 org buffer 有效，其他 buffer 强制边距为 0
  (defvar my/org-body-width-ratio (/ 8.0 10)
    "Ratio of screen width used for org body text.")

  (defun my/org-calc-margin (win)
    "Return margin char count for WIN, or 0 if non-GUI."
    (if (display-graphic-p)
        (let* ((frame      (window-frame win))
               (char-width (frame-char-width frame))
               (frame-px   (frame-pixel-width frame))
               (body-px    (truncate (* frame-px my/org-body-width-ratio)))
               (margin-px  (/ (- frame-px body-px) 2)))
          (max 0 (/ margin-px char-width)))
      0))

  (defun my/enforce-window-margins (win)
    "Org / Markdown 且窗口占满 frame 宽 → 居中边距；
其余（含左右分屏的窄窗口、其它 buffer）→ 零边距。"
    (when (window-live-p win)
      (if (and (with-current-buffer (window-buffer win)
                 ;; gfm-mode 派生自 markdown-mode，一并覆盖
                 (derived-mode-p 'org-mode 'markdown-mode))
               ;; 左右分屏（side-by-side，如预览开在右侧）时窗口不满宽 → 不居中，
               ;; 省出空间；上下分屏仍满宽，保持居中。
               (window-full-width-p win))
          (set-window-margins win (my/org-calc-margin win) (my/org-calc-margin win))
        (set-window-margins win 0 0))))

  ;; buffer 在窗口中切换时立即更新边距
  (add-hook 'window-buffer-change-functions #'my/enforce-window-margins)

  ;; 窗口大小变化时重新计算（处理 frame resize / 分栏等）
  (add-hook 'window-size-change-functions
            (lambda (frame)
              (dolist (win (window-list frame))
                (my/enforce-window-margins win))))

  ;; 窗口布局变化时也刷新（分屏/弹出预览/关窗等 size-change 不一定触发的场景）：
  ;; 一旦左右分屏（如预览开在右侧），源 md/org 窗口不再满宽 → 立即去居中；
  ;; 关掉分屏恢复满宽 → 立即重新居中。
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (dolist (win (window-list))
                (my/enforce-window-margins win))))

  ;; org 表格行临时取消边距（全宽显示），离开恢复
  (defun my/org-table-adjust-margin ()
    (when (derived-mode-p 'org-mode)
      (let ((win (selected-window)))
        (if (org-at-table-p)
            (set-window-margins win 0 0)
          (my/enforce-window-margins win)))))

  (add-hook 'org-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (setq-local truncate-lines t)
              (my/enforce-window-margins (selected-window))
              (add-hook 'post-command-hook #'my/org-table-adjust-margin nil t))))
