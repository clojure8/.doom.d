;;; my/markdown/config.el -*- lexical-binding: t; -*-

;; 在 Doom `:lang markdown' 之上做视觉层级与排版增强。
;; Doom 已默认开启：native 代码块高亮、GFM checkbox 按钮、italic-underscore、
;; 整行标题高亮等，这里不重复，只补 Doom 没设的几项。
;; 表格对齐由 my/org 的 valign（markdown-mode hook）负责。

;; ── 编辑用 gfm-mode，而非极简的 markdown-ts-mode ──────────────────────────
;; Doom `:lang markdown +tree-sitter' 会把 .md 重映射到 Emacs 内置的
;; markdown-ts-mode，但后者极简：标题只是 keyword face，无标题层级、无强调/数学
;; 字体化、无插入命令，对写作是退化。这里改回功能完整的 markdown-mode 家族，并
;; 默认 gfm-mode（GitHub 风格：复选框、表格、围栏代码）。markdown grammar 仍由
;; Doom 安装、保留备用。
;;
;; 注意：major-mode-remap-alist（用户级，优先级高于 Doom 写入 +tree-sitter 重映射
;; 的 major-mode-remap-defaults）必须在任何 .md 打开前就位，故放在顶层而非 after!。
(add-to-list 'major-mode-remap-alist '(markdown-mode . gfm-mode))

;; markdown-enable-math 是「自动 buffer-local」变量，setq 只会改当前 buffer，
;; 必须用 setq-default 改全局默认值，新 buffer 才会启用行内 LaTeX 数学。
(setq-default markdown-enable-math t)

(after! markdown-mode
  (setq
   ;; 标题按级别放大，给出 org 那样的视觉层级（Doom 默认不开）
   markdown-header-scaling t
   markdown-header-scaling-values '(1.6 1.4 1.2 1.1 1.0 1.0)
   ;; ATX 标题只在行首放 #，不在行尾重复
   markdown-asymmetric-header t
   ;; 列表缩进 2 空格，与 web/prose 缩进一致
   markdown-list-indent-width 2)

  ;; header-scaling 平时通过 defcustom 的 :set 钩子生效，setq 不触发它，
  ;; 这里手动刷新一次，让标题字号立即应用到 markdown-header-face-N。
  (markdown-update-header-faces markdown-header-scaling markdown-header-scaling-values)

  ;; prose 不需要行号（org 已在 +pretty 里关过，这里给 markdown 同样处理）
  (add-hook 'markdown-mode-hook
            (lambda () (display-line-numbers-mode 0))))
