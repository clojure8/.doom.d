;;; my/clojure/config.el -*- lexical-binding: t; -*-

;; Doom `:lang clojure' 已提供 CIDER / clojure-mode；补全与诊断走 lsp-bridge +
;; clojure-lsp（已安装）。这里只做几项 REPL 体验与缩进的个性化。
;; 结构化括号编辑由 my/lisp（evil-cleverparens）负责。

(after! clojure-mode
  ;; rich comment 块 (comment ...) 内按顶层 form 求值，符合 Clojure 习惯
  (setq clojure-toplevel-inside-comment-form t))

(after! cider
  (setq
   ;; REPL 干净点：不显示帮助横幅
   cider-repl-display-help-banner nil
   ;; 错误 buffer 只在非 REPL 来源时弹，REPL 里直接看输出
   cider-show-error-buffer 'except-in-repl
   ;; 连接后只显示 REPL、不抢焦点
   cider-repl-pop-to-buffer-on-connect 'display-only
   ;; load 前自动存盘，免去手动保存
   cider-save-file-on-load t
   ;; 求值结果以 overlay 形式就地显示（而非只在 minibuffer）
   cider-use-overlays t
   cider-result-overlay-position 'at-eol
   ;; 动态字体化：宏 / 核心函数 / var 着色更清晰
   cider-font-lock-dynamically '(macro core function var)
   ;; REPL 历史持久化
   cider-repl-history-file (expand-file-name "cider-history" doom-cache-dir)
   cider-repl-history-size 3000))
