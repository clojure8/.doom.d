;;; my/blamer/config.el -*- lexical-binding: t; -*-

;; blamer：光标停在某行片刻，行尾以淡色显示「作者 · 相对时间 · 提交信息」，
;; 不用专门开 magit-blame。配合已有的 magit / vc-gutter 使用。

(use-package! blamer
  :commands (blamer-mode global-blamer-mode)
  ;; 默认不再全局开启光标行 blame 提示（太干扰）。需要时 `M-x blamer-mode'
  ;; 临时在当前 buffer 打开，或 `M-x global-blamer-mode' 全局打开。
  :config
  (setq blamer-idle-time 0.5
        blamer-min-offset 6
        ;; 只显示作者名 + 相对时间 + 提交信息，截断过长 commit message
        blamer-author-formatter " ✎ %s "
        blamer-datetime-formatter "[%s]"
        blamer-commit-formatter " ● %s"
        blamer-max-commit-message-length 60)
  (custom-set-faces!
    '(blamer-face :foreground "#7a88cf" :italic t)))
