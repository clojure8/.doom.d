;;; my/blamer/config.el -*- lexical-binding: t; -*-

;; blamer：光标停在某行片刻，行尾以淡色显示「作者 · 相对时间 · 提交信息」，
;; 不用专门开 magit-blame。配合已有的 magit / vc-gutter 使用。

(use-package! blamer
  :commands (blamer-mode global-blamer-mode)
  :init
  ;; 默认全局开启：所有 git 跟踪的文件 buffer 都生效
  (add-hook 'doom-after-init-hook #'global-blamer-mode)
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
