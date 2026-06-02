;;; my/magit/config.el -*- lexical-binding: t; -*-

;; magit-delta：用 delta 渲染 magit 的 diff，得到语法高亮、更易读的 diff。
;; 仅在 `delta' 二进制存在时启用（brew install git-delta）。

(use-package! magit-delta
  :when (executable-find "delta")
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; 跟随 Doom 主题做高亮；窗口窄时用 --side-by-side 会很挤，这里关掉
  (setq magit-delta-default-dark-theme "Nord"
        magit-delta-default-light-theme "GitHub"
        magit-delta-delta-args
        '("--max-line-distance" "0.6" "--true-color" "always" "--color-only")))
