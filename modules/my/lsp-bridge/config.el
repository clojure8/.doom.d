;;; my/lsp-bridge/config.el -*- lexical-binding: t; -*-

(use-package! lsp-bridge
  :init
  ;; 使用 pyenv 的绝对路径，避免 Emacs GUI 启动时找不到正确的 python3
  (setq lsp-bridge-python-command "/Users/mac/.pyenv/versions/3.12.2/bin/python3")
  :config
  ;; TUI 下 acm 依赖 child-frame，终端不支持，只在 GUI 启用
  (when (display-graphic-p)
    (global-lsp-bridge-mode)))

;; daemon 模式：Emacs 以服务启动时 display-graphic-p 为 nil，
;; 等 GUI frame 创建后再启用
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (and (display-graphic-p)
                       (not (bound-and-true-p global-lsp-bridge-mode)))
              (global-lsp-bridge-mode))))
