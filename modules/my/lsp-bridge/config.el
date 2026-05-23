;;; my/lsp-bridge/config.el -*- lexical-binding: t; -*-

(use-package! lsp-bridge
  :config
  (global-lsp-bridge-mode))

;; TUI 补全前端：acm-terminal 用 popon 替换 acm 的 child-frame 渲染
;; 在 daemon 模式下，acm-terminal 在每次渲染时检查当前 frame 的 display-graphic-p，
;; GUI frame 继续使用 child-frame，TUI frame 自动切换到 popon，无需手动切换
(use-package! acm-terminal
  :after acm)
