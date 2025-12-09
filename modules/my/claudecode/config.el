;;; modules/my/ai/config.el -*- lexical-binding: t; -*-

;; 继承 inheritenv (通常在 Doom Emacs 中不是必须的，因为 Doom 会处理环境变量)
(use-package! inheritenv)

;; 配置 Monet 和 Claude-Code
(use-package! monet
  :config
  (monet-mode 1))

(use-package! claude-code
  :defer t

  :config
  (setq claude-code-terminal-backend 'vterm)

  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (claude-code-mode))
