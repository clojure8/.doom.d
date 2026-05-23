;;; modules/my/claudecode/config.el -*- lexical-binding: t; -*-

;; 继承 inheritenv (通常在 Doom Emacs 中不是必须的，因为 Doom 会处理环境变量)
(use-package! inheritenv)

;; 配置 Monet 和 Claude-Code
(use-package! monet :config (monet-mode 1))

(use-package! claude-code
  :config
  (setq claude-code-terminal-backend (if (display-graphic-p) 'vterm 'eat))
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (claude-code-mode))

(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools
