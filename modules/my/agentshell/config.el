;;; my/agentshell/config.el -*- lexical-binding: t; -*-

(use-package! acp)

(use-package! agent-shell)

(use-package! agent-recall
  :config
  (setq agent-recall-search-paths '("~/projects"))
  (global-agent-recall-transcript-mode))
