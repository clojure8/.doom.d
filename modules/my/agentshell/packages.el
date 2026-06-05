;; -*- no-byte-compile: t; -*-
;;; my/agentshell/packages.el

(package! shell-maker)
(package! acp
  :recipe (:host github :repo "xenodium/acp.el"))

;; agent-shell 运行时依赖外部可执行文件 claude-agent-acp（npm CLI，非 Emacs 包）：
;;   npm install -g @agentclientprotocol/claude-agent-acp
;; 安装后用 `which claude-agent-acp` 确认它在 PATH 中。
(package! agent-shell)

;; 侧边栏 UI（之前误以 claude-agent-acp 之名声明，实际是 agent-shell-sidebar）
(package! agent-shell-sidebar
  :recipe (:host github :repo "cmacrae/agent-shell-sidebar"))

(package! agent-recall)
