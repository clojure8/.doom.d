;; -*- no-byte-compile: t; -*-
;;; my/agentshell/packages.el

(package! shell-maker)
(package! acp
  :recipe (:host github :repo "xenodium/acp.el"))

(package! claude-agent-acp
  :recipe (:host github :repo "cmacrae/agent-shell-sidebar"))

;; 依赖 claude-agent-acp 
;; npm install -g @agentclientprotocol/claude-agent-acp
(package! agent-shell)

(package! agent-recall)
