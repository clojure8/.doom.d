;; -*- no-byte-compile: t; -*-
;;; my/agentshell/packages.el

(package! shell-maker)
(package! acp
  :recipe (:host github :repo "xenodium/acp.el"))
(package! agent-shell)
