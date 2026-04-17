;;; my/chatgpt-shell/config.el -*- lexical-binding: t; -*-


(after! chatgpt-shell

  ;; 可选：设为默认模型
  ;; (setq chatgpt-shell-model-version "glm-4")
  (setq chatgpt-shell-zhipu-key (getenv "GPTEL_ZHIPU_API_KEY")))
