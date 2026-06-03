;;; my/minuet/config.el -*- lexical-binding: t; -*-

(use-package! minuet
  :commands (minuet-complete-with-minibuffer minuet-auto-suggestion-mode)
  :init
  ;; 先提供手动触发入口，避免自动 ghost text 与 lsp-bridge/acm 补全 UI 打架。
  (map! :leader
        (:prefix ("o" . "open")
         (:prefix ("l" . "llm")
          :desc "Minuet inline completion" "i" #'minuet-complete-with-minibuffer)))
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
  ;; I recommend beginning with a small context window size and incrementally
  ;; expanding it, depending on your local computing power. A context window
  ;; of 512, serves as an good starting point to estimate your computing
  ;; power. Once you have a reliable estimate of your local computing power,
  ;; you should adjust the context window to a larger value.
  (setq minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder.
  ;; For Windows users, TERM may not be present in environment variables.
  ;; Consider using APPDATA instead.
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 512))
