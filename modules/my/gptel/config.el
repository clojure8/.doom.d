;;; my/gptel/config.el -*- lexical-binding: t; -*-

(use-package! gptel

  :config
  ;; DeepSeek 后端配置
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (lambda () (getenv "GPTEL_DEEPSEEK_API_KEY"))
    :models '(deepseek-chat
              deepseek-coder))

  ;; 默认配置
  (setq gptel-default-mode 'org-mode)

  ;; 智谱 AI 后端配置
  (setq gptel-backend (gptel-make-openai "zhipu-ai"
                        :host "open.bigmodel.cn"
                        :endpoint "/api/coding/paas/v4/chat/completions"
                        :stream t
                        :key (lambda () (getenv "GPTEL_ZHIPU_API_KEY"))
                        :models '(glm-4.6 glm-4.5 glm-4.5-air)))
  (setq gptel-model 'glm-4.6))
