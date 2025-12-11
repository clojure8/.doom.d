;;; my/translate/config.el -*- lexical-binding: t; -*-

(use-package! google-translate
  :config
  ;; 设置默认语言
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh-CN")
  
  ;; 设置翻译后端
  (setq google-translate-backend-method 'curl)
  
  ;; 修复 google-translate 的 token 问题
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  
  ;; 翻译函数
  (defun my/google-translate-at-point ()
    "翻译光标处的单词或选中的文本"
    (interactive)
    (google-translate-at-point))
  
  (defun my/google-translate-query-translate ()
    "交互式翻译"
    (interactive)
    (google-translate-query-translate))
  
  (defun my/google-translate-reverse ()
    "反向翻译（中英互译）"
    (interactive)
    (let ((source google-translate-default-source-language)
          (target google-translate-default-target-language))
      (setq google-translate-default-source-language target
            google-translate-default-target-language source)
      (google-translate-at-point)
      (setq google-translate-default-source-language source
            google-translate-default-target-language target))))

;; 快捷键绑定
(map! :leader
      (:prefix ("T" . "translate")
       :desc "Translate at point" "t" #'my/google-translate-at-point
       :desc "Query translate" "q" #'my/google-translate-query-translate
       :desc "Reverse translate" "r" #'my/google-translate-reverse))
