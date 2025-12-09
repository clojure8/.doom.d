;; -*- no-byte-compile: t; -*-
;;; my/jinx/config.el


;; 启用 jinx 全局拼写检查
(use-package! jinx
  :hook (after-init . global-jinx-mode)
  :config
  (setq jinx-languages "en_US"))

