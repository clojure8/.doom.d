;;; my/utilities/config.el -*- lexical-binding: t; -*-

;; ── 小工具集成：大文件 / grep 批量编辑 / 跳转历史 / 可视化 undo / 快速切目录 ──
(use-package! vlf
  :commands (vlf vlf-mode)
  :init
  (map! :leader
        :desc "Open large file with VLF" "f V" #'vlf))

(use-package! wgrep
  :commands (wgrep-change-to-wgrep-mode)
  :init
  (map! :after grep
        :map grep-mode-map
        :n "e" #'wgrep-change-to-wgrep-mode))

(use-package! dogears
  :commands (dogears-back dogears-forward dogears-list dogears-go dogears-mode)
  :hook (doom-first-buffer . dogears-mode)
  :init
  (map! :leader
        (:prefix ("j" . "jump")
         :desc "Dogears back"    "b" #'dogears-back
         :desc "Dogears forward" "." #'dogears-forward
         :desc "Dogears list"    "l" #'dogears-list)))

(use-package! vundo
  :commands vundo
  :init
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Visual undo" "u" #'vundo)))

(use-package! consult-dir
  :commands (consult-dir consult-dir-jump-file)
  :init
  (map! :leader
        :desc "Switch directory" "f D" #'consult-dir
        :map minibuffer-local-completion-map
        "C-x C-d" #'consult-dir
        "C-x C-j" #'consult-dir-jump-file))
