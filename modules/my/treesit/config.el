;;; my/treesit/config.el -*- lexical-binding: t; -*-

;; treesit-auto: 自动切换到 tree-sitter 版本的 major-mode
;;
;; ⚠️ 性能坑（已修）：treesit-auto 给 `set-auto-mode-0' 挂了 :before advice，
;; 每次打开文件都会遍历所有 recipe 调 `treesit-ready-p' 重建 remap 表。grammar
;; 已装的语言探测很快，但**未装** grammar 的语言在 macOS 上每次 dlopen 探测约
;; 29ms 且不缓存；对 markdown 这类未装 grammar 的类型还会触发 `revert-buffer'
;; 递归，把整套扫描重复 ~3 遍 → 单次开文件阻塞 5~10s（本机未装的 grammar 有 50 个）。
;;
;; 修复：
;;   1. `treesit-auto-install nil' —— 本机网络/代理下 grammar 根本下载不下来，
;;      关掉无谓的安装尝试（缺失就静默回退普通 major-mode）。
;;   2. 启动时把 `treesit-auto-langs' 过滤成「grammar 已安装」的子集 —— 未装的
;;      语言反正用不了 ts-mode，移出 recipe 后开文件不再对它们反复探测。
;; 效果：打开 markdown 等文件从 ~9.8s 降到 ~0.27s。
(use-package! treesit-auto
  :config
  (setq treesit-auto-install nil)
  ;; markdown / markdown-inline：本机未装 grammar，且我们已用 major-mode-remap-alist
  ;; 强制 markdown → gfm-mode（markdown-ts-mode 无标题分级等特性，并不想要），显式
  ;; 排除，避免首次开 .md 时还去探测一次缺失的 markdown-inline grammar。
  (setq treesit-auto-langs
        (seq-filter (lambda (lang)
                      (and (not (memq lang '(markdown markdown-inline)))
                           (treesit-ready-p lang t)))
                    treesit-auto-langs))
  (global-treesit-auto-mode))
