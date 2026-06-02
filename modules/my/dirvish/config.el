;;; my/dirvish/config.el -*- lexical-binding: t; -*-

;; dirvish：dired 的现代化替代，接管所有 dired 入口。提供文件预览、图标、
;; 头部路径导航、属性列（大小/时间/git 状态）、子目录树等。

(use-package! dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-quick-access-entries
        '(("h" "~/"             "Home")
          ("d" "~/Downloads/"   "Downloads")
          ("o" "~/org/"         "Org")
          ("e" "~/.doom.d/"     "Doom config")))

  ;; 头部显示项 + 行内属性列
  (setq dirvish-header-line-format '(:left (path) :right (free-space))
        dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state)
        ;; mode-line 显示排序/过滤/标记信息
        dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))

  ;; 用 GNU ls 的分组目录优先（macOS 默认 BSD ls 不支持，gls 由 coreutils 提供）
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  ;; evil 友好的常用键位（dirvish-mode-map）
  (map! :map dirvish-mode-map
        :n "q"   #'dirvish-quit
        :n "a"   #'dirvish-quick-access
        :n "f"   #'dirvish-file-info-menu
        :n "y"   #'dirvish-yank-menu
        :n "s"   #'dirvish-quicksort
        :n "TAB" #'dirvish-subtree-toggle
        :n "M-t" #'dirvish-layout-toggle      ; 全屏/普通布局切换
        :n "h"   #'dired-up-directory
        :n "l"   #'dired-find-file))
