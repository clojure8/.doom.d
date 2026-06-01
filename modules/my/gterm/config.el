;;; my/gterm/config.el -*- lexical-binding: t; -*-

;; emacs-libgterm（gterm）：Ghostty 引擎的终端模拟器。
;;
;; 重要：gterm.el 在「加载时」就会检测并触发 Zig 编译（首次还会 clone Ghostty），
;; 所以这里用 `:commands' 延迟加载——只有真正 `M-x gterm' 时才加载、才编译，
;; 不拖慢 Emacs 启动。`gterm-always-compile-module' 设为 t，首次编译不再询问。
;;
;; 前置依赖（需在 PATH）：zig 0.15.2+（本机 0.16，版本校验通过）、git；
;; 以及编译期需要的 emacs-module.h（emacs-plus 的 include 目录，gterm 会自动探测）。
(use-package! gterm
  :commands (gterm gterm-module-compile)
  :init
  (setq gterm-always-compile-module t))
