;; -*- no-byte-compile: t; -*-
;;; my/gterm/packages.el

;; emacs-libgterm：基于 Ghostty(libghostty-vt) 终端引擎的 Emacs 终端模拟器，
;; 架构类似 vterm，但用 Zig 编译动态模块。
;;
;; `:files ("*")' 必须保留：straight 默认只拷 *.el，而 gterm 首次加载时需要
;; build.zig / src/*.zig 等源码就地用 zig 编译出 libgterm-module.dylib。
(package! gterm
  :recipe (:host github
           :repo "rwc9u/emacs-libgterm"
           :branch "main"
           :files ("*")
           :build (:not compile)))
