;; -*- no-byte-compile: t; -*-
;;; my/rust/packages.el

(package! rust-mode)
(package! cargo)
(package! toml-mode)
(package! rust-playground)
(package! flycheck-rust)
(package! transient)  ; transient 菜单支持
(package! racer :disable t)  ; 禁用 racer，使用 lsp-bridge
(package! company-racer :disable t)  ; 禁用 company-racer，使用 lsp-bridge 的补全