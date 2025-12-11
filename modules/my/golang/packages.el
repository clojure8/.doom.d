;; -*- no-byte-compile: t; -*-
;;; my/golang/packages.el

(package! go-mode)
(package! go-tag)
(package! go-gen-test)
(package! go-impl)
(package! go-fill-struct)
(package! gorepl-mode)
(package! go-playground)
(package! go-projectile)
(package! go-eldoc)
(package! go-guru)
(package! go-rename)
(package! transient)  ; transient 菜单支持
(package! dap-mode)   ; 调试支持
(package! company-go :disable t)  ; 禁用 company-go，使用 lsp-bridge 的补全