;; -*- no-byte-compile: t; -*-
;;; my/lsp-bridge/packages.el

(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :branch "master"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not compile)))

;; acm-terminal：TTY 下用 popon 渲染 acm 补全菜单（GUI 仍用原生 child-frame）。
;; popon 是其依赖（在 codeberg，非 MELPA），acm-terminal 在 github、非 MELPA，均需 recipe。
(package! popon :recipe (:host codeberg :repo "akib/emacs-popon"))
(package! acm-terminal :recipe (:host github :repo "twlz0ne/acm-terminal"))
