;; -*- no-byte-compile: t; -*-
;;; my/lsp-bridge/packages.el

(when (package! lsp-bridge
        :recipe (:host github
                 :repo "manateelazycat/lsp-bridge"
                 :branch "master"
                 :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 :build (:not compile)))
  (package! markdown-mode)
  (package! yasnippet))

;; TUI 下 acm child-frame 不可用，用 popon + acm-terminal 提供 terminal popup 渲染
;; acm-terminal 通过 advice 替换 acm-frame 函数，不修改 lsp-bridge 源码
(package! popon
  :recipe (:type git :host nil :repo "https://codeberg.org/akib/emacs-popon.git"))
(package! acm-terminal
  :recipe (:host github :repo "twlz0ne/acm-terminal"))
