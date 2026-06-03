;; -*- no-byte-compile: t; -*-
;;; my/lsp-bridge/packages.el

(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :branch "master"
           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not compile)))
