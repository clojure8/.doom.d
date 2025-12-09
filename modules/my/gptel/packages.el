;;; my/gptel/packages.el -*- lexical-binding: t; -*-
(package! gptel :recipe (:nonrecursive t))

(package! sort-tab
  :recipe (:host github
           :repo "manateelazycat/sort-tab"
           :branch "main"
           :files ("*.el")
           ;; do not perform byte compilation or native compilation for lsp-bridge
           :build (:not compile)))
