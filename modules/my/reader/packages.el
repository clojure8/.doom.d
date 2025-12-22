;; -*- no-byte-compile: t; -*-
;;; my/reader/packages.el

(package! reader
  :recipe ( :type git :host codeberg :repo "divyaranjan/emacs-reader"
     		     :files ("*.el" "render-core.dylib")
     		     :pre-build ("make" "all")))
