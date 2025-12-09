;; -*- no-byte-compile: t; -*-
;;; my/chatgpt-shell/packages.el

(package! shell-maker)

(package! chatgpt-shell
  :recipe (:host github :repo "clojure8/chatgpt-shell"))
