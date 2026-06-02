;;; my/lisp/config.el -*- lexical-binding: t; -*-

;; 你是 Clojure / Lisp 用户 + evil，但之前只有 smartparens。evil-cleverparens
;; 让结构化括号编辑在 evil 下顺手：slurp/barf、wrap、move-form、splice 等都有
;; evil 风格的键位，且不破坏括号平衡。
;;
;; 常用（normal state，cleverparens 默认键位）：
;;   >)  / <)   向右 slurp / barf（把右边的表达式吞进/吐出当前 form）
;;   >(  / <(   向左 slurp / barf
;;   M-( / M-)  在前/后包一对括号
;;   D          删除整个 form（保持平衡）
;;   M-j / M-k  把当前 form 下移 / 上移

(use-package! evil-cleverparens
  :when (modulep! :editor evil)
  :init
  ;; 保留 cleverparens 的额外移动键（[ ] { } 等按 form 跳转），但不抢占
  ;; 你可能习惯的部分键。先用默认，按需再调。
  (setq evil-cleverparens-use-additional-movement-keys t
        evil-cleverparens-use-additional-bindings t
        ;; s/S 留给 evil-snipe/常规用途，不被 cleverparens 接管
        evil-cleverparens-swap-move-by-word-and-symbol t)
  :hook
  ((emacs-lisp-mode . evil-cleverparens-mode)
   (lisp-mode       . evil-cleverparens-mode)
   (clojure-mode    . evil-cleverparens-mode)
   (clojurec-mode   . evil-cleverparens-mode)
   (clojurescript-mode . evil-cleverparens-mode)
   (scheme-mode     . evil-cleverparens-mode)
   (hy-mode         . evil-cleverparens-mode)
   (racket-mode     . evil-cleverparens-mode)
   (cider-repl-mode . evil-cleverparens-mode)))
