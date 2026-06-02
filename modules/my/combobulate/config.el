;;; my/combobulate/config.el -*- lexical-binding: t; -*-

;; combobulate：基于 tree-sitter 的结构化导航/编辑。你几乎所有 :lang 都开了
;; +tree-sitter，但默认只拿它做高亮；combobulate 把语法树用起来——按节点跳转
;; （兄弟/父子）、拖动节点、按结构选区/删除/克隆、智能 splice 等。
;;
;; 键位前缀默认 `C-c o'（combobulate-key-prefix），进入支持的 ts-mode 后用
;; `C-c o o' 打开 combobulate 菜单查看全部命令。

;; 注意：本版本 combobulate 的入口 `combobulate-mode' 实际是个「忽略实参的开关」，
;; 在 buffer 里首次调用会把自己关掉（要调两次才打开），而且配套的
;; `combobulate-get-registered-language' 有 3 元组/4 元组解构不匹配的 bug。所以这里
;; 不直接 hook `combobulate-mode'，而是按 major-mode 在注册表里找到对应的语言
;; minor-mode 函数直接打开——实测首次调用即生效。
(defun +combobulate-activate-h ()
  "可靠地为当前 buffer 的语言开启 combobulate（绕过 `combobulate-mode' 的开关怪癖）。"
  (when (bound-and-true-p combobulate-registered-languages-alist)
    (when-let ((entry (seq-find (lambda (e) (memq major-mode (nth 1 e)))
                                combobulate-registered-languages-alist)))
      ;; entry = (LANGUAGE MAJOR-MODES MINOR-MODE-FN)
      (funcall (nth 2 entry) 1))))

(use-package! combobulate
  :init
  ;; 自定义前缀（保留默认 C-c o，这里显式声明，方便以后改）
  (setq combobulate-key-prefix "C-c o")
  :hook
  ;; 在你实际启用了 +tree-sitter 的语言 ts-mode 里开启
  ((python-ts-mode     . +combobulate-activate-h)
   (js-ts-mode         . +combobulate-activate-h)
   (typescript-ts-mode . +combobulate-activate-h)
   (tsx-ts-mode        . +combobulate-activate-h)
   (json-ts-mode       . +combobulate-activate-h)
   (css-ts-mode        . +combobulate-activate-h)
   (yaml-ts-mode       . +combobulate-activate-h)
   (html-ts-mode       . +combobulate-activate-h)
   (go-ts-mode         . +combobulate-activate-h)))
