;;; my/utilities/config.el -*- lexical-binding: t; -*-

;; ── 小工具集成：大文件 / grep 批量编辑 / 跳转历史 / 可视化 undo / 快速切目录 ──
(use-package! vlf
  :commands (vlf vlf-mode)
  :init
  (map! :leader
        :desc "Open large file with VLF" "f V" #'vlf))

(use-package! wgrep
  :commands (wgrep-change-to-wgrep-mode)
  :init
  (map! :after grep
        :map grep-mode-map
        :n "e" #'wgrep-change-to-wgrep-mode))

(use-package! dogears
  :commands (dogears-back dogears-forward dogears-list dogears-go dogears-mode)
  :hook (doom-first-buffer . dogears-mode)
  :init
  (map! :leader
        (:prefix ("j" . "jump")
         :desc "Dogears back"    "b" #'dogears-back
         :desc "Dogears forward" "." #'dogears-forward
         :desc "Dogears list"    "l" #'dogears-list)))

(use-package! vundo
  :commands vundo
  :init
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Visual undo" "u" #'vundo)))

(use-package! consult-dir
  :commands (consult-dir consult-dir-jump-file)
  :init
  (map! :leader
        :desc "Switch directory" "f D" #'consult-dir
        :map minibuffer-local-completion-map
        "C-x C-d" #'consult-dir
        "C-x C-j" #'consult-dir-jump-file))

;; ── rainbow-delimiters：括号按嵌套深度上色（所有 prog-mode；lisp 系尤其受益）──
(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ── aggressive-indent：边写边自动重缩进 ─────────────────────────────────────
;; 只在 lisp 系语言开启——这类语言缩进无歧义、收益最大；python/yaml 等语义缩进
;; 语言不开（aggressive-indent 在那些模式下会捣乱）。
(use-package! aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode
          clojure-mode clojurescript-mode clojurec-mode
          scheme-mode racket-mode hy-mode)
         . aggressive-indent-mode))

;; ── yasnippet-snippets：社区现成 snippet 库（你已有 yasnippet 引擎）──────────
;; 加载后把自带 snippets 目录并入 yas-snippet-dirs；这里跟在 yasnippet 之后加载，
;; 再 yas-reload-all 确保新片段立即可用。
(use-package! yasnippet-snippets
  :after yasnippet
  :config
  (when (fboundp 'yas-reload-all) (yas-reload-all)))

;; ── symbol-overlay：光标符号高亮/跳转/就地改名（不依赖 LSP，evil 友好）────────
;; M-i 高亮/取消当前符号；其余操作走 leader 前缀（避免 evil 下单键 n/p 冲突）。
(use-package! symbol-overlay
  :commands (symbol-overlay-put symbol-overlay-mode
             symbol-overlay-jump-next symbol-overlay-jump-prev
             symbol-overlay-rename symbol-overlay-remove-all)
  :init
  ;; SPC s o 已是 Doom 的 +lookup/online，改挂到空闲的 SPC s h（highlight）。
  (map! "M-i" #'symbol-overlay-put
        :leader
        (:prefix ("s" . "search")
         (:prefix ("h" . "symbol-overlay")
          :desc "Toggle at point" "h" #'symbol-overlay-put
          :desc "Next occurrence" "n" #'symbol-overlay-jump-next
          :desc "Prev occurrence" "p" #'symbol-overlay-jump-prev
          :desc "Rename all"      "r" #'symbol-overlay-rename
          :desc "Remove all"      "R" #'symbol-overlay-remove-all))))

;; ── pcre2el：PCRE ↔ Emacs 正则互转 / 解释正则 ───────────────────────────────
(use-package! pcre2el
  :commands (rxt-explain rxt-convert-syntax pcre-to-elisp
             rxt-pcre-to-elisp rxt-quote-pcre)
  :init
  (map! :leader
        (:prefix ("s" . "search")
         (:prefix ("x" . "regex/pcre2el")
          :desc "Explain regex at point" "e" #'rxt-explain
          :desc "Convert syntax"         "c" #'rxt-convert-syntax
          :desc "PCRE → elisp string"    "p" #'rxt-pcre-to-elisp
          :desc "Quote string as PCRE"   "q" #'rxt-quote-pcre))))

;; ── golden-ratio：自动放大聚焦窗口 —— 仅安装，默认不开，手动 toggle ──────────
;; 按需 `SPC t G' 开/关；不挂任何 hook、不自动启用。
(use-package! golden-ratio
  :commands (golden-ratio golden-ratio-mode)
  :init
  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Golden ratio" "G" #'golden-ratio-mode)))
