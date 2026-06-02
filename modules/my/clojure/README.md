# my/clojure — Clojure REPL/缩进个性化

Doom `:lang clojure` 已提供 CIDER 与 clojure-mode；补全/诊断走 lsp-bridge +
`clojure-lsp`（已安装）。本模块只补几项个性化：

- `clojure-toplevel-inside-comment-form`：rich comment `(comment …)` 内按顶层
  form 求值（Clojure 常用调试习惯）。
- CIDER REPL 体验：去掉帮助横幅、错误 buffer 只在非 REPL 时弹、连接不抢焦点、
  load 前自动存盘、求值结果就地 overlay 显示、动态字体化、REPL 历史持久化。

> 注：未引入 `flycheck-clj-kondo`，因为本机没有独立 `clj-kondo` 二进制；诊断由
> `clojure-lsp`（内置 kondo）经 lsp-bridge 提供。要单独的 kondo 检查可
> `brew install borkdude/brew/clj-kondo` 后再加。

结构化括号编辑见 [my/lisp](../lisp)（evil-cleverparens）。
