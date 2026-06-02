# my/lisp — evil 下的结构化括号编辑

为 Lisp 家族（Emacs Lisp / Clojure / Scheme / Hy / Racket）在 evil 下提供
[evil-cleverparens](https://github.com/luxbock/evil-cleverparens) 的结构化编辑，
叠在 Doom 既有的 smartparens 之上，编辑括号时始终保持平衡。

## 常用键位（normal state）

| 键 | 作用 |
|----|------|
| `>)` / `<)` | 向右 slurp / barf |
| `>(` / `<(` | 向左 slurp / barf |
| `M-(` / `M-)` | 在前 / 后包一对括号 |
| `D` | 删除整个 form（保持平衡） |
| `M-j` / `M-k` | 当前 form 下移 / 上移 |
| `[` `]` `{` `}` | 按 form / 顶层 form 跳转 |

## 覆盖模式

`emacs-lisp` / `lisp` / `clojure`(`c`/`script`) / `scheme` / `hy` / `racket` /
`cider-repl`。

只在 `:editor evil` 启用时生效。
