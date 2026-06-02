# my/magit — magit diff 用 delta 渲染

[magit-delta](https://github.com/dandavison/magit-delta) 让 magit 的 diff 经
[`delta`](https://github.com/dandavison/delta) 渲染：语法高亮、更清晰的增删行。

只在 `delta` 二进制存在时启用（`brew install git-delta`）。在 `magit-mode` 自动开启。

参数 `--color-only`（不改变 diff 结构，只上色），明暗主题分别用 Nord / GitHub。
若想改成并排 diff，把 `magit-delta-delta-args` 里加 `--side-by-side`。
