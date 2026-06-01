# my/markdown — markdown 排版增强

在 Doom 内置 `:lang markdown` 之上补几项 Doom 默认没开、但 prose 写作很实用的设置。
Doom 已有的（native 代码块高亮、GFM checkbox 按钮、`italic-underscore`、整行标题高亮、
表格 valign 对齐）不在此重复。

## 增强项

| 设置 | 作用 |
|------|------|
| `markdown-header-scaling` + 递减字号 | 标题按级别放大，给出 org 那样的视觉层级 |
| `markdown-asymmetric-header` | ATX 标题只在行首放 `#`，不在行尾重复 |
| `markdown-list-indent-width 2` | 列表缩进 2 空格，与 web/prose 一致 |
| `markdown-enable-math` | 行内 `$...$` LaTeX 数学渲染 |
| `markdown-mode-hook` 关行号 | prose 阅读不需要行号 |

软换行由全局的 `+global-word-wrap-mode`（在 `config.el` 启用）统一负责，覆盖
markdown / org / text / rst，保留缩进的语言感知折行。

## 依赖

- Doom `:lang markdown`（提供 `markdown-mode`）
- `my/org` 的 valign（markdown 表格对齐）

## 运行时切换

Doom 的 `SPC m t` 菜单可临时切换 markup 隐藏、URL 隐藏、行内图片、数学、代码高亮等。
