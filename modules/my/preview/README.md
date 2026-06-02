# my/preview — org / markdown 实时预览

为 org 和 markdown 提供实时 HTML 预览，两种显示方式：

- **xwidget**：内嵌 WebKit，右侧分屏显示，**保存后自动重载**（需 GUI + 编译了
  `--with-xwidgets` 的 Emacs，本机已满足）。
- **默认浏览器**：用 `file://` 在系统浏览器打开，保存后重新打开。

导出不依赖 grip：
- **org** → 内置 `ox-html`（`org-export-to-file 'html`）
- **markdown** → 直接调 `pandoc -f gfm -t html5 -s`（GFM 表格、代码高亮）

生成的 HTML 写在源文件**同目录**的隐藏文件 `.<名字>.preview.html`，所以文中
相对路径的图片/链接照常解析；kill buffer 时自动删除。

## 命令 / 键位（org 与 markdown 的 localleader `SPC m v`）

| 键 | 命令 | 作用 |
|----|------|------|
| `SPC m v v` | `+preview/open` | 自动选 xwidget（有 GUI）否则浏览器 |
| `SPC m v x` | `+preview/xwidget` | 强制 xwidget 内嵌预览 |
| `SPC m v b` | `+preview/browser` | 强制默认浏览器 |
| `SPC m v q` | `+preview/stop` | 停止保存自动刷新 |

也可直接 `M-x +preview/...`。

## 依赖

- `pandoc`（markdown 预览，已安装）
- 编译了 `--with-xwidgets` 的 Emacs（xwidget 预览，已满足）

## 自定义

- `+preview-pandoc-args`：markdown 预览的 pandoc 参数（默认
  `-f gfm -t html5 -s --highlight-style=tango`）。
