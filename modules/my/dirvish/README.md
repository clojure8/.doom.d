# my/dirvish — dired 现代化

[dirvish](https://github.com/alexluigit/dirvish) 接管所有 dired 入口，提供文件
预览、nerd 图标、头部路径导航、行内属性列（大小/时间/git 状态）、子目录树等。

## 常用键位（`dirvish-mode`）

| 键 | 作用 |
|----|------|
| `a` | 快速访问书签（Home/Downloads/Org/Doom 配置） |
| `TAB` | 展开/折叠子目录树 |
| `M-t` | 全屏 / 普通布局切换 |
| `f` | 文件信息菜单 |
| `y` | yank（复制/移动）菜单 |
| `s` | 快速排序 |
| `h` / `l` | 上级目录 / 进入 |
| `q` | 退出 |

## 依赖

- nerd-icons（Doom 已带）
- 可选 `gls`（`brew install coreutils`）：让「目录优先」排序在 macOS 生效；
  没有则自动回退到系统 ls。
