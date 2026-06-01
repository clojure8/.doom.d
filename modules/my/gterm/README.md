# my/gterm — Ghostty 引擎终端（emacs-libgterm）

[`rwc9u/emacs-libgterm`](https://github.com/rwc9u/emacs-libgterm) —— 架构类似 vterm，
但终端引擎用 [Ghostty](https://ghostty.org/) 的 `libghostty-vt`，通过 Zig 编译动态模块。

> 上游自述为「early prototype, fully vibe coded」，仅在 macOS Apple Silicon 测过。

## 使用

`M-x gterm` 开一个终端。常用键：

| 键 | 作用 |
|----|------|
| `C-c C-k` | 进入 copy mode（选区，`y` 复制，`q` 退出） |
| `C-c C-v` | 跳回实时终端 |
| `C-c C-c` / `C-c C-d` / `C-c C-z` | 发送 Ctrl-C / Ctrl-D / Ctrl-Z |
| `Shift-PageUp/Down`、滚轮 | 翻 scrollback |
| 从 Finder 拖文件 | 把路径送进终端 |

## 首次加载会自动编译

`gterm.el` 在**加载时**就会检测并编译动态模块，因此本模块用 `:commands` 延迟加载：
只有第一次 `M-x gterm` 才加载 → 触发编译，不拖慢启动。编译流程：

1. 把 Ghostty clone 到 `<straight-build>/gterm/vendor/ghostty`
2. `zig build` 编出 `zig-out/lib/libgterm-module.dylib`
3. `module-load` 加载

`gterm-always-compile-module` 已设为 `t`，首次编译不再询问。
（本机已预先 clone 好 ghostty，首跑省去约 130MB 下载。）

## 前置依赖

- Emacs 带 module 支持（emacs-plus@30 ✓，`module-file-suffix` = `.dylib`）
- `git`
- `zig` 0.15.2+（gterm 的版本校验接受 0.15–0.99）
- 编译期需要 `emacs-module.h`（emacs-plus 的 include 目录，gterm 自动探测）

### Zig 版本注意

仓库 `.tool-versions` 钉的是 **zig 0.15.2**，本机是 **0.16.0**。Ghostty 的
`minimum_zig_version` 为 `0.15.2`，版本校验通过；若首次 `zig build` 因 0.16 的
破坏性改动失败，用 mise/asdf 装 0.15.2 再编：

```bash
mise use -g zig@0.15.2      # 或 asdf install zig 0.15.2
```

## 已编译好（开箱即用）

动态模块已编译并放在 gterm 期望的路径：

```
<straight-build>/gterm/zig-out/lib/libgterm-module.dylib   # arm64 Emacs module，已验证 module-load 通过
```

所以重启 Emacs 后 `M-x gterm` 会**直接 module-load 现成模块**，不再编译、不碰网络。

### 关于代理（Surge）的坑

本机跑着 Surge（`127.0.0.1:6152`）。**zig 的 HTTP 下载器无法穿过这个代理**
（同一依赖 URL：`curl` 能 200，zig 经代理被 400/503），而很多 zig 依赖又托管在
github（本机直连不通、要走代理）—— 两边夹击导致 `zig build` 拉依赖必失败。

解决办法（已替你做好，模块已编译，无需再操作）：

1. 用 `curl --proxy 127.0.0.1:6152` 把依赖 tar 包下到本地，再 `zig fetch <本地包>`
   塞进 zig 全局缓存（`~/.cache/zig/p/`，hash 与 build.zig.zon 对得上）；
   github 直连的包则**临时去掉代理环境变量**让 zig 直取。
2. 依赖凑齐后 `zig build` 全程走缓存、零联网，编译成功。

> 若日后 `doom sync` 重建了 gterm 包、清掉了 `zig-out/`，触发重编时：zig 依赖
> 缓存（`~/.cache/zig/p/`）已齐全，`zig build` 可离线完成；只有重新 clone ghostty
> 需要网络（git 能走代理）。万一仍卡在 zig 拉依赖，按上面第 1 步补缓存即可。

## 验证状态

- ✅ `doom sync` 安装成功；`:files ("*")` 已把 `build.zig` / `src/` 拷进 build 目录。
- ✅ 加载已延迟：启动时 `gterm` 仅为 autoload，未加载、未触发编译。
- ✅ 前置依赖齐全（git、emacs-module.h、arm64 macOS）。
- ✅ **动态模块已编译成功**（zig 0.15.2，绕开代理拉齐依赖后离线编译）。
- ✅ `module-load` 实测通过：dylib 是合法的 arm64 Emacs 动态模块，提供
  `gterm-module` feature。
