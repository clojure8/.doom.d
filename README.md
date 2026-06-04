# Doom Emacs Configuration

个人化的 Doom Emacs 配置，集成 AI 工具、数据科学与多语言开发环境，针对 macOS + 中文用户优化。

## 特色功能

- **AI 集成套件**: Agent Shell、Gptel（智谱 GLM / DeepSeek）、Minuet 本地 LLM 行内补全、aigen 就地代码生成
- **补全引擎**: lsp-bridge（Python 实现的高性能 LSP 前端，全局启用，非 eglot）
- **多语言开发**: Go、Rust、Python、JS/TS、Vue/React、Clojure、Swift、Zig 等（lsp-bridge + tree-sitter）
- **结构化编辑**: combobulate（tree-sitter 语法树导航/编辑）、evil-cleverparens（Lisp 括号操作）
- **写作 & 文档**: org（super-agenda / roam / 美化 / Jupyter）、markdown（gfm + 实时预览）、xwidget/浏览器预览
- **数据科学**: emacs-jupyter，org-babel 内联执行 Python（含 matplotlib 内联绘图）
- **现代界面**: JetBrainsMono Nerd Font、doom-one 主题、Vertico、Treemacs、dirvish、瘦身 modeline
- **系统优化**: macOS 支持、中文输入、jinx 拼写检查（跳过 CJK）、Google 翻译、按 web/非 web 区分缩进

## 快速开始

```bash
# 首次安装 Doom Emacs
git clone https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# 克隆本配置
git clone <this-repo> ~/.doom.d

doom sync   # 同步配置，重启 Emacs
```

## 项目结构

```
~/.doom.d/
├── init.el                  # Doom 模块开关
├── config.el                # 核心用户配置（字体/主题/缩进/modeline/UI 小件）
├── packages.el              # 外部包声明
└── modules/my/              # 自定义模块
    ├── agentshell/          # Agent Protocol + Claude Agent Shell
    ├── aigen/               # gptel 就地 AI 生成（注释→代码 / 总结 / 扩展）
    ├── blamer/              # 行内 git blame（默认关，按需开）
    ├── clojure/             # CIDER REPL 体验 + rich-comment 求值
    ├── combobulate/         # tree-sitter 结构化导航/编辑
    ├── dirvish/             # dired 现代化（预览/图标/属性列）
    ├── editing/             # string-inflection 命名风格切换
    ├── golang/              # Go 完整工具链
    ├── gptel/              # AI 聊天（智谱 GLM / DeepSeek）
    ├── jinx/                # 拼写检查（enchant，跳过 CJK）
    ├── layout/              # 窗口布局保存/恢复
    ├── lisp/                # evil-cleverparens 结构化括号编辑
    ├── lsp-bridge/          # 高性能 LSP 补全（python 后端已 pin）
    ├── magit/               # magit-delta（语法高亮 diff）
    ├── markdown/            # gfm-mode + 标题分级 + 数学
    ├── minuet/              # 本地 LLM 行内补全（Ollama）
    ├── org/                 # Org 增强 + super-agenda + roam + Jupyter
    ├── preview/             # org/markdown 实时预览（xwidget / 浏览器）
    ├── reader/              # 电子书阅读器（emacs-reader）
    ├── translate/           # Google 翻译
    └── web/                 # 前端（HTML/CSS/JS/Vue/React）lsp-bridge 接线
```

> 另有 `claudecode/`、`awesome-tray/` 模块存在但默认未在 `init.el` 启用。

## 核心模块

### AI

| 模块 | 功能 |
|------|------|
| **agentshell** | Agent Protocol + Claude Agent Shell |
| **gptel** | 多后端 AI 聊天：智谱 GLM-5.1 / DeepSeek（key 走环境变量） |
| **aigen** | 就地生成：`SPC o l g` 下 注释→代码 / 总结选区 / 扩展描述 / 自定义指令 |
| **minuet** | Ollama 本地 LLM 行内补全（qwen2.5-coder:3b） |

### 开发语言（init.el `:lang`，均 LSP + tree-sitter）

Go、Python（uv/pyenv/pyright）、JS/TS、Web（Vue/React/HTML/CSS）、Rust、Clojure、
Java、Swift、Lua、Nim、Zig、GraphQL、YAML、JSON、Markdown、LaTeX、Shell、Org、PureScript

### 编辑 / 工具

| 模块 | 功能 |
|------|------|
| **lsp-bridge** | 全局 LSP 补全；python 后端 pin 到带 epc/orjson 的解释器 |
| **combobulate** | `C-c o o` 打开菜单，按语法节点导航/拖动/选区（9 个 ts-mode） |
| **lisp** | evil 下 slurp/barf/wrap/move（elisp/clojure/scheme/hy/racket） |
| **editing** | `SPC c ~` 循环命名风格、`SPC c _` 下划线↔驼峰 |
| **golang** | 构建/测试/调试（Delve/DAP）/性能/代码生成/Guru，见 localleader |
| **dirvish** | dired 现代化；`a` 书签 / `TAB` 子树 / `M-t` 全屏 |
| **magit** | magit-delta 渲染语法高亮 diff（需 `git-delta`） |
| **blamer** | 行内 git blame，默认关；`M-x blamer-mode` 按需开 |
| **translate** | Google 翻译，中英自动互译 |
| **layout** | 窗口布局保存/恢复 + 预设模板 |

### 写作 / 文档 / 数据科学

| 模块 | 功能 |
|------|------|
| **org** | valign 表格、org-modern/appear 美化、居中阅读、super-agenda 分组、org-roam(+UI)、ox-gfm、org-pandoc-import |
| **markdown** | `.md` → gfm-mode、标题分级放大、行内数学、列表 2 空格缩进 |
| **preview** | org/markdown 实时预览，`SPC m v` 下 xwidget（右侧内嵌）/ 浏览器 |
| **org Jupyter** | `jupyter-python` 代码块内联执行（python3 kernel + matplotlib 内联绘图） |

## 主要快捷键

### 全局

| 快捷键 | 功能 |
|--------|------|
| `SPC SPC` | Execute command (M-x) |
| `s-p` | 切换 Buffer |
| `SPC c ~` / `SPC c _` | 命名风格循环 / 下划线↔驼峰 |
| `C-c o o` | combobulate 结构化编辑菜单（ts-mode 内） |

### AI 生成（`SPC o l g`）

| 快捷键 | 功能 |
|--------|------|
| `SPC o l g c` | 注释 → 代码（插到注释下方） |
| `SPC o l g s` | 总结选区 |
| `SPC o l g e` | 扩展描述/提纲为正文 |
| `SPC o l g p` | 对选区执行自定义指令 |

### 预览（org / markdown，`SPC m v`）

| 快捷键 | 功能 |
|--------|------|
| `SPC m v v` | 预览（自动选 xwidget/浏览器） |
| `SPC m v x` / `b` | 强制 xwidget / 浏览器 |
| `SPC m v q` | 停止保存自动刷新 |

### 翻译（`SPC T`）

| 快捷键 | 功能 |
|--------|------|
| `SPC T t` / `q` / `r` | 光标词翻译 / 交互翻译 / 反向翻译 |

### 窗口布局（`SPC l`）

| 快捷键 | 功能 |
|--------|------|
| `SPC l a` / `u` / `r` / `m` | 布局菜单 / 撤销 / 重做 / 最大化 |

### Go 开发（localleader `,`）

| 前缀 | 功能 |
|------|------|
| `, g` / `, t` / `, r` / `, b` / `, d` / `, u` / `, m` | 代码生成 / 测试 / 运行 / 构建 / 调试 / Guru 分析 / 完整菜单 |

### Lisp 结构编辑（evil normal，lisp 系语言）

| 键 | 功能 |
|----|------|
| `>)` / `<)` | 向右 slurp / barf |
| `M-(` / `M-)` | 在前/后包一对括号 |
| `M-j` / `M-k` | 当前 form 下移 / 上移 |

## Jupyter / 数据科学

org 文件里写 `jupyter-python` 代码块，光标置块内 `C-c C-c` 执行（异步，结果含图自动内联）：

```org
#+begin_src jupyter-python
import matplotlib.pyplot as plt
plt.plot([1,2,3],[1,4,9]); plt.show()
#+end_src
```

- kernel：`python3`（ipykernel，装在 pyenv 3.12.2，含 numpy/pandas/matplotlib）
- `M-x jupyter-run-repl` 开独立 REPL
- 默认 header：异步 / 共享 session `py` / kernel `python3`

## 缩进标准

- 全局 **4 空格**（`indent-tabs-mode nil`）
- **Web**（HTML/CSS/JS/TS/JSON/Vue）：**2 空格**；存在 `.editorconfig` 时以其为准
- **Go**：真实 Tab（遵从 gofmt）

## 环境变量

```bash
export GPTEL_ZHIPU_API_KEY="your_zhipu_ai_key"
export GPTEL_DEEPSEEK_API_KEY="your_deepseek_key"
# 可选：指定 lsp-bridge 的 python（需带 epc/orjson）
export LSP_BRIDGE_PYTHON="$HOME/.pyenv/versions/3.12.2/bin/python3"
```

## 外部依赖（按需）

| 工具 | 用途 | 安装 |
|------|------|------|
| `jupyter` + `ipykernel` | 数据科学 | `pip install jupyter ipykernel` |
| `pandoc` | markdown 预览 / org 导入导出 | `brew install pandoc` |
| `git-delta` | magit 语法高亮 diff | `brew install git-delta` |
| `coreutils`（`gls`） | dirvish 目录优先排序 | `brew install coreutils` |
| `clojure-lsp` | Clojure 诊断/补全 | `brew install clojure-lsp/brew/clojure-lsp` |
| Ollama + `qwen2.5-coder:3b` | minuet 本地补全 | `ollama pull qwen2.5-coder:3b` |

## 维护命令

```bash
doom sync      # 同步配置
doom upgrade   # 更新包
doom doctor    # 检查问题
```

## 系统要求

- **平台**: macOS（主要）
- **Emacs**: 30+（tree-sitter / xwidget / 原生编译）
- **Doom Emacs**: 3.0+
- **字体**: JetBrainsMono Nerd Font
