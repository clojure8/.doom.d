# Doom Emacs Configuration

个人化的 Doom Emacs 配置，集成 AI 工具和多语言开发环境，针对 macOS + 中文用户优化。

## 特色功能

- **AI 集成套件**: Agent Shell、Gptel (智谱 AI / DeepSeek)、Minuet 本地 LLM 补全、Claude Code IDE
- **多语言开发**: Go、Rust、Python、JavaScript/TypeScript、Clojure、Swift、Zig 等 (基于 lsp-bridge + tree-sitter)
- **现代化界面**: JetBrainsMono 字体、doom-xcode 主题、Vertico 补全、Treemacs 文件树
- **开发工具链**: Magit、Docker、Tree-sitter、保存时自动格式化、多光标编辑
- **系统优化**: macOS 支持、中文输入法、Google Translate、全局 4 空格缩进

## 快速开始

```bash
# 首次安装 Doom Emacs
git clone https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# 克隆本配置
git clone <this-repo> ~/.doom.d

# 同步配置
doom sync

# 重启 Emacs
```

## 项目结构

```
~/.doom.d/
├── init.el                  # Doom 模块配置
├── config.el                # 核心用户配置
├── packages.el              # 外部包声明
└── modules/my/              # 自定义模块
    ├── agentshell/          # Agent Shell / Claude Agent 集成
    ├── awesome-tray/        # 状态栏美化 (可选)
    ├── claudecode/          # Claude Code IDE 集成
    ├── golang/              # Go 扩展开发工具
    ├── gptel/               # AI 聊天客户端
    ├── jinx/                # 现代拼写检查 (可选)
    ├── layout/              # 窗口布局管理
    ├── lsp-bridge/          # 高性能 LSP 补全
    ├── minuet/              # 本地 LLM 代码补全 (Ollama)
    ├── org/                 # Org-mode 增强
    ├── reader/              # 电子书阅读器
    └── translate/           # Google 翻译集成
```

## 核心模块

### AI 集成

| 模块 | 功能 |
|------|------|
| **agentshell** | Agent Protocol + Claude Agent Shell，AI Coding Partner |
| **claudecode** | Claude Code IDE（`SPC c c` / `C-c C-'` 打开菜单） |
| **gptel** | 多后端 AI 聊天：智谱 AI (glm-5.1)、DeepSeek |
| **minuet** | Ollama 本地 LLM 行内代码补全（qwen2.5-coder:3b） |

### 开发语言（init.el :lang）

通过 Doom `:lang` 模块启用，均配备 LSP + Tree-sitter：

Go、Python (uv/pyenv/pyright)、JavaScript/TypeScript、Rust、Clojure、
Java、Swift、Lua、Nim、Zig、GraphQL、YAML、JSON、Markdown、LaTeX、
Web (HTML/CSS)、Shell、Org-mode、PureScript

### 工具模块

| 模块 | 功能 |
|------|------|
| **golang** | Go 完整工具链：构建/测试/调试 (Delve/DAP)、性能分析、代码生成、Guru 分析 |
| **lsp-bridge** | Python 实现的高性能 LSP 补全前端，全局启用 |
| **layout** | 窗口布局保存/恢复，支持预设模板和自动保存 |
| **translate** | Google Translate 集成，支持中英自动互译 |
| **org** | Org-mode 增强：valign 表格对齐、org-modern 美化、org-appear 动态标记 |
| **reader** | Emacs 电子书阅读器 |

## 主要快捷键

### 全局

| 快捷键 | 功能 |
|--------|------|
| `SPC SPC` | Execute command (M-x) |
| `s-p` | 切换 Buffer |
| `SPC c c` | Claude Code IDE 菜单 |
| `C-c C-'` | Claude Code IDE 菜单 (备用) |

### Tab 管理（centaur-tabs）

| 快捷键 | 功能 |
|--------|------|
| `SPC t l` / `t l` | 下一个 Tab |
| `SPC t h` / `t h` | 上一个 Tab |
| `SPC t k` / `t k` | 关闭当前 Tab |

### 翻译（SPC T）

| 快捷键 | 功能 |
|--------|------|
| `SPC T t` | 翻译光标词/选中文本 |
| `SPC T q` | 交互式翻译 |
| `SPC T r` | 反向翻译（中↔英） |

### 窗口布局（SPC l）

| 快捷键 | 功能 |
|--------|------|
| `SPC l a` | 布局管理菜单 (transient) |
| `SPC l u` | 撤销布局变化 |
| `SPC l r` | 重做布局变化 |
| `SPC l m` | 最大化当前窗口 |

### Go 开发（localleader）

| 前缀 | 功能 |
|------|------|
| `, g` | 代码生成（tag/impl/doc/rename） |
| `, t` | 测试（生成/运行/覆盖率） |
| `, r` | 运行（当前文件/main/playground/REPL） |
| `, b` | 构建（build/vet/generate/clean） |
| `, d` | 调试（DAP/Delve，步进/断点/变量） |
| `, u` | Guru 代码分析（引用/调用链/实现） |
| `, m` | 完整 transient 菜单 |

## 缩进标准

全局 **4 空格**（`indent-tabs-mode nil`），以下例外：

- **Go**：真实 Tab（`indent-tabs-mode t`，遵从 gofmt 规范）

## 环境变量

```bash
# AI 服务 API 密钥
export GPTEL_ZHIPU_API_KEY="your_zhipu_ai_key"
export GPTEL_DEEPSEEK_API_KEY="your_deepseek_key"
```

## 维护命令

```bash
doom sync      # 同步配置
doom upgrade   # 更新包
doom clean     # 清理缓存
doom doctor    # 检查问题
```

## 系统要求

- **平台**: macOS（主要）
- **Emacs**: 29+（tree-sitter 原生支持）
- **Doom Emacs**: 3.0+
- **字体**: JetBrainsMono Nerd Font
- **本地 LLM（可选）**: Ollama + qwen2.5-coder:3b
