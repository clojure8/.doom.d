# Doom Emacs Configuration

个人化的 Doom Emacs 配置，集成了 AI 工具和多语言开发环境。

## 🌟 特色功能

- **AI 集成套件**: Claude Code、Gptel (DeepSeek/智谱 AI)、ChatGPT Shell、Agent Shell
- **多语言开发**: Go、Rust、Python、Vue、React、Clojure、C、Zig (基于 lsp-bridge)
- **现代化界面**: JetBrainsMono 字体、Awesome Tray 状态栏、Vertico 补全、Treemacs 文件树
- **开发工具链**: Magit Git、Docker、Tree-sitter、自动格式化、多光标编辑
- **系统优化**: macOS 支持、中文输入法、拼写检查 (Jinx)、Unicode 支持

## 🚀 快速开始

```bash
# 首次安装 Doom Emacs
git clone https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# 同步配置
doom sync

# 重启 Emacs
```

## 📁 项目结构

```
~/.doom.d/
├── init.el                  # Doom 模块配置
├── config.el                # 核心用户配置
├── packages.el              # 外部包声明
└── modules/my/              # 自定义模块
    ├── agentshell/          # Agent Shell 模块
    ├── awesome-tray/        # 状态栏美化
    ├── c/                   # C/C++ 开发
    ├── chatgpt-shell/       # ChatGPT Shell
    ├── claudecode/          # Claude Code 集成
    ├── clojure/             # Clojure 开发
    ├── golang/              # Go 开发
    ├── gptel/               # AI 聊天客户端
    ├── lsp-bridge/          # LSP 桥接
    ├── python/              # Python 开发 (uv 支持)
    ├── react/               # React 开发
    ├── rust/                # Rust 开发
    ├── vue/                 # Vue 开发
    └── zig/                 # Zig 开发
```

## 🔧 核心模块

### 🤖 AI 集成
- **Claude Code**: vterm 后端集成
- **Gptel**: DeepSeek/智谱 AI 支持
- **ChatGPT Shell**: 多 AI 提供商界面
- **Agent Shell**: AI Coding Partner 集成

### 💻 开发语言
- **Go**: 项目管理、构建、测试、调试 (Delve)
- **Rust**: Cargo 集成、测试、格式化
- **Python**: uv 包管理、Jupyter 支持、虚拟环境
- **Vue**: Vue 2/3 支持、组件开发
- **React**: JSX/TSX 支持、现代 React 开发
- **Clojure**: REPL 集成、函数式编程
- **C/C++**: CMake 支持、现代 C++ 特性
- **Zig**: 系统编程、交叉编译

### 🎨 界面增强
- **Awesome Tray**: 自定义状态栏
- **LSP Bridge**: 高性能 LSP 支持
- **Org Mode**: 美化配置和中文优化

## ⌨️ 主要快捷键

- `SPC SPC` - 执行命令 (M-x)
- `SPC m m` - 语言模块 transient 菜单
- `s-p` - 缓冲区切换
- `SPC o a` - AI 工具面板

## 🔑 环境变量

```bash
# AI 服务 API 密钥
export GPTEL_ZHIPU_API_KEY="your_zhipu_ai_key"
export GPTEL_DEEPSEEK_API_KEY="your_deepseek_key"
export CLAUDE_API_KEY="your_claude_api_key"
```

## 🛠️ 维护命令

```bash
doom sync      # 同步配置
doom upgrade   # 更新包
doom clean     # 清理缓存
doom doctor    # 检查问题
```

## 📦 系统要求

- **平台**: macOS (主要), Linux, Windows (WSL)
- **Emacs**: 27.1+
- **Doom Emacs**: 3.0+
- **字体**: JetBrainsMono Nerd Font

---

> 💡 为中文用户优化的 Doom Emacs 配置，集成现代化 AI 工作流和多语言开发环境。