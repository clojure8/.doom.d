# Doom Emacs Configuration

个人化的 Doom Emacs 配置，集成了 AI 工具和现代化的用户界面。

## 🌟 特色功能

- **现代化界面**:
  - JetBrainsMono Nerd Font (14pt) + Doom One 主题
  - Awesome Tray 状态栏 + Nano Modeline 极简模式栏
  - Vertico 补全框架 + 工作区管理
  - Treemacs 文件树 + 平滑滚动

- **AI 集成套件**:
  - Claude Code (vterm 后端，Monet 服务器集成)
  - Gptel (支持 DeepSeek 和智谱 AI)
  - ChatGPT Shell (多 AI 提供商支持)
  - Agent Shell (智能 Shell 交互)

- **开发工具链**:
  - Magit Git 管理 + Docker 集成
  - Tree-sitter 语法高亮 + LSP Bridge
  - 自动代码格式化 + 多光标编辑
  - 文件模板 + 代码片段

- **系统优化**:
  - macOS 原生支持 + 中文输入法
  - 增强的拼写检查 (Jinx)
  - Unicode 全支持 + 终端模拟器

## 🚀 快速开始

```bash
# 首次安装配置
git clone https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install

# 同步配置文件
doom sync

# 重启 Emacs 使配置生效
```

## 📁 项目架构

```
~/.doom.d/
├── init.el                  # Doom 模块定义和加载顺序
├── config.el                # 核心用户配置 (字体、主题、基本设置)
├── packages.el              # 外部包声明 (GitHub 依赖)
├── CLAUDE.md                # Claude Code 指导文档
├── README.md                # 项目说明文档
└── modules/my/              # 自定义模块目录
    ├── ai/                  # AI 核心集成模块
    │   ├── config.el        # Monet 服务器和 Claude Code 配置
    │   └── packages.el      # AI 相关包声明
    ├── claudecode/          # Claude Code 专用模块
    │   ├── config.el        # Claude Code vterm 后端配置
    │   └── packages.el      # Claude Code 依赖包
    ├── gptel/               # Gptel AI 聊天模块
    │   ├── config.el        # DeepSeek 和智谱 AI 配置
    │   └── packages.el      # Gptel 包声明
    ├── chatgpt-shell/       # ChatGPT Shell 模块
    │   ├── config.el        # 多 AI 提供商界面配置
    │   └── packages.el      # Shell-maker 和相关依赖
    ├── agentshell/          # Agent Shell 模块
    │   ├── config.el        # Agent Shell 交互配置
    │   └── packages.el      # acp 和相关包声明
    ├── awesome-tray/        # 状态栏美化模块
    │   ├── config.el        # Awesome Tray 配置和模式栏隐藏
    │   └── packages.el      # awesome-tray 和 hide-mode-line
    ├── nano-modeline/       # 极简模式栏模块
    │   ├── config.el        # Nano Modeline 配置
    │   └── packages.el      # nano-modeline 包声明
    ├── lsp-bridge/          # LSP 桥接模块
    │   ├── config.el        # LSP Bridge 配置
    │   └── packages.el      # lsp-bridge 包声明
    └── jinx/                # 现代拼写检查模块
        ├── config.el        # Jinx 配置
        └── packages.el      # jinx 包声明
```

## 🔧 核心模块详解

### 🤖 AI 集成模块

#### `ai/` - 核心 AI 集成
- **功能**: 统一的 AI 工具集成框架
- **依赖**: monet (环境管理), inheritenv (环境继承), claude-code.el
- **特性**: Monet 服务器自动启动和环境管理

#### `claudecode/` - Claude Code 专用
- **功能**: Claude Code CLI 的 Emacs 集成
- **后端**: vterm 终端模拟器
- **特性**: 环境变量继承和会话管理

#### `gptel/` - AI 聊天客户端
- **后端配置**:
  - **DeepSeek**: API 端点 `api.deepseek.com`
  - **智谱 AI**: API 端点 `open.bigmodel.cn/api/coding/paas/v4`
- **模型**: GLM-4.6, GLM-4.5, GLM-4.5-air, deepseek-chat, deepseek-coder
- **环境变量**: `GPTEL_DEEPSEEK_API_KEY`, `GPTEL_ZHIPU_API_KEY`

#### `chatgpt-shell/` - 多 AI 提供商 Shell
- **功能**: 基于 shell-maker 的多 AI 聊天界面
- **支持**: OpenAI, 智谱 AI, DeepSeek 等
- **特性**: 统一的聊天界面和提供商切换

#### `agentshell/` - 智能 Shell
- **功能**: AI Coding Partner (acp) 集成
- **特性**: 基于 Agent 的 Shell 增强功能

### 🎨 界面美化模块

#### `awesome-tray/` - 状态栏增强
- **功能**: 自定义状态栏和信息显示
- **模块**: evil, buffer-name, file-path, git, mode
- **特性**:
  - 自动隐藏原有模式栏
  - 适用于 Treemacs, Magit 等特殊模式
  - 自定义颜色和高度

#### `nano-modeline/` - 极简模式栏
- **功能**: Nano 风格的极简模式栏
- **支持模式**: prog-mode, text-mode, org-mode, term-mode 等
- **特性**:
  - 完全隐藏原有模式栏
  - 统一的视觉风格
  - 专用窗口标识符

### 🔧 工具增强模块

#### `lsp-bridge/` - LSP 桥接
- **功能**: 增强的 LSP 支持
- **特性**: 更快的 LSP 响应和更好的性能

#### `jinx/` - 现代拼写检查
- **功能**: 替代传统 flyspell 的现代化拼写检查器
- **特性**: 更准确的拼写建议和更好的性能

## ⌨️ 主要快捷键

### 导航和操作
- `SPC SPC` - 执行扩展命令 (M-x)
- `s-p` - 在缓冲区间切换
- `t h/l/k` - 工作区标签页导航 (左/下/上/右)

### AI 工具
- `C-<return>` - Codeium 接受代码建议
- `C-[` - Codeium 触发代码补全
- `SPC o a` - 打开 AI 工具面板 (自定义)

### 界面控制
- `SPC t t` - 切换主题
- `SPC w m` - 切换模式栏显示/隐藏

## 🔑 环境变量配置

在 `~/.zshrc` 或 `~/.bashrc` 中设置：

```bash
# AI 服务 API 密钥
export GPTEL_ZHIPU_API_KEY="your_zhipu_ai_key"
export GPTEL_DEEPSEEK_API_KEY="your_deepseek_key"
export CODEIUM_ZHIPU_API_KEY="your_codeium_zhipu_key"

# Claude Code (可选)
export CLAUDE_API_KEY="your_claude_api_key"
```

## 🛠️ 维护和故障排除

### 常用命令
```bash
# 同步配置 (修改 init.el 或 packages.el 后)
doom sync

# 更新 Doom 和所有包
doom upgrade

# 清理编译文件和缓存
doom clean

# 重新编译包
doom build

# 检查配置问题
doom doctor
```

### 配置修改后处理
1. **修改 `init.el`**: 必须运行 `doom sync`
2. **修改 `config.el`**: 重启 Emacs 或使用 `M-x doom/reload`
3. **修改模块配置**: 运行 `doom sync` 并重启
4. **添加新包**: 运行 `doom sync` 安装

### 常见问题
- **启动错误**: 检查 `*Messages*` 缓冲区
- **包安装失败**: 确保网络连接和 GitHub 可访问
- **AI 工具无响应**: 检查 API 密钥和环境变量
- **字体显示问题**: 确保 JetBrainsMono Nerd Font 已安装

## 📦 依赖包管理

### 外部包源
- **GitHub 仓库**: 通过 `:recipe` 指定
- **MELPA/ELPA**: Doom 默认包仓库
- **本地包**: 放置在 `modules/` 目录下

### 模块开发模式
- 遵循 Doom 模块结构: `modules/category/module-name/{config.el,packages.el}`
- 使用 `(package! name :recipe ...)` 声明外部依赖
- 使用 `(use-package! name ...)` 在 config.el 中配置
- 自定义模块放置在 `:my` 部分

## 🌐 系统兼容性

- **主要平台**: macOS (Darwin)
- **支持平台**: Linux, Windows (WSL)
- **Emacs 版本**: 27.1+
- **Doom Emacs**: 3.0+

## 📄 许可证

本项目为个人配置，遵循 MIT 许可证。

---

> 💡 **提示**: 这是为中文用户优化的 Doom Emacs 配置，集成了现代化的 AI 工作流和美观的用户界面。
