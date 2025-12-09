# Doom Emacs Configuration

个人化的 Doom Emacs 配置，集成了 AI 工具。

## 特色功能

- **字体主题**: JetBrainsMono Nerd Font (14pt) + Doom One 主题
- **AI 集成**: Claude Code, ChatGPT Shell, Gptel (智谱AI, DeepSeek)
- **开发工具**: Magit, Docker, Tree-sitter, 自动格式化
- **界面优化**: Vertico 补全, 工作区, Treemacs 文件树

## 快速开始

```bash
# 同步配置
doom sync

# 重启 Emacs
```

## 目录结构

```
~/.doom.d/
├── init.el              # Doom 模块配置
├── config.el            # 用户配置
├── packages.el          # 包声明
└── modules/my/          # 自定义模块
    ├── claudecode/      # Claude Code 集成
    ├── chatgpt-shell/   # ChatGPT Shell
    ├── codeium/         # AI 代码补全
    └── agentshell/      # Agent Shell
```

## 自定义模块

### claudecode
- **功能**: Claude Code 集成，使用 vterm 后端
- **依赖**: monet, inheritenv, claude-code.el
- **配置**: 自动启动 Monet 服务器环境

### chatgpt-shell  
- **功能**: 多 AI 提供商聊天界面
- **依赖**: shell-maker, chatgpt-shell
- **支持**: OpenAI, 智谱 AI 等

### codeium
- **功能**: AI 代码补全，集成智谱 AI 模型
- **依赖**: codeium.el, inheritenv
- **模型**: GLM-4，API 端点：https://open.bigmodel.cn/api/coding/paas/v4
- **快捷键**: `C-<return>` 接受建议，`C-[` 触发补全

### agentshell
- **功能**: Agent Shell 交互支持
- **说明**: 基于 Agent 的 Shell 增强功能

### lsp-bridge
- **功能**: LSP 桥接功能
- **说明**: 提供 enhanced LSP 支持

### jinx
- **功能**: 现代拼写检查器
- **说明**: 替代传统拼写检查工具

## 主要快捷键

- `SPC SPC` - 执行命令
- `s-p` - 切换缓冲区
- `t h/l/k` - 标签页导航

## AI 配置

需要设置环境变量：

```bash
export GPTEL_ZHIPU_API_KEY="your_key"
export GPTEL_DEEPSEEK_API_KEY="your_key"
export CODEIUM_ZHIPU_API_KEY="your_zhipu_key"
```

## 维护命令

```bash
doom sync    # 同步配置
doom upgrade # 更新
doom clean   # 清理
```

---

基于 Doom Emacs 3.0+，主要在 macOS 上使用。
