# Rust 模块

这是一个为 Doom Emacs 定制的 Rust 开发模块，专门配合 lsp-bridge 使用，提供完整的 Rust 开发体验。

## 功能特性

### 核心功能
- **Rust 语法高亮和编辑支持** - 通过 `rust-mode` 提供
- **LSP 支持** - 使用 lsp-bridge 配合 rust-analyzer，提供代码补全、跳转、重构等功能
- **代码格式化** - 保存时自动使用 `rustfmt` 格式化代码
- **Cargo 集成** - 完整的 Cargo 项目管理支持

### 代码质量
- **Clippy 集成** - 代码质量检查和建议
- **Flycheck 支持** - 实时语法和错误检查
- **代码格式化** - 自动 rustfmt 格式化

### 项目管理
- **Cargo 命令** - 完整的 Cargo 工作流支持
- **依赖管理** - 添加、更新、审计依赖
- **项目模板** - 快速创建二进制和库项目
- **工作空间支持** - 多包项目管理

### 开发工具
- **Rust Playground** - 在线代码测试环境
- **性能分析** - 火焰图和性能分析工具
- **监视模式** - 文件变化自动构建/测试
- **宏展开** - 查看宏展开结果

## 完整键绑定

### Transient 菜单 (`SPC m m`)
使用 `SPC m m` 打开交互式 transient 菜单，提供所有 Rust 开发功能的可视化界面。菜单包含：
- 基础操作（格式化、Clippy 检查、文档生成）
- 运行操作（运行项目、示例、监视模式）
- 构建操作（构建、发布构建、快速检查）
- 测试功能（运行测试、基准测试、监视测试）
- 依赖管理（添加、更新、审计依赖）
- 项目管理（创建项目、feature 管理）
- 性能分析（火焰图、性能分析）
- 工具管理（安装工具、宏展开、工作空间）

### 基础操作 (`SPC m r`)
- `r f` - 格式化代码 (rustfmt)
- `r c` - 运行 Clippy 检查
- `r d` - 生成文档
- `r D` - 生成并打开文档
- `r p` - 打开 Rust Playground

### 运行相关 (`SPC m e`)
- `e e` - 运行项目
- `e E` - 带参数运行项目
- `e x` - 运行示例
- `e w` - 监视运行 (cargo watch)

### 构建相关 (`SPC m b`)
- `b b` - 构建项目
- `b r` - 发布构建
- `b c` - 快速检查 (cargo check)
- `b C` - 清理构建产物
- `b w` - 监视检查

### 测试相关 (`SPC m t`)
- `t t` - 运行所有测试
- `t f` - 测试当前文件
- `t b` - 运行基准测试
- `t c` - Criterion 基准测试
- `t w` - 监视测试

### 依赖管理 (`SPC m d`)
- `d a` - 添加依赖
- `d u` - 更新依赖
- `d A` - 安全审计
- `d o` - 检查过时依赖
- `d t` - 显示依赖树

### 项目管理 (`SPC m p`)
- `p n` - 创建新的二进制项目
- `p l` - 创建新的库项目
- `p i` - 在当前目录初始化项目
- `p f` - 添加 feature
- `p F` - 切换 feature 编译

### 工作空间 (`SPC m w`)
- `w s` - 设置工作空间

### 性能分析 (`SPC m P`)
- `P f` - 生成火焰图
- `P r` - 性能分析发布版本

### 工具管理 (`SPC m x`)
- `x i` - 安装常用开发工具
- `x m` - 展开宏

## 依赖要求

### 必需工具
```bash
# 安装 Rust 工具链
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# 安装 rust-analyzer
rustup component add rust-analyzer
```

### 推荐工具（通过 `SPC m x i` 自动安装）
```bash
cargo install cargo-edit      # 依赖管理
cargo install cargo-audit     # 安全审计
cargo install cargo-outdated  # 检查过时依赖
cargo install cargo-tree      # 依赖树
cargo install cargo-expand    # 宏展开
cargo install cargo-watch     # 文件监视
cargo install cargo-flamegraph # 火焰图
cargo install cargo-criterion # 基准测试
```

## 项目结构模板

### 二进制项目结构
```
project-name/
├── src/
│   └── main.rs
├── Cargo.toml
└── README.md
```

### 库项目结构
```
project-name/
├── src/
│   └── lib.rs
├── Cargo.toml
└── README.md
```

### 工作空间结构
```
workspace-name/
├── Cargo.toml
├── project1/
│   ├── src/
│   └── Cargo.toml
└── project2/
    ├── src/
    └── Cargo.toml
```

## 开发工作流

### 基本开发流程
1. `SPC m p n` - 创建新项目
2. `SPC m d a` - 添加依赖
3. `SPC m e e` - 运行项目
4. `SPC m t t` - 运行测试
5. `SPC m r c` - 代码检查
6. `SPC m b r` - 发布构建

### 监视开发模式
1. `SPC m e w` - 监视运行
2. `SPC m t w` - 监视测试
3. `SPC m b w` - 监视检查

### 性能优化流程
1. `SPC m t b` - 基准测试
2. `SPC m P f` - 生成火焰图
3. `SPC m P r` - 性能分析

## 配置说明

- 禁用了 `racer` 和 `company-racer`，完全依赖 lsp-bridge 和 rust-analyzer
- 自动设置 CARGO_HOME、RUSTUP_HOME 等环境变量
- 保存时自动格式化代码
- 集成 Flycheck 进行实时错误检查
- 支持 Cargo.toml 智能补全
- 自动检测并启用 Cargo 项目模式

## 特色功能

### Feature 管理
- 动态添加和切换 Cargo features
- 支持条件编译开发

### 工作空间支持
- 多包项目管理
- 统一的依赖和构建管理

### 性能分析
- 集成火焰图生成
- 支持多种性能分析工具

### 监视模式
- 文件变化自动构建
- 实时测试反馈
- 快速开发迭代