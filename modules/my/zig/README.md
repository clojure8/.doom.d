# Zig 模块

这是一个为 Doom Emacs 定制的 Zig 开发模块，专门配合 lsp-bridge 使用，提供完整的 Zig 开发体验。

## 功能特性

### 核心功能
- **Zig 语法支持** - 完整的 Zig 语法高亮和编辑
- **LSP 支持** - 使用 lsp-bridge 配合 ZLS，提供智能补全和错误检查
- **项目管理** - 完整的 Zig 项目创建和管理
- **构建系统** - 集成 Zig 构建系统

### 项目管理
- **项目脚手架** - 快速创建可执行文件和库项目
- **构建配置** - Debug、Release、性能优化构建
- **交叉编译** - 支持多平台目标构建

### 开发工具
- **代码格式化** - zig fmt 自动格式化
- **语法检查** - 实时语法和类型检查
- **测试支持** - 完整的测试运行和过滤
- **调试集成** - LLDB 调试器支持

### 高级功能
- **C 互操作** - C 代码转换和编译
- **包管理** - Zig 包获取和管理
- **性能分析** - 基准测试和性能优化
- **文档集成** - 快速访问官方文档

## 完整键绑定

### Transient 菜单 (`SPC m m`)
使用 `SPC m m` 打开交互式 transient 菜单，提供所有 Zig 开发功能的可视化界面。

### 项目管理 (`SPC m p`)
- `p i` - 初始化 Zig 项目 (zig init-exe/lib)
- `p c` - 创建新项目
- `p s` - 创建简单项目结构

### 构建相关 (`SPC m b`)
- `b b` - 构建项目
- `b r` - 发布构建 (ReleaseFast)
- `b d` - 调试构建
- `b t` - 目标平台构建
- `b p` - 性能优化构建

### 运行相关 (`SPC m r`)
- `r r` - 运行项目
- `r a` - 带参数运行
- `r f` - 运行当前文件

### 测试相关 (`SPC m t`)
- `t t` - 运行所有测试
- `t f` - 测试当前文件
- `t F` - 过滤测试
- `t b` - 运行基准测试

### 格式化和检查 (`SPC m f`)
- `f f` - 格式化当前文件
- `f p` - 格式化整个项目
- `f c` - 检查语法

### 调试功能 (`SPC m d`)
- `d d` - 调试可执行文件
- `d b` - 切换断点

### 文档和帮助 (`SPC m h`)
- `h d` - 打开 Zig 文档
- `h s` - 打开标准库文档
- `h v` - 显示版本信息
- `h e` - 显示环境信息
- `h t` - 列出支持的目标平台

### 工具相关 (`SPC m x`)
- `x c` - 将 C 代码转换为 Zig
- `x C` - 使用 Zig 作为 C 编译器
- `x o` - 反汇编可执行文件
- `x f` - 获取包

### 安装相关 (`SPC m i`)
- `i z` - 安装 Zig
- `i l` - 安装 ZLS (Zig Language Server)

## 依赖要求

### 必需工具
```bash
# Zig 编译器
# 从官网下载: https://ziglang.org/download/
# 或使用包管理器安装

# macOS
brew install zig

# Ubuntu/Debian (需要添加仓库)
# 或直接下载二进制文件
```

### LSP 服务器
```bash
# ZLS (Zig Language Server)
# 从 GitHub 下载: https://github.com/zigtools/zls/releases
# 或从源码构建:
git clone https://github.com/zigtools/zls
cd zls
zig build -Doptimize=ReleaseSafe
```

### 调试工具
```bash
# LLDB (通常随 Zig 一起提供)
# macOS: 系统自带
# Linux: 
sudo apt install lldb
```

## 项目结构模板

### 可执行文件项目
```
zig-exe-project/
├── build.zig
├── src/
│   └── main.zig
├── .gitignore
└── README.md
```

### 库项目
```
zig-lib-project/
├── build.zig
├── src/
│   ├── main.zig
│   └── root.zig
├── .gitignore
└── README.md
```

### 完整项目结构
```
zig-project/
├── build.zig
├── build.zig.zon (Zig 0.11+)
├── src/
│   ├── main.zig
│   ├── lib.zig
│   └── utils/
├── tests/
├── docs/
├── examples/
├── zig-out/
├── zig-cache/
├── .gitignore
└── README.md
```

## 开发工作流

### 基本开发流程
1. `SPC m p c` - 创建新项目
2. `SPC m b b` - 构建项目
3. `SPC m r r` - 运行项目
4. `SPC m t t` - 运行测试
5. `SPC m f f` - 格式化代码

### 快速原型开发
1. 创建 .zig 文件
2. `SPC m r f` - 直接运行当前文件
3. `SPC m t f` - 测试当前文件

### 发布构建流程
1. `SPC m b r` - 发布构建
2. `SPC m t b` - 运行基准测试
3. `SPC m x o` - 检查生成的代码

### 调试工作流
1. `SPC m b d` - 调试构建
2. `SPC m d b` - 设置断点
3. `SPC m d d` - 启动调试

### 交叉编译流程
1. `SPC m h t` - 查看支持的目标
2. `SPC m b t` - 为特定目标构建
3. 测试不同平台的兼容性

## 配置说明

- 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
- 自动设置 Zig 缓存目录环境变量
- 保存时自动格式化代码 (zig fmt)
- 设置 4 空格缩进，不使用制表符
- 集成 LLDB 调试器支持

## 特色功能

### 现代系统编程
- **内存安全** - 编译时内存安全检查
- **零成本抽象** - 高性能系统编程
- **交叉编译** - 一次编写，到处编译
- **C 互操作** - 无缝 C 代码集成

### 强大的构建系统
- **声明式构建** - build.zig 构建脚本
- **依赖管理** - 内置包管理系统
- **多目标支持** - 支持所有主流平台
- **优化选项** - 多种优化级别

### 开发体验
- **快速编译** - 极快的编译速度
- **清晰错误** - 友好的错误信息
- **实时反馈** - 即时语法检查
- **自动格式化** - 统一代码风格

### 测试和调试
- **内置测试** - 原生测试支持
- **基准测试** - 性能测试框架
- **调试信息** - 完整的调试支持
- **静态分析** - 编译时检查

## 最佳实践

### 代码组织
- 使用清晰的模块结构
- 遵循 Zig 命名约定
- 编写文档注释
- 保持函数简洁

### 内存管理
- 明确内存分配策略
- 使用适当的分配器
- 避免内存泄漏
- 利用编译时检查

### 性能优化
- 使用 ReleaseFast 进行性能测试
- 编写基准测试
- 分析生成的汇编代码
- 优化热点路径

### 跨平台开发
- 测试多个目标平台
- 使用条件编译
- 处理平台差异
- 保持代码可移植性

## Zig 特性

### 编译时计算
- comptime 关键字
- 编译时函数执行
- 类型作为值
- 泛型编程

### 错误处理
- 显式错误类型
- try/catch 语法
- 错误联合类型
- 可选类型

### 内存安全
- 无空指针解引用
- 边界检查
- 整数溢出检查
- 未定义行为检测

### C 互操作
- 直接调用 C 函数
- C 头文件导入
- 零成本 C 绑定
- C 代码转换工具