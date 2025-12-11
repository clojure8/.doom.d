# C/C++ 模块

这是一个为 Doom Emacs 定制的 C/C++ 开发模块，专门配合 lsp-bridge 使用，提供完整的 C/C++ 开发体验。

## 功能特性

### 核心功能
- **C/C++ 语法支持** - 完整的 C/C++ 语法高亮和编辑
- **LSP 支持** - 使用 lsp-bridge 配合 clangd，提供智能补全和错误检查
- **现代 C++ 支持** - C++11/14/17/20/23 特性高亮
- **多构建系统** - CMake 和 Makefile 支持

### 项目管理
- **项目脚手架** - 快速创建 CMake 和 Makefile 项目
- **构建系统集成** - 完整的构建、清理、运行支持
- **多配置支持** - Debug、Release 等构建配置

### 调试功能
- **断点调试** - 基于 DAP 的现代调试体验
- **GDB/LLDB 集成** - 完整的调试器支持
- **进程附加** - 调试运行中的程序
- **内存检查** - Valgrind 集成

### 代码质量
- **代码格式化** - clang-format 自动格式化
- **静态分析** - cppcheck、clang-tidy 集成
- **内存检查** - Valgrind 内存泄漏检测
- **反汇编** - 查看生成的汇编代码

## 完整键绑定

### Transient 菜单 (`SPC m m`)
使用 `SPC m m` 打开交互式 transient 菜单，提供所有 C/C++ 开发功能的可视化界面。

### 项目管理 (`SPC m p`)
- `p c` - 创建 CMake 项目
- `p m` - 创建 Makefile 项目

### 构建相关 (`SPC m b`)
- `b c` - CMake 配置
- `b b` - CMake 构建
- `b C` - CMake 清理
- `b m` - Make 构建
- `b d` - 调试构建
- `b r` - 发布构建
- `b M` - Make 清理
- `b f` - 编译单个文件
- `b R` - 运行可执行文件

### 调试功能 (`SPC m d`)
- `d d` - 调试可执行文件
- `d a` - 附加到进程调试
- `d b` - 切换断点
- `d c` - 继续执行
- `d n` - 单步跳过
- `d i` - 单步进入
- `d o` - 单步跳出

### 代码质量 (`SPC m l`)
- `l f` - 格式化缓冲区
- `l r` - 格式化选中区域
- `l s` - 静态代码分析
- `l v` - Valgrind 内存检查

### 测试相关 (`SPC m t`)
- `t t` - 运行测试
- `t v` - Valgrind 检查

### 运行相关 (`SPC m r`)
- `r r` - 编译并运行当前文件
- `r e` - 运行可执行文件

### 工具管理 (`SPC m x`)
- `x d` - 反汇编当前函数
- `x i` - 安装开发工具

## 依赖要求

### 必需工具
```bash
# GCC 或 Clang 编译器
gcc --version
# 或
clang --version

# CMake (推荐)
cmake --version

# Make
make --version
```

### LSP 服务器
```bash
# clangd (推荐)
# Ubuntu/Debian
sudo apt install clangd

# macOS
brew install llvm

# 或者从 LLVM 官网下载
```

### 调试工具
```bash
# GDB
sudo apt install gdb

# LLDB (macOS 默认)
# Ubuntu/Debian
sudo apt install lldb
```

### 可选工具
```bash
# Valgrind (内存检查)
sudo apt install valgrind

# 静态分析工具
sudo apt install cppcheck
sudo apt install clang-tidy

# 代码格式化
sudo apt install clang-format
```

## 项目结构模板

### CMake 项目结构
```
cmake-project/
├── CMakeLists.txt
├── src/
│   └── main.cpp
├── include/
│   └── project.h
├── tests/
├── build/
├── .gitignore
└── README.md
```

### Makefile 项目结构
```
makefile-project/
├── Makefile
├── src/
│   └── main.c
├── include/
├── obj/
└── bin/
```

## 开发工作流

### CMake 项目开发流程
1. `SPC m p c` - 创建 CMake 项目
2. `SPC m b c` - 配置 CMake (选择 Debug/Release)
3. `SPC m b b` - 构建项目
4. `SPC m r e` - 运行可执行文件
5. `SPC m d d` - 调试程序

### Makefile 项目开发流程
1. `SPC m p m` - 创建 Makefile 项目
2. `SPC m b m` - 构建项目
3. `SPC m b d` - 调试构建
4. `SPC m r e` - 运行程序

### 单文件开发流程
1. 创建 .c 或 .cpp 文件
2. `SPC m r r` - 编译并运行当前文件
3. `SPC m d d` - 调试程序

### 调试工作流
1. `SPC m d b` - 设置断点
2. `SPC m d d` - 启动调试
3. `SPC m d c` - 继续执行到断点
4. `SPC m d n` - 单步执行
5. 检查变量和内存状态

## 代码质量工作流

### 静态分析流程
1. `SPC m l s` - 运行静态分析
2. 选择分析工具 (cppcheck/clang-tidy/scan-build)
3. 根据报告修复问题

### 内存检查流程
1. `SPC m b d` - 调试构建
2. `SPC m l v` - Valgrind 内存检查
3. 修复内存泄漏和错误

### 代码格式化
1. `SPC m l f` - 格式化整个文件
2. `SPC m l r` - 格式化选中区域
3. 保存时自动格式化 (如果启用)

## 配置说明

- 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
- 自动设置 CC、CXX、CFLAGS 等环境变量
- 使用 Linux 代码风格，基础缩进 4 空格
- 保存时自动格式化代码 (如果安装了 clang-format)
- 集成现代 C++ 字体锁定
- 支持 CMake 和 Makefile 项目

## 特色功能

### 智能项目检测
- 自动检测项目类型 (CMake/Makefile)
- 智能选择合适的构建命令
- 自动配置编译器和标准

### 现代 C++ 支持
- C++11/14/17/20/23 语法高亮
- 现代 C++ 特性支持
- 智能代码补全和错误检查

### 完整的调试体验
- 可视化断点管理
- 变量检查和监视
- 调用栈和内存查看
- 进程附加调试

### 多工具集成
- 多种静态分析工具
- 内存检查和性能分析
- 代码格式化和风格检查
- 反汇编和底层分析

## 最佳实践

### 项目组织
- 使用 CMake 管理复杂项目
- 分离头文件和源文件
- 使用合适的目录结构

### 代码质量
- 启用所有编译器警告
- 定期运行静态分析
- 使用 Valgrind 检查内存
- 保持一致的代码风格

### 调试策略
- 使用断点而非 printf 调试
- 检查内存泄漏和越界
- 利用调试器的高级功能
- 编写可调试的代码

### 构建配置
- 区分 Debug 和 Release 构建
- 使用适当的优化级别
- 配置合适的编译标志
- 管理依赖和链接