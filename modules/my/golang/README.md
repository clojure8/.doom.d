# Golang 模块

这是一个为 Doom Emacs 定制的 Golang 开发模块，专门配合 lsp-bridge 使用，提供完整的 Go 开发体验。

## 功能特性

### 核心功能
- **Go 语法高亮和编辑支持** - 通过 `go-mode` 提供
- **LSP 支持** - 使用 lsp-bridge 而非 eglot，提供代码补全、跳转、重构等功能
- **代码格式化** - 保存时自动使用 `goimports` 格式化代码
- **实时文档** - 通过 `go-eldoc` 显示函数签名和文档

### 代码生成和重构
- **标签管理** - 通过 `go-tag` 添加和移除结构体标签
- **测试生成** - 通过 `go-gen-test` 自动生成测试代码
- **接口实现** - 通过 `go-impl` 自动生成接口实现
- **结构体填充** - 通过 `go-fill-struct` 自动填充结构体字段
- **代码重命名** - 通过 `go-rename` 安全重命名符号

### 项目管理
- **模块管理** - 完整的 Go modules 支持
- **依赖管理** - 包获取、更新、清理等操作
- **项目模板** - 快速创建 CLI 和 Web 项目结构
- **构建工具** - 编译、测试、性能分析等

### 开发工具
- **Go Playground** - 在线代码测试环境
- **REPL 支持** - 交互式开发环境
- **Go Guru** - 代码分析和导航工具
- **性能分析** - CPU 和内存性能分析
- **断点调试** - 基于 Delve 的完整调试支持

## 完整键绑定

### Transient 菜单 (`SPC m m`)
使用 `SPC m m` 打开交互式 transient 菜单，提供所有 Go 开发功能的可视化界面。菜单包含：
- 基础操作（填充结构体、实现接口、重命名等）
- 标签管理（添加/移除标签）
- 运行操作（运行文件、项目、Playground）
- 构建操作（构建、检查、清理）
- 测试功能（生成测试、运行测试、覆盖率）
- 模块管理（初始化、依赖管理）
- 性能分析（CPU、内存分析）
- 调试功能（断点、单步、求值等）
- 项目模板（CLI、Web 项目）

### 基础操作 (`SPC m g`)
- `g a` - 添加结构体标签
- `g r` - 移除结构体标签
- `g i` - 实现接口
- `g f` - 填充结构体
- `g d` - 显示文档
- `g R` - 重命名符号

### 测试相关 (`SPC m t`)
- `t t` - 生成测试（智能）
- `t T` - 生成所有测试
- `t r` - 测试当前文件
- `t p` - 测试整个项目
- `t b` - 运行基准测试
- `t c` - 测试覆盖率分析

### 运行相关 (`SPC m r`)
- `r r` - 运行当前文件
- `r m` - 运行项目 main.go
- `r p` - 打开 Go Playground
- `r R` - 启动 Go REPL
- `r l` - 在 REPL 中加载当前文件

### 构建相关 (`SPC m b`)
- `b b` - 构建项目
- `b v` - 运行 go vet
- `b g` - 运行 go generate
- `b c` - 清理缓存
- `b t` - 添加构建标签

### 模块管理 (`SPC m m`)
- `m i` - 初始化模块
- `m t` - go mod tidy
- `m d` - go mod download
- `m g` - 获取包
- `m l` - 列出包
- `m D` - 显示依赖
- `m w` - 解释包依赖

### 性能分析 (`SPC m p`)
- `p c` - CPU 性能分析
- `p m` - 内存性能分析

### 工具管理 (`SPC m x`)
- `x i` - 安装常用开发工具

### Go Guru (`SPC m u`)
- `u d` - 描述符号
- `u f` - 显示自由变量
- `u i` - 显示实现
- `u c` - 显示通道操作
- `u r` - 显示引用
- `u s` - 显示调用栈
- `u e` - 显示错误
- `u p` - 指针分析
- `u <` - 显示调用者
- `u >` - 显示被调用者

### 调试功能 (`SPC m d`)
- `d d` - 调试 main 函数
- `d t` - 调试当前测试函数
- `d f` - 调试当前文件
- `d a` - 附加到进程
- `d r` - 远程调试
- `d b` - 切换断点
- `d B` - 清除所有断点
- `d c` - 继续执行
- `d n` - 单步跳过
- `d i` - 单步进入
- `d o` - 单步跳出
- `d R` - 重启调试
- `d s` - 停止调试
- `d e` - 求值表达式
- `d E` - 求值选中区域
- `d l` - 显示局部变量
- `d S` - 显示调试会话
- `d L` - 显示断点列表

### dlv 服务器 (`SPC m D`)
- `D s` - 启动 dlv 调试服务器
- `D t` - 启动 dlv 测试服务器

### 项目模板 (`SPC m n`)
- `n c` - 创建 CLI 项目
- `n w` - 创建 Web 项目

## 依赖要求

### 必需工具
```bash
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/gopls@latest
go install github.com/go-delve/delve/cmd/dlv@latest  # 调试器
```

### 可选工具（通过 `SPC m x i` 自动安装）
```bash
go install github.com/go-delve/delve/cmd/dlv@latest
go install honnef.co/go/tools/cmd/staticcheck@latest
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
```

## 项目结构模板

### CLI 项目结构
```
project-name/
├── cmd/
├── internal/
├── pkg/
├── main.go
├── README.md
├── .gitignore
└── go.mod
```

### Web 项目结构
```
project-name/
├── cmd/server/
│   └── main.go
├── internal/
│   ├── handler/
│   ├── service/
│   └── repository/
├── pkg/middleware/
├── web/
│   ├── static/
│   └── templates/
└── go.mod
```

## 开发工作流

### 基本开发流程
1. `SPC m p n` - 创建新项目
2. `SPC m m g` - 获取依赖
3. `SPC m r r` - 运行代码
4. `SPC m t t` - 运行测试
5. `SPC m c l` - 代码检查
6. `SPC m b r` - 发布构建

### 调试工作流
1. `SPC m d b` - 设置断点
2. `SPC m d d` - 启动调试（main 函数）
3. `SPC m d c` - 继续执行到断点
4. `SPC m d l` - 查看局部变量
5. `SPC m d e` - 求值表达式
6. `SPC m d n` - 单步执行
7. `SPC m d s` - 停止调试

### 远程调试流程
1. `SPC m D s` - 启动 dlv 服务器
2. `SPC m d r` - 连接远程调试
3. 使用标准调试命令进行调试

### 测试调试流程
1. `SPC m d t` - 调试测试函数
2. 在测试代码中设置断点
3. 使用调试命令检查测试状态

## 配置说明

- 禁用了 `company-go`，完全依赖 lsp-bridge 提供补全
- 自动设置 GOPATH、GOROOT、GOPROXY 等环境变量
- 使用 `goimports` 而非 `gofmt` 进行代码格式化
- 设置了适合 Go 的缩进（tab-width: 4, indent-tabs-mode: t）
- 集成 Go Guru 进行代码分析和导航
- 支持构建标签和条件编译
- 集成 Delve 调试器，支持断点调试、远程调试等