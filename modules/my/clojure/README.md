# Clojure 模块

这是一个为 Doom Emacs 定制的 Clojure 开发模块，专门配合 lsp-bridge 使用，提供完整的 Clojure/ClojureScript 开发体验。

## 功能特性

### 核心功能
- **Clojure 语法支持** - 完整的 Clojure/ClojureScript/EDN 语法高亮
- **CIDER 集成** - 强大的 REPL 驱动开发环境
- **LSP 支持** - 使用 lsp-bridge 配合 clojure-lsp，提供智能补全和错误检查
- **结构化编辑** - Paredit 和 Smartparens 支持

### 项目管理
- **多种项目类型** - Leiningen、deps.edn、Shadow CLJS、Babashka
- **项目脚手架** - 快速创建各种 Clojure 项目
- **构建工具集成** - 完整的构建和测试支持

### REPL 驱动开发
- **交互式开发** - 实时代码求值和反馈
- **多 REPL 支持** - Clojure 和 ClojureScript REPL
- **调试工具** - 检查、宏展开、文档查看

### 代码质量
- **自动格式化** - 代码自动缩进和格式化
- **重构工具** - clj-refactor 集成
- **静态分析** - clj-kondo 集成
- **测试支持** - 完整的测试运行和报告

## 完整键绑定

### Transient 菜单 (`SPC m m`)
使用 `SPC m m` 打开交互式 transient 菜单，提供所有 Clojure 开发功能的可视化界面。

### 项目管理 (`SPC m p`)
- `p l` - 创建 Leiningen 项目
- `p d` - 创建 deps.edn 项目
- `p s` - 创建 Shadow CLJS 项目
- `p b` - 创建 Babashka 项目

### REPL 管理 (`SPC m r`)
- `r j` - 启动 Clojure REPL
- `r J` - 启动 ClojureScript REPL
- `r c` - 连接到现有 REPL
- `r r` - 重启 REPL
- `r q` - 退出 REPL
- `r s` - 切换到 REPL 缓冲区

### 代码求值 (`SPC m e`)
- `e b` - 求值整个缓冲区
- `e f` - 求值当前函数
- `e e` - 求值上一个表达式
- `e r` - 求值选中区域
- `e n` - 求值命名空间表单

### 测试相关 (`SPC m t`)
- `t a` - 运行所有测试
- `t n` - 运行当前命名空间测试
- `t t` - 运行当前测试
- `t f` - 重新运行失败的测试
- `t r` - 显示测试报告

### 文档和检查 (`SPC m d`)
- `d d` - 查看符号文档
- `d a` - 搜索符号 (apropos)
- `d i` - 检查上次求值结果
- `d m` - 宏展开一层
- `d M` - 完全宏展开

### 重构功能 (`SPC m R`)
- `R a` - 添加缺失的库规范
- `R c` - 清理命名空间
- `R e` - 提取函数
- `R i` - 内联符号
- `R f` - 线程优先宏重构
- `R l` - 线程后置宏重构

### 构建相关 (`SPC m b`)
- `b t` - Leiningen 测试
- `b u` - 构建 uberjar
- `b r` - 运行项目
- `b T` - deps.edn 测试
- `b U` - deps.edn uberjar
- `b B` - Babashka 测试
- `b R` - 运行 Babashka 脚本

### 格式化 (`SPC m f`)
- `f b` - 格式化缓冲区
- `f r` - 格式化选中区域

### 工具管理 (`SPC m x`)
- `x i` - 安装开发工具

## 依赖要求

### 必需工具
```bash
# Java (JDK 8+)
java -version

# Clojure CLI
curl -O https://download.clojure.org/install/linux-install-1.11.1.1413.sh
chmod +x linux-install-1.11.1.1413.sh
sudo ./linux-install-1.11.1.1413.sh

# Leiningen
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
chmod +x ~/bin/lein
```

### LSP 服务器
```bash
# clojure-lsp
curl -L https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-linux-amd64.zip -o clojure-lsp.zip
unzip clojure-lsp.zip
sudo mv clojure-lsp /usr/local/bin/
```

### 可选工具
```bash
# Babashka (快速 Clojure 脚本)
curl -sLO https://raw.githubusercontent.com/babashka/babashka/master/install
chmod +x install
./install

# clj-kondo (静态分析)
curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
chmod +x install-clj-kondo
./install-clj-kondo
```

## 项目结构模板

### Leiningen 项目结构
```
lein-project/
├── project.clj
├── src/
│   └── project_name/
│       └── core.clj
├── test/
│   └── project_name/
│       └── core_test.clj
├── resources/
└── target/
```

### deps.edn 项目结构
```
deps-project/
├── deps.edn
├── src/
│   └── core.clj
├── test/
│   └── core_test.clj
└── resources/
```

### Babashka 项目结构
```
bb-project/
├── bb.edn
├── src/
│   └── main.clj
└── test/
```

## 开发工作流

### REPL 驱动开发流程
1. `SPC m p l` - 创建 Leiningen 项目
2. `SPC m r j` - 启动 REPL
3. `SPC m e f` - 求值函数进行测试
4. `SPC m t n` - 运行测试
5. `SPC m R c` - 清理命名空间

### ClojureScript 开发流程
1. `SPC m p s` - 创建 Shadow CLJS 项目
2. `SPC m r J` - 启动 ClojureScript REPL
3. 在浏览器中开发和测试

### 快速脚本开发
1. `SPC m p b` - 创建 Babashka 项目
2. `SPC m b R` - 直接运行脚本
3. 快速原型和工具开发

### 测试驱动开发
1. `SPC m t t` - 运行当前测试
2. 编写实现代码
3. `SPC m e f` - 求值函数
4. `SPC m t n` - 运行命名空间测试

## 配置说明

- 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
- 自动设置 LEIN_HOME、CLOJURE_HOME、JAVA_HOME 环境变量
- 启用结构化编辑 (Paredit)
- 启用自动缩进 (Aggressive Indent)
- 启用彩虹括号显示
- 集成 CIDER 进行 REPL 驱动开发
- 集成 clj-refactor 进行代码重构
- 集成 clj-kondo 进行静态分析

## 特色功能

### REPL 驱动开发
- **实时反馈** - 代码修改立即在 REPL 中测试
- **交互式调试** - 检查变量和函数状态
- **热重载** - 无需重启应用即可更新代码

### 结构化编辑
- **Paredit 模式** - 智能括号编辑
- **自动缩进** - 代码自动格式化
- **彩虹括号** - 视觉上区分嵌套层级

### 强大的重构
- **自动导入** - 智能添加 require/import
- **命名空间清理** - 移除未使用的导入
- **函数提取** - 重构代码结构
- **线程宏转换** - 改善代码可读性

### 多项目支持
- **Leiningen** - 传统 Clojure 项目管理
- **deps.edn** - 现代 Clojure 依赖管理
- **Shadow CLJS** - ClojureScript 开发
- **Babashka** - 快速脚本和工具开发

## 最佳实践

### REPL 工作流
- 保持 REPL 运行，实时测试代码
- 使用命名空间重载而非重启 REPL
- 利用 REPL 历史进行快速迭代

### 代码组织
- 遵循 Clojure 命名约定
- 使用有意义的命名空间结构
- 保持函数简洁和纯净

### 测试策略
- 编写单元测试覆盖核心逻辑
- 使用 REPL 进行探索性测试
- 利用 test.check 进行属性测试