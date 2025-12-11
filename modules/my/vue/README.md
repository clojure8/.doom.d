# Vue.js 模块

这是一个为 Doom Emacs 定制的 Vue.js 开发模块，专门配合 lsp-bridge 使用，提供完整的 Vue 2/3 开发体验。

## 功能特性

### 核心功能
- **Vue 单文件组件支持** - 完整的 .vue 文件语法高亮和编辑
- **LSP 支持** - 使用 lsp-bridge 配合 Volar/Vetur，提供智能补全和错误检查
- **TypeScript 集成** - 完整的 TypeScript 支持
- **多框架支持** - Vue 2/3、Nuxt.js、Quasar 等

### 项目管理
- **项目脚手架** - 快速创建各种 Vue 项目
- **开发服务器** - 启动、构建、预览项目
- **包管理** - npm 依赖管理和安全审计
- **代码生成** - 自动生成组件、页面、Store

### 开发工具
- **代码格式化** - Prettier 自动格式化
- **代码检查** - ESLint 集成
- **测试支持** - 单元测试和端到端测试
- **Emmet 支持** - 快速 HTML/CSS 编写

## 完整键绑定

### Transient 菜单 (`SPC m m`)
使用 `SPC m m` 打开交互式 transient 菜单，提供所有 Vue 开发功能的可视化界面。

### 项目管理 (`SPC m p`)
- `p 2` - 创建 Vue 2 项目
- `p 3` - 创建 Vue 3 项目
- `p n` - 创建 Nuxt.js 项目
- `p v` - 创建 Vite + Vue 项目
- `p q` - 创建 Quasar 项目

### 开发服务器 (`SPC m s`)
- `s s` - 启动开发服务器
- `s b` - 构建项目
- `s p` - 构建并预览

### 代码质量 (`SPC m l`)
- `l l` - 运行 ESLint 检查
- `l f` - 自动修复 ESLint 问题
- `l F` - 格式化当前文件
- `l P` - 格式化整个项目

### 测试相关 (`SPC m t`)
- `t u` - 运行单元测试
- `t e` - 运行端到端测试
- `t w` - 监视模式运行测试

### 包管理 (`SPC m n`)
- `n i` - 安装依赖
- `n a` - 添加依赖
- `n d` - 添加开发依赖
- `n r` - 移除依赖
- `n u` - 更新依赖
- `n A` - 安全审计
- `n f` - 修复安全问题

### 代码生成 (`SPC m g`)
- `g c` - 创建 Vue 组件
- `g p` - 创建页面
- `g s` - 创建 Vuex/Pinia Store

### 工具管理 (`SPC m x`)
- `x i` - 安装开发工具
- `x c` - 安装 Vue CLI
- `x v` - 安装 Vite
- `x n` - 安装 Nuxt CLI

## 依赖要求

### 必需工具
```bash
# Node.js 和 npm
node --version
npm --version

# Vue CLI (可选)
npm install -g @vue/cli

# Vite (推荐)
npm install -g create-vite
```

### LSP 服务器
```bash
# Volar (Vue 3 推荐)
npm install -g @vue/language-server

# 或者 Vetur (Vue 2)
npm install -g vls
```

### 开发工具（通过 `SPC m x i` 自动安装）
```bash
npm install -g @vue/cli vite nuxi prettier eslint @typescript-eslint/parser
```

## 项目结构模板

### Vue 3 + Vite 项目结构
```
vue3-project/
├── public/
│   └── index.html
├── src/
│   ├── components/
│   ├── views/
│   ├── router/
│   ├── store/
│   ├── assets/
│   ├── App.vue
│   └── main.ts
├── package.json
├── vite.config.ts
└── tsconfig.json
```

### Nuxt.js 项目结构
```
nuxt-project/
├── components/
├── pages/
├── layouts/
├── middleware/
├── plugins/
├── store/
├── static/
├── assets/
├── nuxt.config.js
└── package.json
```

## 开发工作流

### 基本开发流程
1. `SPC m p 3` - 创建 Vue 3 项目
2. `SPC m n i` - 安装依赖
3. `SPC m s s` - 启动开发服务器
4. `SPC m g c` - 创建组件
5. `SPC m l F` - 格式化代码
6. `SPC m s b` - 构建项目

### Vue 2 项目流程
1. `SPC m p 2` - 创建 Vue 2 项目
2. 选择所需功能（Router、Vuex、TypeScript 等）
3. `SPC m s s` - 启动开发服务器

### Nuxt.js 开发流程
1. `SPC m p n` - 创建 Nuxt 项目
2. `SPC m g p` - 创建页面（自动路由）
3. `SPC m g c` - 创建组件
4. `SPC m s s` - 启动开发服务器

### 组件开发流程
1. `SPC m g c` - 创建新组件
2. 选择 TypeScript 和 Composition API 选项
3. 编写组件逻辑和样式
4. `SPC m l F` - 格式化代码

## 代码生成功能

### 组件生成
- 支持 Options API 和 Composition API
- 可选 TypeScript 支持
- 自动生成基础模板和样式

### 页面生成
- 自动创建页面组件
- 包含基础路由结构
- 支持 TypeScript

### Store 生成
- 支持 Vuex 和 Pinia
- TypeScript 类型定义
- 标准模块结构

## 配置说明

- 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
- 自动设置 NODE_ENV 等环境变量
- 保存时自动格式化代码（如果安装了 Prettier）
- 自动添加 node_modules 到执行路径
- 集成 Emmet 快速编写 HTML/CSS
- 支持 Vue 单文件组件的语法高亮

## 特色功能

### 智能项目检测
- 自动检测项目类型（Vue CLI、Vite、Nuxt）
- 智能选择合适的开发命令

### 多框架支持
- Vue 2/3 完整支持
- Nuxt.js 全栈框架
- Quasar 跨平台开发
- Vite 现代构建工具

### TypeScript 集成
- 完整的 TypeScript 支持
- 类型检查和智能补全
- 组件和 Store 类型定义

### 现代开发体验
- 热重载开发服务器
- 快速构建和预览
- 自动代码格式化
- ESLint 代码检查

## 最佳实践

### Vue 3 + Composition API
- 使用 `<script setup>` 语法
- 利用 TypeScript 类型推导
- 组合式函数复用逻辑

### 项目结构
- 组件按功能模块组织
- 使用 Pinia 替代 Vuex
- 统一的代码风格配置

### 性能优化
- 使用 Vite 快速构建
- 组件懒加载
- 合理的代码分割