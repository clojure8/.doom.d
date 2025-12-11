# Python 模块

这是一个为 Doom Emacs 定制的 Python 开发模块，专门配合 lsp-bridge 使用，提供完整的 Python 开发生态支持。

## 功能特性

### 核心功能
- **Python 语法高亮和编辑支持** - 通过 `python-mode` 提供
- **LSP 支持** - 使用 lsp-bridge 配合 pylsp/pyright，提供代码补全、跳转、重构等功能
- **虚拟环境管理** - 完整的 venv/virtualenv 支持
- **包管理** - pip、Poetry、Pipenv 全面支持

### 环境管理
- **虚拟环境** - 创建、激活、停用、列出虚拟环境
- **自动检测** - 自动激活项目虚拟环境
- **多环境支持** - 支持 venv、virtualenv、conda 等

### 包管理工具
- **pip 管理** - 安装、卸载、升级、列表、requirements.txt
- **Poetry 支持** - 现代 Python 包管理和依赖解析
- **Pipenv 支持** - Python 开发工作流工具
- **uv 支持** - 极速 Python 包管理器和项目管理工具

### 代码质量
- **代码格式化** - Black、autopep8 自动格式化
- **导入排序** - isort 自动排序导入语句
- **代码检查** - flake8、pylint、mypy 静态分析
- **安全检查** - bandit 安全漏洞扫描

### 测试框架
- **pytest** - 现代 Python 测试框架
- **unittest** - Python 标准测试库
- **doctest** - 文档测试
- **覆盖率** - coverage.py 测试覆盖率分析

### 项目模板
- **Python 包** - 标准 Python 包结构
- **Flask 应用** - Web 应用快速启动
- **Django 项目** - 企业级 Web 框架

### 数据科学
- **Jupyter 集成** - Notebook 和 Lab 支持
- **EIN 支持** - 在 Emacs 中直接使用 Jupyter Notebook
- **emacs-jupyter** - 现代 Jupyter 客户端，支持 REPL 和 Org 模式
- **科学计算包** - NumPy、Pandas、Matplotlib 等
- **实时编程** - live-py-mode 实时代码执行
- **Notebook 转换** - 支持多种格式转换

## 完整键绑定

### Transient 菜单 (`SPC m m`)
使用 `SPC m m` 打开交互式 transient 菜单，提供所有 Python 开发功能的可视化界面。菜单包含：
- 环境管理（激活、创建、列出虚拟环境）
- 包管理（pip、uv、Poetry 包管理）
- 代码质量（格式化、检查、类型检查）
- 测试功能（pytest、unittest、覆盖率）
- 运行调试（运行文件、调试、性能分析）
- 项目模板（Python 包、Flask、Django、uv 项目）
- Jupyter 支持（Notebook、Lab、REPL、数据科学包）
- EIN Notebook（在 Emacs 中直接使用 Jupyter）
- Notebook 转换（多种格式转换）
- 工具管理（开发工具、文档生成）

### 环境管理 (`SPC m e`)
- `e a` - 激活虚拟环境
- `e d` - 停用虚拟环境
- `e c` - 创建虚拟环境
- `e l` - 列出虚拟环境

### 包管理 (`SPC m p`)
- `p i` - 安装包
- `p u` - 卸载包
- `p U` - 升级包
- `p l` - 列出已安装包
- `p f` - 生成 requirements.txt
- `p r` - 从 requirements.txt 安装

### Poetry 管理 (`SPC m P`)
- `P i` - 初始化 Poetry 项目
- `P I` - 安装 Poetry 依赖
- `P a` - 添加依赖
- `P r` - 移除依赖
- `P s` - 进入 Poetry shell
- `P R` - 运行 Poetry 命令

### uv 包管理 (`SPC m u`)
- `u i` - 初始化 uv 项目
- `u a` - 添加依赖
- `u A` - 添加开发依赖
- `u r` - 移除依赖
- `u s` - 同步依赖
- `u l` - 锁定依赖版本
- `u u` - 更新依赖
- `u t` - 显示依赖树
- `u R` - 运行命令
- `u p` - 运行 Python 文件
- `u S` - 运行脚本
- `u b` - 构建项目
- `u P` - 发布到 PyPI

### uv Python 版本管理 (`SPC m U`)
- `U i` - 安装 Python 版本
- `U l` - 列出可用 Python 版本
- `U v` - 创建虚拟环境

### uv pip 兼容 (`SPC m v`)
- `v i` - 安装包 (uv pip)
- `v l` - 列出包 (uv pip)
- `v f` - 生成 requirements.txt

### uv 工具管理 (`SPC m T`)
- `T i` - 安装全局工具
- `T l` - 列出工具
- `T r` - 运行工具

### uv 系统管理 (`SPC m S`)
- `S c` - 清理缓存
- `S u` - 更新 uv 自身

### 代码质量 (`SPC m c`)
- `c f` - 格式化代码
- `c s` - 排序导入
- `c l` - flake8 检查
- `c L` - pylint 检查
- `c t` - mypy 类型检查
- `c b` - bandit 安全检查

### 测试相关 (`SPC m t`)
- `t t` - 运行 pytest
- `t f` - 测试当前文件
- `t F` - 测试当前函数
- `t u` - 运行 unittest
- `t d` - 运行 doctest
- `t c` - 覆盖率测试
- `t h` - 生成 HTML 覆盖率报告

### 运行和调试 (`SPC m r`)
- `r r` - 运行当前文件
- `r a` - 带参数运行
- `r d` - pdb 调试
- `r p` - 性能分析

### 文档生成 (`SPC m d`)
- `d d` - 生成文档字符串
- `d s` - Sphinx 快速开始
- `d b` - 构建 Sphinx 文档

### 项目模板 (`SPC m n`)
- `n p` - 创建 Python 包
- `n f` - 创建 Flask 应用
- `n d` - 创建 Django 项目
- `n u` - 创建 uv 项目
- `n F` - 创建 uv Flask 应用
- `n A` - 创建 uv FastAPI 应用

### Jupyter 支持 (`SPC m j`)
- `j n` - 启动 Jupyter Notebook
- `j l` - 启动 Jupyter Lab
- `j r` - 启动 Jupyter REPL
- `j c` - 连接到现有内核
- `j e` - 求值当前行或区域
- `j b` - 求值整个缓冲区
- `j f` - 求值当前函数
- `j ?` - 检查光标处对象
- `j z` - 跳转到 REPL 缓冲区
- `j R` - 重启 REPL 内核
- `j o` - 启用 Org 模式 Jupyter
- `j i` - 安装数据科学包

### EIN Notebook (`SPC m E`)
- `E c` - 连接到 Notebook 服务器
- `E n` - 创建新 Notebook
- `E o` - 打开现有 Notebook
- `E s` - 保存 Notebook
- `E r` - 重命名 Notebook
- `E k` - 终止内核
- `E R` - 重启内核

### Notebook 转换 (`SPC m C`)
- `C p` - Notebook 转 Python 文件
- `C n` - Python 文件转 Notebook
- `C h` - Notebook 转 HTML
- `C P` - Notebook 转 PDF

### 工具管理 (`SPC m x`)
- `x i` - 安装开发工具
- `x l` - 启用 live-py-mode

## 依赖要求

### 必需工具
```bash
# Python 3.6+
python3 --version

# pip (通常随 Python 安装)
pip --version
```

### LSP 服务器（选择一个）
```bash
# Python LSP Server (推荐)
pip install python-lsp-server[all]

# 或者 Pyright
npm install -g pyright
```

### 开发工具（通过 `SPC m x i` 自动安装）
```bash
pip install black flake8 pylint mypy pytest coverage bandit isort autopep8 sphinx twine
```

### 可选工具
```bash
# uv (极速包管理器，强烈推荐)
curl -LsSf https://astral.sh/uv/install.sh | sh

# Poetry (现代包管理)
curl -sSL https://install.python-poetry.org | python3 -

# Pipenv
pip install pipenv

# Jupyter 和数据科学包
pip install jupyter jupyterlab ipython
pip install numpy pandas matplotlib seaborn scikit-learn plotly bokeh scipy

# Notebook 转换工具
pip install jupytext nbconvert
```

## 项目结构模板

### Python 包结构
```
package-name/
├── package_name/
│   └── __init__.py
├── tests/
│   └── __init__.py
├── setup.py
├── requirements.txt
└── README.md
```

### Flask 应用结构
```
flask-app/
├── app.py
├── templates/
│   └── index.html
├── static/
├── requirements.txt
└── README.md
```

### Django 项目结构
```
django-project/
├── manage.py
├── project_name/
│   ├── __init__.py
│   ├── settings.py
│   ├── urls.py
│   └── wsgi.py
└── app_name/
    ├── __init__.py
    ├── models.py
    ├── views.py
    └── urls.py
```

### uv 项目结构
```
uv-project/
├── pyproject.toml
├── uv.lock
├── README.md
├── src/
│   └── project_name/
│       ├── __init__.py
│       └── main.py
└── tests/
    └── __init__.py
```

## 开发工作流

### 基本开发流程
1. `SPC m e c` - 创建虚拟环境
2. `SPC m e a` - 激活虚拟环境
3. `SPC m p i` - 安装依赖
4. `SPC m r r` - 运行代码
5. `SPC m t t` - 运行测试
6. `SPC m c f` - 格式化代码

### Poetry 工作流
1. `SPC m P i` - 初始化项目
2. `SPC m P a` - 添加依赖
3. `SPC m P I` - 安装依赖
4. `SPC m P R` - 运行命令

### uv 工作流（推荐）
1. `SPC m u i` - 初始化项目
2. `SPC m u a` - 添加依赖
3. `SPC m u s` - 同步依赖
4. `SPC m u p` - 运行 Python 文件
5. `SPC m u b` - 构建项目

### 数据科学工作流
1. `SPC m j i` - 安装科学计算包
2. `SPC m j n` - 启动 Jupyter Notebook
3. `SPC m E c` - 连接到 Notebook 服务器
4. `SPC m E n` - 创建新 Notebook
5. 在 EIN 中进行数据分析

### Jupyter REPL 工作流
1. `SPC m j r` - 启动 Jupyter REPL
2. `SPC m j e` - 求值代码
3. `SPC m j ?` - 检查对象
4. `SPC m j z` - 跳转到 REPL

### Notebook 转换工作流
1. `SPC m C p` - 将 .ipynb 转为 .py
2. `SPC m C n` - 将 .py 转为 .ipynb
3. `SPC m C h` - 导出为 HTML
4. `SPC m C P` - 导出为 PDF

### 代码质量检查流程
1. `SPC m c f` - 格式化代码
2. `SPC m c s` - 排序导入
3. `SPC m c l` - 代码风格检查
4. `SPC m c t` - 类型检查
5. `SPC m c b` - 安全检查

## EIN Notebook 键绑定

在 EIN Notebook 模式中，使用以下键绑定：

### Cell 操作 (`SPC m c`)
- `c r` - 运行当前 cell
- `c R` - 运行所有 cells
- `c a` - 在上方插入 cell
- `c b` - 在下方插入 cell
- `c d` - 删除当前 cell
- `c t` - 更改 cell 类型
- `c o` - 切换输出显示
- `c c` - 清除输出

### Kernel 操作 (`SPC m k`)
- `k i` - 终止内核
- `k r` - 重启内核

### Notebook 操作 (`SPC m n`)
- `n s` - 保存 Notebook
- `n r` - 重命名 Notebook

## 配置说明

- 禁用了 `anaconda-mode` 和 `company-anaconda`，完全依赖 lsp-bridge
- 自动设置 PYTHONPATH、VIRTUAL_ENV 等环境变量
- 保存时自动格式化代码（如果安装了 black）
- 保存时自动排序导入（如果安装了 isort）
- 自动检测并激活项目虚拟环境
- 支持 IPython 作为 Python shell
- 集成多种测试框架和覆盖率工具
- EIN 支持内联图像显示和自动补全
- Jupyter REPL 支持实时求值和对象检查
- 支持 Org 模式中的 Jupyter 代码块

## 特色功能

### 智能环境管理
- 自动检测项目虚拟环境
- 支持多种虚拟环境工具
- 环境状态显示

### 全面的包管理
- 传统 pip 工作流
- 现代 Poetry 管理
- Pipenv 支持
- 极速 uv 包管理器

### 代码质量保证
- 多种格式化工具
- 静态分析集成
- 安全漏洞检测

### 测试驱动开发
- 多测试框架支持
- 覆盖率分析
- 函数级测试运行

### 数据科学友好
- **EIN 集成** - 在 Emacs 中直接编辑和运行 Jupyter Notebook
- **Jupyter REPL** - 现代 Jupyter 客户端，支持实时求值
- **Org 模式集成** - 在 Org 文档中嵌入 Jupyter 代码块
- **Notebook 转换** - 支持 .ipynb、.py、HTML、PDF 格式转换
- **科学计算包管理** - 一键安装数据科学工具链
- **内联图像显示** - 在 Emacs 中直接显示 matplotlib 图表

### 项目脚手架
- 多种项目模板
- 标准项目结构
- 最佳实践集成