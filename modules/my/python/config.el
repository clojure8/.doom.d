;;; my/python/config.el -*- lexical-binding: t; -*-

;; Python 环境管理函数
(defvar +python/current-venv nil
  "当前激活的虚拟环境")

(defun +python/activate-venv ()
  "激活虚拟环境"
  (interactive)
  (let ((venv-path (read-directory-name "虚拟环境路径: " "~/.virtualenvs/")))
    (pyvenv-activate venv-path)
    (setq +python/current-venv venv-path)
    (message "已激活虚拟环境: %s" venv-path)))

(defun +python/deactivate-venv ()
  "停用虚拟环境"
  (interactive)
  (pyvenv-deactivate)
  (setq +python/current-venv nil)
  (message "已停用虚拟环境"))

(defun +python/create-venv ()
  "创建新的虚拟环境"
  (interactive)
  (let ((venv-name (read-string "虚拟环境名称: "))
        (python-version (completing-read "Python 版本: " '("python3" "python3.8" "python3.9" "python3.10" "python3.11" "python3.12"))))
    (compile (format "%s -m venv ~/.virtualenvs/%s" python-version venv-name))))

(defun +python/list-venvs ()
  "列出所有虚拟环境"
  (interactive)
  (let ((venvs-dir "~/.virtualenvs/"))
    (when (file-directory-p venvs-dir)
      (message "虚拟环境列表: %s" 
               (mapconcat 'identity 
                         (directory-files venvs-dir nil "^[^.]") 
                         ", ")))))

;; 包管理函数
(defun +python/pip-install ()
  "安装 Python 包"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "pip install %s" package))))

(defun +python/pip-uninstall ()
  "卸载 Python 包"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "pip uninstall %s" package))))

(defun +python/pip-upgrade ()
  "升级 Python 包"
  (interactive)
  (let ((package (read-string "包名 (留空升级所有): ")))
    (if (string-empty-p package)
        (compile "pip list --outdated --format=freeze | grep -v '^\\-e' | cut -d = -f 1 | xargs -n1 pip install -U")
      (compile (format "pip install -U %s" package)))))

(defun +python/pip-list ()
  "列出已安装的包"
  (interactive)
  (compile "pip list"))

(defun +python/pip-freeze ()
  "生成 requirements.txt"
  (interactive)
  (compile "pip freeze > requirements.txt"))

(defun +python/pip-install-requirements ()
  "从 requirements.txt 安装依赖"
  (interactive)
  (compile "pip install -r requirements.txt"))

;; uv 包管理函数
(defun +python/uv-init ()
  "初始化 uv 项目"
  (interactive)
  (let ((project-name (read-string "项目名: ")))
    (compile (format "uv init %s" project-name))))

(defun +python/uv-add ()
  "添加依赖包"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "uv add %s" package))))

(defun +python/uv-add-dev ()
  "添加开发依赖"
  (interactive)
  (let ((package (read-string "开发依赖包名: ")))
    (compile (format "uv add --dev %s" package))))

(defun +python/uv-remove ()
  "移除依赖包"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "uv remove %s" package))))

(defun +python/uv-sync ()
  "同步依赖"
  (interactive)
  (compile "uv sync"))

(defun +python/uv-lock ()
  "锁定依赖版本"
  (interactive)
  (compile "uv lock"))

(defun +python/uv-run ()
  "使用 uv 运行命令"
  (interactive)
  (let ((command (read-string "命令: ")))
    (compile (format "uv run %s" command))))

(defun +python/uv-run-python ()
  "使用 uv 运行 Python 文件"
  (interactive)
  (compile (format "uv run python %s" (buffer-file-name))))

(defun +python/uv-run-script ()
  "运行 pyproject.toml 中定义的脚本"
  (interactive)
  (let ((script (read-string "脚本名: ")))
    (compile (format "uv run %s" script))))

(defun +python/uv-install ()
  "安装项目依赖"
  (interactive)
  (compile "uv sync"))

(defun +python/uv-update ()
  "更新依赖"
  (interactive)
  (compile "uv lock --upgrade"))

(defun +python/uv-tree ()
  "显示依赖树"
  (interactive)
  (compile "uv tree"))

(defun +python/uv-python-install ()
  "安装 Python 版本"
  (interactive)
  (let ((version (read-string "Python 版本 (如 3.12): ")))
    (compile (format "uv python install %s" version))))

(defun +python/uv-python-list ()
  "列出可用的 Python 版本"
  (interactive)
  (compile "uv python list"))

(defun +python/uv-venv ()
  "创建虚拟环境"
  (interactive)
  (let ((python-version (read-string "Python 版本 (可选): ")))
    (if (string-empty-p python-version)
        (compile "uv venv")
      (compile (format "uv venv --python %s" python-version)))))

(defun +python/uv-pip-install ()
  "使用 uv pip 安装包"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "uv pip install %s" package))))

(defun +python/uv-pip-list ()
  "列出已安装的包"
  (interactive)
  (compile "uv pip list"))

(defun +python/uv-pip-freeze ()
  "生成 requirements.txt"
  (interactive)
  (compile "uv pip freeze > requirements.txt"))

(defun +python/uv-build ()
  "构建项目"
  (interactive)
  (compile "uv build"))

(defun +python/uv-publish ()
  "发布项目到 PyPI"
  (interactive)
  (compile "uv publish"))

(defun +python/uv-tool-install ()
  "安装全局工具"
  (interactive)
  (let ((tool (read-string "工具名: ")))
    (compile (format "uv tool install %s" tool))))

(defun +python/uv-tool-list ()
  "列出已安装的工具"
  (interactive)
  (compile "uv tool list"))

(defun +python/uv-tool-run ()
  "运行工具"
  (interactive)
  (let ((tool (read-string "工具名: "))
        (args (read-string "参数 (可选): ")))
    (compile (format "uv tool run %s %s" tool args))))

(defun +python/uv-cache-clean ()
  "清理缓存"
  (interactive)
  (compile "uv cache clean"))

(defun +python/uv-self-update ()
  "更新 uv 自身"
  (interactive)
  (compile "uv self update"))

;; Poetry 支持
(defun +python/poetry-init ()
  "初始化 Poetry 项目"
  (interactive)
  (compile "poetry init"))

(defun +python/poetry-install ()
  "安装 Poetry 依赖"
  (interactive)
  (compile "poetry install"))

(defun +python/poetry-add ()
  "添加 Poetry 依赖"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "poetry add %s" package))))

(defun +python/poetry-remove ()
  "移除 Poetry 依赖"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "poetry remove %s" package))))

(defun +python/poetry-shell ()
  "进入 Poetry shell"
  (interactive)
  (compile "poetry shell"))

(defun +python/poetry-run ()
  "使用 Poetry 运行命令"
  (interactive)
  (let ((command (read-string "命令: ")))
    (compile (format "poetry run %s" command))))

;; 代码质量工具
(defun +python/format-buffer ()
  "格式化当前缓冲区"
  (interactive)
  (cond
   ((executable-find "black") (blacken-buffer))
   ((executable-find "autopep8") (py-autopep8-buffer))
   (t (message "未找到代码格式化工具"))))

(defun +python/sort-imports ()
  "排序导入语句"
  (interactive)
  (py-isort-buffer))

(defun +python/lint-flake8 ()
  "使用 flake8 检查代码"
  (interactive)
  (compile (format "flake8 %s" (buffer-file-name))))

(defun +python/lint-pylint ()
  "使用 pylint 检查代码"
  (interactive)
  (compile (format "pylint %s" (buffer-file-name))))

(defun +python/lint-mypy ()
  "使用 mypy 进行类型检查"
  (interactive)
  (compile (format "mypy %s" (buffer-file-name))))

(defun +python/bandit-security ()
  "使用 bandit 进行安全检查"
  (interactive)
  (compile (format "bandit %s" (buffer-file-name))))

;; 测试相关
(defun +python/run-pytest ()
  "运行 pytest"
  (interactive)
  (compile "pytest"))

(defun +python/run-pytest-file ()
  "运行当前文件的测试"
  (interactive)
  (compile (format "pytest %s" (buffer-file-name))))

(defun +python/run-pytest-function ()
  "运行当前函数的测试"
  (interactive)
  (let ((function-name (python-info-current-defun)))
    (if function-name
        (compile (format "pytest %s::%s" (buffer-file-name) function-name))
      (message "未找到当前函数"))))

(defun +python/run-unittest ()
  "运行 unittest"
  (interactive)
  (compile "python -m unittest discover"))

(defun +python/run-doctest ()
  "运行 doctest"
  (interactive)
  (compile (format "python -m doctest %s" (buffer-file-name))))

(defun +python/coverage-run ()
  "运行覆盖率测试"
  (interactive)
  (compile "coverage run -m pytest && coverage report"))

(defun +python/coverage-html ()
  "生成 HTML 覆盖率报告"
  (interactive)
  (compile "coverage html"))

;; 运行和调试
(defun +python/run-file ()
  "运行当前 Python 文件"
  (interactive)
  (compile (format "python %s" (buffer-file-name))))

(defun +python/run-file-with-args ()
  "带参数运行当前文件"
  (interactive)
  (let ((args (read-string "参数: ")))
    (compile (format "python %s %s" (buffer-file-name) args))))

(defun +python/debug-file ()
  "使用 pdb 调试当前文件"
  (interactive)
  (compile (format "python -m pdb %s" (buffer-file-name))))

(defun +python/profile-file ()
  "性能分析当前文件"
  (interactive)
  (compile (format "python -m cProfile %s" (buffer-file-name))))

;; 文档生成
(defun +python/generate-docstring ()
  "生成文档字符串"
  (interactive)
  (python-docstring-fill))

(defun +python/sphinx-quickstart ()
  "初始化 Sphinx 文档"
  (interactive)
  (compile "sphinx-quickstart docs"))

(defun +python/sphinx-build ()
  "构建 Sphinx 文档"
  (interactive)
  (compile "sphinx-build -b html docs docs/_build/html"))

;; 项目管理
(defun +python/create-package ()
  "创建 Python 包结构"
  (interactive)
  (let ((package-name (read-string "包名: ")))
    (make-directory package-name t)
    (with-temp-file (format "%s/__init__.py" package-name)
      (insert (format "\"\"\"
%s package.
\"\"\"

__version__ = \"0.1.0\"
" package-name)))
    (with-temp-file (format "%s/setup.py" package-name)
      (insert (format "from setuptools import setup, find_packages

setup(
    name=\"%s\",
    version=\"0.1.0\",
    packages=find_packages(),
    install_requires=[],
    author=\"Your Name\",
    author_email=\"your.email@example.com\",
    description=\"A short description\",
    long_description=open(\"README.md\").read(),
    long_description_content_type=\"text/markdown\",
    url=\"https://github.com/yourusername/%s\",
    classifiers=[
        \"Programming Language :: Python :: 3\",
        \"License :: OSI Approved :: MIT License\",
        \"Operating System :: OS Independent\",
    ],
    python_requires=\">=3.6\",
)
" package-name package-name)))
    (with-temp-file (format "%s/README.md" package-name)
      (insert (format "# %s

## 描述

## 安装

```bash
pip install %s
```

## 使用

```python
import %s
```
" package-name package-name package-name)))
    (with-temp-file (format "%s/requirements.txt" package-name)
      (insert ""))
    (make-directory (format "%s/tests" package-name) t)
    (with-temp-file (format "%s/tests/__init__.py" package-name)
      (insert ""))
    (message "已创建包结构: %s" package-name)))

(defun +python/create-flask-app ()
  "创建 Flask 应用结构"
  (interactive)
  (let ((app-name (read-string "应用名: ")))
    (make-directory app-name t)
    (let ((default-directory (format "%s/" app-name)))
      (with-temp-file "app.py"
        (insert "from flask import Flask, render_template, request, jsonify

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/hello')
def hello():
    return jsonify({'message': 'Hello, World!'})

if __name__ == '__main__':
    app.run(debug=True)
"))
      (make-directory "templates" t)
      (with-temp-file "templates/index.html"
        (insert "<!DOCTYPE html>
<html>
<head>
    <title>Flask App</title>
</head>
<body>
    <h1>Hello, Flask!</h1>
</body>
</html>
"))
      (make-directory "static" t)
      (with-temp-file "requirements.txt"
        (insert "Flask==2.3.3
"))
      (message "已创建 Flask 应用: %s" app-name))))

(defun +python/create-django-project ()
  "创建 Django 项目"
  (interactive)
  (let ((project-name (read-string "项目名: ")))
    (compile (format "django-admin startproject %s" project-name))))

(defun +python/create-uv-project ()
  "创建 uv 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-type (completing-read "项目类型: " '("app" "lib"))))
    (if (string= project-type "lib")
        (compile (format "uv init --lib %s" project-name))
      (compile (format "uv init %s" project-name)))))

(defun +python/create-uv-flask-app ()
  "创建 uv Flask 应用"
  (interactive)
  (let ((app-name (read-string "应用名: ")))
    (compile (format "uv init %s" app-name))
    (let ((default-directory (format "%s/" app-name)))
      (compile "uv add flask")
      (with-temp-file "app.py"
        (insert "from flask import Flask, render_template, request, jsonify

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/api/hello')
def hello():
    return jsonify({'message': 'Hello, World!'})

if __name__ == '__main__':
    app.run(debug=True)
"))
      (make-directory "templates" t)
      (with-temp-file "templates/index.html"
        (insert "<!DOCTYPE html>
<html>
<head>
    <title>Flask App</title>
</head>
<body>
    <h1>Hello, Flask with uv!</h1>
</body>
</html>
"))
      (make-directory "static" t)
      (message "已创建 uv Flask 应用: %s" app-name))))

(defun +python/create-uv-fastapi-app ()
  "创建 uv FastAPI 应用"
  (interactive)
  (let ((app-name (read-string "应用名: ")))
    (compile (format "uv init %s" app-name))
    (let ((default-directory (format "%s/" app-name)))
      (compile "uv add fastapi uvicorn")
      (with-temp-file "main.py"
        (insert "from fastapi import FastAPI
from pydantic import BaseModel

app = FastAPI()

class Item(BaseModel):
    name: str
    description: str = None
    price: float
    tax: float = None

@app.get(\"/\")
async def read_root():
    return {\"Hello\": \"World\"}

@app.get(\"/items/{item_id}\")
async def read_item(item_id: int, q: str = None):
    return {\"item_id\": item_id, \"q\": q}

@app.post(\"/items/\")
async def create_item(item: Item):
    return item

if __name__ == \"__main__\":
    import uvicorn
    uvicorn.run(app, host=\"0.0.0.0\", port=8000)
"))
      (message "已创建 uv FastAPI 应用: %s" app-name))))

;; 数据科学工具
(defun +python/jupyter-notebook ()
  "启动 Jupyter Notebook"
  (interactive)
  (compile "jupyter notebook"))

(defun +python/jupyter-lab ()
  "启动 Jupyter Lab"
  (interactive)
  (compile "jupyter lab"))

(defun +python/install-data-science-packages ()
  "安装数据科学常用包"
  (interactive)
  (let ((packages '("numpy" "pandas" "matplotlib" "seaborn" "scikit-learn" 
                   "jupyter" "ipython" "plotly" "bokeh" "scipy")))
    (compile (format "pip install %s" (mapconcat 'identity packages " ")))))

;; EIN (Emacs IPython Notebook) 相关函数
(defun +python/ein-connect-to-notebook ()
  "连接到 Jupyter Notebook 服务器"
  (interactive)
  (call-interactively 'ein:login))

(defun +python/ein-new-notebook ()
  "创建新的 Notebook"
  (interactive)
  (call-interactively 'ein:new-notebook))

(defun +python/ein-open-notebook ()
  "打开现有的 Notebook"
  (interactive)
  (call-interactively 'ein:open))

(defun +python/ein-run-cell ()
  "运行当前 cell"
  (interactive)
  (ein:worksheet-execute-cell))

(defun +python/ein-run-all-cells ()
  "运行所有 cells"
  (interactive)
  (ein:worksheet-execute-all-cells))

(defun +python/ein-insert-cell-above ()
  "在上方插入 cell"
  (interactive)
  (ein:worksheet-insert-cell-above))

(defun +python/ein-insert-cell-below ()
  "在下方插入 cell"
  (interactive)
  (ein:worksheet-insert-cell-below))

(defun +python/ein-delete-cell ()
  "删除当前 cell"
  (interactive)
  (ein:worksheet-delete-cell))

(defun +python/ein-change-cell-type ()
  "更改 cell 类型"
  (interactive)
  (call-interactively 'ein:worksheet-change-cell-type))

(defun +python/ein-toggle-output ()
  "切换输出显示"
  (interactive)
  (ein:worksheet-toggle-output))

(defun +python/ein-clear-output ()
  "清除输出"
  (interactive)
  (ein:worksheet-clear-output))

(defun +python/ein-save-notebook ()
  "保存 Notebook"
  (interactive)
  (ein:notebook-save-notebook))

(defun +python/ein-rename-notebook ()
  "重命名 Notebook"
  (interactive)
  (call-interactively 'ein:notebook-rename))

(defun +python/ein-kill-kernel ()
  "终止内核"
  (interactive)
  (ein:notebook-kernel-interrupt))

(defun +python/ein-restart-kernel ()
  "重启内核"
  (interactive)
  (ein:notebook-restart-session-command))

;; Jupyter (emacs-jupyter) 相关函数
(defun +python/jupyter-run-repl ()
  "启动 Jupyter REPL"
  (interactive)
  (call-interactively 'jupyter-run-repl))

(defun +python/jupyter-connect-repl ()
  "连接到现有的 Jupyter 内核"
  (interactive)
  (call-interactively 'jupyter-connect-repl))

(defun +python/jupyter-eval-line-or-region ()
  "求值当前行或选中区域"
  (interactive)
  (if (use-region-p)
      (jupyter-eval-region (region-beginning) (region-end))
    (jupyter-eval-line-or-region)))

(defun +python/jupyter-eval-buffer ()
  "求值整个缓冲区"
  (interactive)
  (jupyter-eval-buffer))

(defun +python/jupyter-eval-defun ()
  "求值当前函数"
  (interactive)
  (jupyter-eval-defun))

(defun +python/jupyter-inspect-at-point ()
  "检查光标处的对象"
  (interactive)
  (jupyter-inspect-at-point))

(defun +python/jupyter-repl-pop-to-buffer ()
  "跳转到 REPL 缓冲区"
  (interactive)
  (jupyter-repl-pop-to-buffer))

(defun +python/jupyter-repl-restart-kernel ()
  "重启 REPL 内核"
  (interactive)
  (jupyter-repl-restart-kernel))

(defun +python/jupyter-org-mode ()
  "在 Org 模式中启用 Jupyter"
  (interactive)
  (require 'ob-jupyter)
  (message "Jupyter 已在 Org 模式中启用"))

;; Notebook 转换函数
(defun +python/convert-notebook-to-py ()
  "将 Notebook 转换为 Python 文件"
  (interactive)
  (let ((notebook-file (read-file-name "Notebook 文件: " nil nil t nil
                                       (lambda (name) (string-match "\\.ipynb$" name))))
        (output-file (read-file-name "输出 Python 文件: " nil nil nil nil
                                     (lambda (name) (string-match "\\.py$" name)))))
    (compile (format "jupyter nbconvert --to python %s --output %s" 
                     (shell-quote-argument notebook-file)
                     (shell-quote-argument (file-name-sans-extension output-file))))))

(defun +python/convert-py-to-notebook ()
  "将 Python 文件转换为 Notebook"
  (interactive)
  (let ((py-file (read-file-name "Python 文件: " nil nil t nil
                                 (lambda (name) (string-match "\\.py$" name))))
        (output-file (read-file-name "输出 Notebook 文件: " nil nil nil nil
                                     (lambda (name) (string-match "\\.ipynb$" name)))))
    (compile (format "jupytext --to notebook %s --output %s" 
                     (shell-quote-argument py-file)
                     (shell-quote-argument output-file)))))

(defun +python/notebook-to-html ()
  "将 Notebook 转换为 HTML"
  (interactive)
  (let ((notebook-file (read-file-name "Notebook 文件: " nil nil t nil
                                       (lambda (name) (string-match "\\.ipynb$" name)))))
    (compile (format "jupyter nbconvert --to html %s" (shell-quote-argument notebook-file)))))

(defun +python/notebook-to-pdf ()
  "将 Notebook 转换为 PDF"
  (interactive)
  (let ((notebook-file (read-file-name "Notebook 文件: " nil nil t nil
                                       (lambda (name) (string-match "\\.ipynb$" name)))))
    (compile (format "jupyter nbconvert --to pdf %s" (shell-quote-argument notebook-file)))))

;; 工具安装
(defun +python/install-dev-tools ()
  "安装开发工具"
  (interactive)
  (let ((tools '("black" "flake8" "pylint" "mypy" "pytest" "coverage" 
                "bandit" "isort" "autopep8" "sphinx" "twine")))
    (compile (format "pip install %s" (mapconcat 'identity tools " ")))))

(use-package! python-mode
  :mode "\\.py\\'"
  :config
  ;; 设置 Python 解释器
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +python-lsp-clients nil)
  
  ;; Python 模式钩子
  (add-hook 'python-mode-hook
            (lambda ()
              ;; 启用 lsp-bridge（如果全局未启用）
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              ;; 设置缩进
              (setq python-indent-offset 4)
              (setq tab-width 4)
              ;; 启用自动格式化
              (when (executable-find "black")
                (add-hook 'before-save-hook 'blacken-buffer nil t))
              ;; 启用导入排序
              (when (executable-find "isort")
                (add-hook 'before-save-hook 'py-isort-buffer nil t)))))

(use-package! pyvenv
  :after python-mode
  :config
  ;; 自动检测虚拟环境
  (add-hook 'python-mode-hook 'pyvenv-mode)
  
  ;; 设置虚拟环境目录
  (setq pyvenv-workon "~/.virtualenvs/"))

(use-package! poetry
  :after python-mode)

(use-package! pipenv
  :after python-mode)

(use-package! py-isort
  :after python-mode
  :config
  (setq py-isort-options '("--multi-line=3" "--trailing-comma" "--force-grid-wrap=0" 
                          "--combine-as" "--line-width=88")))

(use-package! py-autopep8
  :after python-mode)

(use-package! blacken
  :after python-mode
  :config
  (setq blacken-line-length 88))

(use-package! python-pytest
  :after python-mode)

(use-package! python-docstring
  :after python-mode)

(use-package! sphinx-doc
  :after python-mode)

(use-package! live-py-mode
  :after python-mode)

(use-package! ein
  :after python-mode
  :config
  ;; 设置 EIN 配置
  (setq ein:output-area-inlined-images t)
  (setq ein:slice-image t)
  (setq ein:complete-on-dot t)
  (setq ein:completion-backend 'ein:use-ac-backend)
  
  ;; 设置默认内核
  (setq ein:default-kernel "python3")
  
  ;; 启用 matplotlib 内联显示
  (setq ein:matplotlib-backend-inline t)
  
  ;; 设置 Notebook 目录
  (setq ein:notebook-dir "~/notebooks/")
  
  ;; 自动保存设置
  (setq ein:notebook-autosave-frequency 300) ; 5分钟自动保存
  
  ;; 启用语法高亮
  (add-hook 'ein:notebook-mode-hook
            (lambda ()
              (setq truncate-lines nil)
              (visual-line-mode 1))))

(use-package! jupyter
  :after python-mode
  :config
  ;; 设置 Jupyter 配置
  (setq jupyter-eval-use-overlays t)
  (setq jupyter-include-other-output t)
  
  ;; 设置 REPL 配置
  (setq jupyter-repl-echo-eval-p t)
  (setq jupyter-repl-maximum-traceback-frames 20)
  
  ;; 启用 Org 模式集成
  (when (featurep 'org)
    (require 'ob-jupyter nil t))
  
  ;; 设置内核启动超时
  (setq jupyter-default-timeout 10))

(use-package! zmq
  :config
  ;; ZMQ 配置，Jupyter 通信需要
  (setq zmq-subprocess-timeout 10))

;; 设置 Python 相关的键绑定
(map! :after python-mode
      :localleader
      :map python-mode-map
      ;; 环境管理
      (:prefix ("e" . "environment")
       "a" #'+python/activate-venv
       "d" #'+python/deactivate-venv
       "c" #'+python/create-venv
       "l" #'+python/list-venvs)
      
      ;; 包管理
      (:prefix ("p" . "packages")
       "i" #'+python/pip-install
       "u" #'+python/pip-uninstall
       "U" #'+python/pip-upgrade
       "l" #'+python/pip-list
       "f" #'+python/pip-freeze
       "r" #'+python/pip-install-requirements)
      
      ;; Poetry
      (:prefix ("P" . "poetry")
       "i" #'+python/poetry-init
       "I" #'+python/poetry-install
       "a" #'+python/poetry-add
       "r" #'+python/poetry-remove
       "s" #'+python/poetry-shell
       "R" #'+python/poetry-run)
      
      ;; uv 包管理
      (:prefix ("u" . "uv")
       "i" #'+python/uv-init
       "a" #'+python/uv-add
       "A" #'+python/uv-add-dev
       "r" #'+python/uv-remove
       "s" #'+python/uv-sync
       "l" #'+python/uv-lock
       "u" #'+python/uv-update
       "t" #'+python/uv-tree
       "R" #'+python/uv-run
       "p" #'+python/uv-run-python
       "S" #'+python/uv-run-script
       "b" #'+python/uv-build
       "P" #'+python/uv-publish)
      
      ;; uv Python 版本管理
      (:prefix ("U" . "uv-python")
       "i" #'+python/uv-python-install
       "l" #'+python/uv-python-list
       "v" #'+python/uv-venv)
      
      ;; uv pip 兼容
      (:prefix ("v" . "uv-pip")
       "i" #'+python/uv-pip-install
       "l" #'+python/uv-pip-list
       "f" #'+python/uv-pip-freeze)
      
      ;; uv 工具管理
      (:prefix ("T" . "uv-tools")
       "i" #'+python/uv-tool-install
       "l" #'+python/uv-tool-list
       "r" #'+python/uv-tool-run)
      
      ;; uv 系统管理
      (:prefix ("S" . "uv-system")
       "c" #'+python/uv-cache-clean
       "u" #'+python/uv-self-update)
      
      ;; 代码质量
      (:prefix ("c" . "code-quality")
       "f" #'+python/format-buffer
       "s" #'+python/sort-imports
       "l" #'+python/lint-flake8
       "L" #'+python/lint-pylint
       "t" #'+python/lint-mypy
       "b" #'+python/bandit-security)
      
      ;; 测试
      (:prefix ("t" . "test")
       "t" #'+python/run-pytest
       "f" #'+python/run-pytest-file
       "F" #'+python/run-pytest-function
       "u" #'+python/run-unittest
       "d" #'+python/run-doctest
       "c" #'+python/coverage-run
       "h" #'+python/coverage-html)
      
      ;; 运行和调试
      (:prefix ("r" . "run")
       "r" #'+python/run-file
       "a" #'+python/run-file-with-args
       "d" #'+python/debug-file
       "p" #'+python/profile-file)
      
      ;; 文档
      (:prefix ("d" . "documentation")
       "d" #'+python/generate-docstring
       "s" #'+python/sphinx-quickstart
       "b" #'+python/sphinx-build)
      
      ;; 项目管理
      (:prefix ("n" . "new")
       "p" #'+python/create-package
       "f" #'+python/create-flask-app
       "d" #'+python/create-django-project
       "u" #'+python/create-uv-project
       "F" #'+python/create-uv-flask-app
       "A" #'+python/create-uv-fastapi-app)
      
      ;; 数据科学
      (:prefix ("j" . "jupyter")
       "n" #'+python/jupyter-notebook
       "l" #'+python/jupyter-lab
       "i" #'+python/install-data-science-packages
       "r" #'+python/jupyter-run-repl
       "c" #'+python/jupyter-connect-repl
       "e" #'+python/jupyter-eval-line-or-region
       "b" #'+python/jupyter-eval-buffer
       "f" #'+python/jupyter-eval-defun
       "?" #'+python/jupyter-inspect-at-point
       "z" #'+python/jupyter-repl-pop-to-buffer
       "R" #'+python/jupyter-repl-restart-kernel
       "o" #'+python/jupyter-org-mode)
      
      ;; EIN (Emacs IPython Notebook)
      (:prefix ("E" . "ein")
       "c" #'+python/ein-connect-to-notebook
       "n" #'+python/ein-new-notebook
       "o" #'+python/ein-open-notebook
       "s" #'+python/ein-save-notebook
       "r" #'+python/ein-rename-notebook
       "k" #'+python/ein-kill-kernel
       "R" #'+python/ein-restart-kernel)
      
      ;; Notebook 转换
      (:prefix ("C" . "convert")
       "p" #'+python/convert-notebook-to-py
       "n" #'+python/convert-py-to-notebook
       "h" #'+python/notebook-to-html
       "P" #'+python/notebook-to-pdf)
      
      ;; 工具
      (:prefix ("x" . "tools")
       "i" #'+python/install-dev-tools
       "l" #'live-py-mode))

;; 设置环境变量
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("PYTHONPATH" "VIRTUAL_ENV" "CONDA_DEFAULT_ENV")))

;; IPython 集成
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

;; 自动激活项目虚拟环境
(defun +python/auto-activate-venv ()
  "自动激活项目虚拟环境"
  (when (and (derived-mode-p 'python-mode)
             (not pyvenv-virtual-env))
    (let ((venv-path (or (locate-dominating-file default-directory "pyvenv.cfg")
                         (locate-dominating-file default-directory ".venv")
                         (locate-dominating-file default-directory "venv"))))
      (when venv-path
        (pyvenv-activate venv-path)))))

(add-hook 'python-mode-hook '+python/auto-activate-venv)

;; EIN 模式特定键绑定
(after! ein
  (map! :map ein:notebook-mode-map
        :localleader
        (:prefix ("c" . "cell")
         "r" #'+python/ein-run-cell
         "R" #'+python/ein-run-all-cells
         "a" #'+python/ein-insert-cell-above
         "b" #'+python/ein-insert-cell-below
         "d" #'+python/ein-delete-cell
         "t" #'+python/ein-change-cell-type
         "o" #'+python/ein-toggle-output
         "c" #'+python/ein-clear-output)
        (:prefix ("k" . "kernel")
         "i" #'+python/ein-kill-kernel
         "r" #'+python/ein-restart-kernel)
        (:prefix ("n" . "notebook")
         "s" #'+python/ein-save-notebook
         "r" #'+python/ein-rename-notebook)))

;; Jupyter REPL 模式键绑定
(after! jupyter
  (map! :map jupyter-repl-mode-map
        :localleader
        "r" #'+python/jupyter-repl-restart-kernel
        "c" #'jupyter-repl-clear-cells
        "h" #'jupyter-repl-history
        "?" #'jupyter-inspect-at-point))

;; Transient 菜单定义
(use-package! transient
  :after python-mode
  :config
  (transient-define-prefix +python/transient-menu ()
    "Python 开发菜单"
    ["环境管理"
     ("ea" "激活虚拟环境" +python/activate-venv)
     ("ed" "停用虚拟环境" +python/deactivate-venv)
     ("ec" "创建虚拟环境" +python/create-venv)
     ("el" "列出虚拟环境" +python/list-venvs)]
    ["包管理 (pip)"
     ("pi" "安装包" +python/pip-install)
     ("pu" "卸载包" +python/pip-uninstall)
     ("pU" "升级包" +python/pip-upgrade)
     ("pl" "列出包" +python/pip-list)
     ("pf" "生成 requirements.txt" +python/pip-freeze)
     ("pr" "安装 requirements.txt" +python/pip-install-requirements)]
    ["uv 包管理"
     ("ui" "初始化项目" +python/uv-init)
     ("ua" "添加依赖" +python/uv-add)
     ("uA" "添加开发依赖" +python/uv-add-dev)
     ("ur" "移除依赖" +python/uv-remove)
     ("us" "同步依赖" +python/uv-sync)
     ("ul" "锁定依赖" +python/uv-lock)
     ("uu" "更新依赖" +python/uv-update)
     ("up" "运行 Python" +python/uv-run-python)]
    ["Poetry"
     ("Pi" "初始化项目" +python/poetry-init)
     ("PI" "安装依赖" +python/poetry-install)
     ("Pa" "添加依赖" +python/poetry-add)
     ("Pr" "移除依赖" +python/poetry-remove)
     ("Ps" "Poetry shell" +python/poetry-shell)
     ("PR" "运行命令" +python/poetry-run)]
    ["代码质量"
     ("cf" "格式化代码" +python/format-buffer)
     ("cs" "排序导入" +python/sort-imports)
     ("cl" "flake8 检查" +python/lint-flake8)
     ("cL" "pylint 检查" +python/lint-pylint)
     ("ct" "mypy 类型检查" +python/lint-mypy)
     ("cb" "bandit 安全检查" +python/bandit-security)]
    ["测试"
     ("tt" "运行 pytest" +python/run-pytest)
     ("tf" "测试当前文件" +python/run-pytest-file)
     ("tF" "测试当前函数" +python/run-pytest-function)
     ("tu" "运行 unittest" +python/run-unittest)
     ("td" "运行 doctest" +python/run-doctest)
     ("tc" "覆盖率测试" +python/coverage-run)
     ("th" "HTML 覆盖率" +python/coverage-html)]
    ["运行和调试"
     ("rr" "运行文件" +python/run-file)
     ("ra" "带参数运行" +python/run-file-with-args)
     ("rd" "pdb 调试" +python/debug-file)
     ("rp" "性能分析" +python/profile-file)]
    ["项目模板"
     ("np" "创建 Python 包" +python/create-package)
     ("nf" "创建 Flask 应用" +python/create-flask-app)
     ("nd" "创建 Django 项目" +python/create-django-project)
     ("nu" "创建 uv 项目" +python/create-uv-project)
     ("nF" "创建 uv Flask 应用" +python/create-uv-flask-app)
     ("nA" "创建 uv FastAPI 应用" +python/create-uv-fastapi-app)]
    ["Jupyter"
     ("jn" "启动 Notebook" +python/jupyter-notebook)
     ("jl" "启动 Lab" +python/jupyter-lab)
     ("jr" "Jupyter REPL" +python/jupyter-run-repl)
     ("jc" "连接 REPL" +python/jupyter-connect-repl)
     ("je" "求值区域" +python/jupyter-eval-line-or-region)
     ("ji" "安装数据科学包" +python/install-data-science-packages)]
    ["EIN Notebook"
     ("En" "新建 Notebook" +python/ein-new-notebook)
     ("Eo" "打开 Notebook" +python/ein-open-notebook)
     ("Ec" "连接服务器" +python/ein-connect-to-notebook)
     ("Es" "保存 Notebook" +python/ein-save-notebook)]
    ["转换"
     ("Cp" "Notebook→Python" +python/convert-notebook-to-py)
     ("Cn" "Python→Notebook" +python/convert-py-to-notebook)
     ("Ch" "Notebook→HTML" +python/notebook-to-html)
     ("CP" "Notebook→PDF" +python/notebook-to-pdf)]
    ["工具"
     ("xi" "安装开发工具" +python/install-dev-tools)
     ("xl" "live-py-mode" live-py-mode)
     ("dd" "生成文档字符串" +python/generate-docstring)]
    ["退出"
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after python-mode
        :localleader
        :map python-mode-map
        "m" #'+python/transient-menu))