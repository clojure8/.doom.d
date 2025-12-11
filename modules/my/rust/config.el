;;; my/rust/config.el -*- lexical-binding: t; -*-

;; 自定义 Rust 项目管理函数
(defun +rust/run-project ()
  "运行当前 Rust 项目"
  (interactive)
  (cargo-process-run))

(defun +rust/run-example ()
  "运行 Rust 示例"
  (interactive)
  (let ((example (completing-read "选择示例: " 
                                  (directory-files "examples" nil "\\.rs$"))))
    (cargo-process-run-example (file-name-sans-extension example))))

(defun +rust/build-project ()
  "构建当前 Rust 项目"
  (interactive)
  (cargo-process-build))

(defun +rust/build-release ()
  "发布构建当前 Rust 项目"
  (interactive)
  (cargo-process-build-release))

(defun +rust/test-project ()
  "运行项目所有测试"
  (interactive)
  (cargo-process-test))

(defun +rust/test-current-file ()
  "测试当前文件"
  (interactive)
  (let ((test-name (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (cargo-process-test-current-test)))

(defun +rust/bench-project ()
  "运行项目基准测试"
  (interactive)
  (cargo-process-bench))

(defun +rust/check-project ()
  "检查项目（快速编译检查）"
  (interactive)
  (cargo-process-check))

(defun +rust/clippy ()
  "运行 Clippy 代码检查"
  (interactive)
  (cargo-process-clippy))

(defun +rust/fmt ()
  "格式化代码"
  (interactive)
  (cargo-process-fmt))

(defun +rust/clean ()
  "清理构建产物"
  (interactive)
  (cargo-process-clean))

(defun +rust/doc ()
  "生成文档"
  (interactive)
  (cargo-process-doc))

(defun +rust/doc-open ()
  "生成并打开文档"
  (interactive)
  (cargo-process-doc-open))

(defun +rust/add-dependency ()
  "添加依赖"
  (interactive)
  (let ((crate (read-string "Crate 名称: "))
        (version (read-string "版本 (可选): ")))
    (if (string-empty-p version)
        (cargo-process-add crate)
      (cargo-process-add (format "%s@%s" crate version)))))

(defun +rust/update-dependencies ()
  "更新依赖"
  (interactive)
  (cargo-process-update))

(defun +rust/audit ()
  "安全审计"
  (interactive)
  (compile "cargo audit"))

(defun +rust/outdated ()
  "检查过时的依赖"
  (interactive)
  (compile "cargo outdated"))

(defun +rust/tree ()
  "显示依赖树"
  (interactive)
  (compile "cargo tree"))

(defun +rust/expand-macro ()
  "展开宏"
  (interactive)
  (compile "cargo expand"))

(defun +rust/install-tools ()
  "安装常用的 Rust 开发工具"
  (interactive)
  (let ((tools '("rust-analyzer"
                 "cargo-edit"
                 "cargo-audit"
                 "cargo-outdated"
                 "cargo-tree"
                 "cargo-expand"
                 "cargo-watch"
                 "cargo-flamegraph"
                 "cargo-criterion")))
    (dolist (tool tools)
      (compile (format "cargo install %s" tool)))))

(defun +rust/watch-run ()
  "监视文件变化并自动运行"
  (interactive)
  (compile "cargo watch -x run"))

(defun +rust/watch-test ()
  "监视文件变化并自动测试"
  (interactive)
  (compile "cargo watch -x test"))

(defun +rust/watch-check ()
  "监视文件变化并自动检查"
  (interactive)
  (compile "cargo watch -x check"))

(defun +rust/flamegraph ()
  "生成火焰图"
  (interactive)
  (compile "cargo flamegraph"))

(defun +rust/criterion-bench ()
  "运行 Criterion 基准测试"
  (interactive)
  (compile "cargo criterion"))

(defun +rust/new-bin-project ()
  "创建新的二进制项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "cargo new %s" project-name)))))

(defun +rust/new-lib-project ()
  "创建新的库项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "cargo new --lib %s" project-name)))))

(defun +rust/init-project ()
  "在当前目录初始化 Rust 项目"
  (interactive)
  (let ((project-type (completing-read "项目类型: " '("bin" "lib"))))
    (if (string= project-type "lib")
        (compile "cargo init --lib")
      (compile "cargo init"))))

(defun +rust/add-feature ()
  "为当前项目添加 feature"
  (interactive)
  (let ((feature (read-string "Feature 名称: ")))
    (with-current-buffer (find-file-noselect "Cargo.toml")
      (goto-char (point-min))
      (if (search-forward "[features]" nil t)
          (progn
            (end-of-line)
            (newline)
            (insert (format "%s = []" feature)))
        (goto-char (point-max))
        (newline)
        (insert "[features]")
        (newline)
        (insert (format "%s = []" feature)))
      (save-buffer))))

(defun +rust/toggle-feature ()
  "切换 feature 编译"
  (interactive)
  (let ((features (split-string (shell-command-to-string "cargo metadata --format-version 1 | jq -r '.packages[0].features | keys[]'") "\n" t)))
    (when features
      (let ((feature (completing-read "选择 feature: " features)))
        (setq cargo-process--command-flags (list "--features" feature))
        (message "已设置 feature: %s" feature)))))

(defun +rust/run-with-args ()
  "带参数运行项目"
  (interactive)
  (let ((args (read-string "运行参数: ")))
    (compile (format "cargo run -- %s" args))))

(defun +rust/profile-release ()
  "性能分析发布版本"
  (interactive)
  (compile "cargo build --release && perf record --call-graph=dwarf target/release/$(cargo metadata --format-version 1 | jq -r '.packages[0].name')"))

(use-package! rust-mode
  :mode "\\.rs\\'"
  :config
  ;; 设置 Rust 格式化工具
  (setq rust-format-on-save t)
  (setq rust-rustfmt-bin "rustfmt")
  
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +rust-lsp-clients nil)
  
  ;; Rust 模式钩子
  (add-hook 'rust-mode-hook
            (lambda ()
              ;; 启用 lsp-bridge（如果全局未启用）
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              ;; 设置编译命令
              (setq compile-command "cargo build")
              ;; 启用 flycheck-rust
              (when (featurep 'flycheck)
                (flycheck-rust-setup)))))

(use-package! cargo
  :after rust-mode
  :config
  ;; 自动检测 Cargo 项目
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  
  ;; 设置 Cargo 进程缓冲区
  (setq cargo-process--enable-rust-backtrace t))

(use-package! toml-mode
  :mode "\\.toml\\'")

(use-package! rust-playground
  :after rust-mode
  :config
  (setq rust-playground-basedir "~/rust/playground"))

(use-package! flycheck-rust
  :after (flycheck rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; 设置 Rust 相关的键绑定
(map! :after rust-mode
      :localleader
      :map rust-mode-map
      ;; 基础操作
      (:prefix ("r" . "rust")
       "f" #'+rust/fmt
       "c" #'+rust/clippy
       "d" #'+rust/doc
       "D" #'+rust/doc-open
       "p" #'rust-playground)
      
      ;; 运行相关
      (:prefix ("e" . "execute")
       "e" #'+rust/run-project
       "E" #'+rust/run-with-args
       "x" #'+rust/run-example
       "w" #'+rust/watch-run)
      
      ;; 构建相关
      (:prefix ("b" . "build")
       "b" #'+rust/build-project
       "r" #'+rust/build-release
       "c" #'+rust/check-project
       "C" #'+rust/clean
       "w" #'+rust/watch-check)
      
      ;; 测试相关
      (:prefix ("t" . "test")
       "t" #'+rust/test-project
       "f" #'+rust/test-current-file
       "b" #'+rust/bench-project
       "c" #'+rust/criterion-bench
       "w" #'+rust/watch-test)
      
      ;; 依赖管理
      (:prefix ("d" . "dependencies")
       "a" #'+rust/add-dependency
       "u" #'+rust/update-dependencies
       "A" #'+rust/audit
       "o" #'+rust/outdated
       "t" #'+rust/tree)
      
      ;; 项目管理
      (:prefix ("p" . "project")
       "n" #'+rust/new-bin-project
       "l" #'+rust/new-lib-project
       "i" #'+rust/init-project
       "f" #'+rust/add-feature
       "F" #'+rust/toggle-feature)
      
      ;; 性能分析
      (:prefix ("P" . "profile")
       "f" #'+rust/flamegraph
       "r" #'+rust/profile-release)
      
      ;; 工具相关
      (:prefix ("x" . "tools")
       "i" #'+rust/install-tools
       "m" #'+rust/expand-macro))

;; 设置环境变量
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("CARGO_HOME" "RUSTUP_HOME" "RUST_SRC_PATH")))

;; 自动补全 Cargo.toml
(after! company
  (add-hook 'toml-mode-hook
            (lambda ()
              (when (string-match-p "Cargo\\.toml\\'" (buffer-file-name))
                (setq-local company-backends '(company-capf))))))

;; 项目模板函数
(defun +rust/setup-workspace ()
  "设置 Rust 工作空间"
  (interactive)
  (let ((workspace-name (read-string "工作空间名称: ")))
    (with-temp-file "Cargo.toml"
      (insert (format "[workspace]\nmembers = [\n    \"%s\",\n]\n" workspace-name)))
    (+rust/new-bin-project)))

;; 添加工作空间管理到键绑定
(map! :after rust-mode
      :localleader
      :map rust-mode-map
      (:prefix ("w" . "workspace")
       "s" #'+rust/setup-workspace))

;; Transient 菜单定义
(use-package! transient
  :after rust-mode
  :config
  (transient-define-prefix +rust/transient-menu ()
    "Rust 开发菜单"
    ["基础操作"
     ("f" "格式化" +rust/fmt)
     ("c" "Clippy 检查" +rust/clippy)
     ("d" "生成文档" +rust/doc)
     ("D" "打开文档" +rust/doc-open)
     ("p" "Rust Playground" rust-playground)]
    ["运行"
     ("ee" "运行项目" +rust/run-project)
     ("eE" "带参数运行" +rust/run-with-args)
     ("ex" "运行示例" +rust/run-example)
     ("ew" "监视运行" +rust/watch-run)]
    ["构建"
     ("bb" "构建项目" +rust/build-project)
     ("br" "发布构建" +rust/build-release)
     ("bc" "快速检查" +rust/check-project)
     ("bC" "清理" +rust/clean)
     ("bw" "监视检查" +rust/watch-check)]
    ["测试"
     ("tt" "运行测试" +rust/test-project)
     ("tf" "测试当前文件" +rust/test-current-file)
     ("tb" "基准测试" +rust/bench-project)
     ("tc" "Criterion 基准" +rust/criterion-bench)
     ("tw" "监视测试" +rust/watch-test)]
    ["依赖管理"
     ("da" "添加依赖" +rust/add-dependency)
     ("du" "更新依赖" +rust/update-dependencies)
     ("dA" "安全审计" +rust/audit)
     ("do" "检查过时依赖" +rust/outdated)
     ("dt" "依赖树" +rust/tree)]
    ["项目管理"
     ("pn" "新建二进制项目" +rust/new-bin-project)
     ("pl" "新建库项目" +rust/new-lib-project)
     ("pi" "初始化项目" +rust/init-project)
     ("pf" "添加 feature" +rust/add-feature)
     ("pF" "切换 feature" +rust/toggle-feature)]
    ["性能分析"
     ("Pf" "火焰图" +rust/flamegraph)
     ("Pr" "性能分析" +rust/profile-release)]
    ["工具"
     ("xi" "安装工具" +rust/install-tools)
     ("xm" "展开宏" +rust/expand-macro)
     ("ws" "设置工作空间" +rust/setup-workspace)]
    ["退出"
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after rust-mode
        :localleader
        :map rust-mode-map
        "m" #'+rust/transient-menu))