;;; my/golang/config.el -*- lexical-binding: t; -*-

;; 共享 buffer 初始化，go-mode 和 go-ts-mode 均适用
(defun +go/setup-buffer ()
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)
  (add-hook 'before-save-hook #'gofmt-before-save nil t)
  (setq-local compile-command "go build -v && go test -v && go vet")
  (unless (bound-and-true-p lsp-bridge-mode)
    (lsp-bridge-mode 1))
  (when (getenv "GOPATH")
    (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin")))
  (when (getenv "GOROOT")
    (add-to-list 'exec-path (concat (getenv "GOROOT") "/bin"))))

(after! go-ts-mode
  ;; go-ts-mode 专属缩进偏移
  (setq-hook! 'go-ts-mode-hook go-ts-mode-indent-offset 4)
  (add-hook 'go-ts-mode-hook #'+go/setup-buffer)
  ;; 继承 go-mode-map 的 localleader 键绑定
  (set-keymap-parent go-ts-mode-map go-mode-map))

;; 自定义 Go 项目管理函数
(defun +go/run-main ()
  "运行当前项目的 main.go 文件"
  (interactive)
  (let ((main-file (or (locate-dominating-file default-directory "main.go")
                       (locate-dominating-file default-directory "cmd"))))
    (if main-file
        (let ((default-directory main-file))
          (compile "go run ."))
      (message "未找到 main.go 文件"))))

(defun +go/run-current-file ()
  "运行当前 Go 文件"
  (interactive)
  (if (buffer-file-name)
      (compile (format "go run %s" (shell-quote-argument (buffer-file-name))))
    (user-error "当前缓冲区没有关联文件")))

(defun +go/build-project ()
  "构建当前 Go 项目"
  (interactive)
  (compile "go build -v ./..."))

(defun +go/test-project ()
  "运行项目所有测试"
  (interactive)
  (compile "go test -v ./..."))

(defun +go/test-current-file ()
  "测试当前文件"
  (interactive)
  (if (buffer-file-name)
      (compile (format "go test -v %s" (shell-quote-argument (file-name-directory (buffer-file-name)))))
    (user-error "当前缓冲区没有关联文件")))

(defun +go/benchmark-project ()
  "运行项目基准测试"
  (interactive)
  (compile "go test -bench=. -benchmem ./..."))

(defun +go/vet-project ()
  "对项目运行 go vet"
  (interactive)
  (compile "go vet ./..."))

(defun +go/mod-tidy ()
  "运行 go mod tidy"
  (interactive)
  (compile "go mod tidy"))

(defun +go/mod-download ()
  "运行 go mod download"
  (interactive)
  (compile "go mod download"))

(defun +go/get-package ()
  "获取 Go 包"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "go get %s" (shell-quote-argument package)))))

(defun +go/install-tools ()
  "安装常用的 Go 开发工具"
  (interactive)
  (let* ((tools '("golang.org/x/tools/cmd/goimports@latest"
                  "golang.org/x/tools/gopls@latest"
                  "github.com/go-delve/delve/cmd/dlv@latest"
                  "honnef.co/go/tools/cmd/staticcheck@latest"
                  "github.com/golangci/golangci-lint/cmd/golangci-lint@latest"))
         (cmd (mapconcat (lambda (t) (format "go install %s" t)) tools " && ")))
    (compile cmd)))

(defun +go/generate ()
  "运行 go generate"
  (interactive)
  (compile "go generate ./..."))

(defun +go/clean ()
  "清理构建缓存"
  (interactive)
  (compile "go clean -cache -modcache -testcache"))

(defun +go/doc-at-point ()
  "显示光标处符号的文档"
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (compile (format "go doc %s" (shell-quote-argument symbol)))
      (user-error "光标处没有符号"))))

(defun +go/list-packages ()
  "列出项目中的所有包"
  (interactive)
  (compile "go list ./..."))

(defun +go/show-deps ()
  "显示项目依赖"
  (interactive)
  (compile "go list -m all"))

(defun +go/why-package ()
  "解释为什么需要某个包"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "go mod why %s" (shell-quote-argument package)))))

(defun +go/coverage ()
  "运行测试覆盖率分析"
  (interactive)
  (compile "go test -coverprofile=coverage.out ./... && go tool cover -html=coverage.out"))

(defun +go/profile-cpu ()
  "CPU 性能分析，生成后用 pprof web UI 展示（http://localhost:8080）"
  (interactive)
  (compile "go test -cpuprofile=cpu.prof -bench=. ./... && go tool pprof -http=:8080 cpu.prof"))

(defun +go/profile-mem ()
  "内存性能分析，生成后用 pprof web UI 展示（http://localhost:8080）"
  (interactive)
  (compile "go test -memprofile=mem.prof -bench=. ./... && go tool pprof -http=:8080 mem.prof"))

(defun +go/init-module (&optional module-name)
  "初始化 Go 模块"
  (interactive)
  (let ((module-name (or module-name (read-string "模块名: "))))
    (compile (format "go mod init %s" (shell-quote-argument module-name)))))

(defun +go/add-build-tags ()
  "添加构建标签到当前文件，多个标签用 && 或 || 连接（如 linux && amd64）"
  (interactive)
  (let ((tags (read-string "构建标签 (如: linux && amd64): ")))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "^//go:build")
          (progn
            (end-of-line)
            (insert (format " && (%s)" tags)))
        (insert (format "//go:build %s\n\n" tags))))))

;; 调试相关函数
(defun +go/debug-test ()
  "调试当前测试函数"
  (interactive)
  (if (buffer-file-name)
      (let ((test-name (save-excursion
                         (when (re-search-backward "^func \\(Test[A-Za-z0-9_]*\\)" nil t)
                           (match-string 1))))
            (program-path (expand-file-name (file-name-directory (buffer-file-name)))))
        (if test-name
            ;; 启动调试 (布局将由 hook 自动设置)
            (dap-debug (list :type "go"
                             :request "launch"
                             :name "Debug Test"
                             :mode "test"
                             :program program-path
                             :cwd program-path
                             :args (list "-test.run" (format "^%s$" test-name))))
          (message "未找到测试函数")))
    (message "当前缓冲区没有关联文件")))

(defun +go/debug-main ()
  "调试 main 函数"
  (interactive)
  (let ((main-dir (or (locate-dominating-file default-directory "main.go")
                      (when (and (buffer-file-name)
                                 (string-match "main\\.go$" (buffer-file-name)))
                        (file-name-directory (buffer-file-name))))))
    (if main-dir
        (let ((program-path (expand-file-name main-dir)))
          ;; 启动调试 (布局将由 hook 自动设置)
          (dap-debug (list :type "go"
                           :request "launch"
                           :name "Debug Main"
                           :mode "debug"
                           :program program-path
                           :cwd program-path)))
      (message "未找到 main.go 文件"))))

(defun +go/debug-current-file ()
  "调试当前文件"
  (interactive)
  (if (buffer-file-name)
      (let ((program-path (expand-file-name (file-name-directory (buffer-file-name)))))
        ;; 启动调试 (布局将由 hook 自动设置)
        (dap-debug (list :type "go"
                         :request "launch"
                         :name "Debug Current File"
                         :mode "debug"
                         :program program-path
                         :cwd program-path)))
    (message "当前缓冲区没有关联文件")))

(defun +go/debug-attach ()
  "附加到运行中的 Go 进程"
  (interactive)
  (let ((pid (read-string "进程 PID: ")))
    (dap-debug (list :type "go"
                     :request "attach"
                     :name "Attach to Process"
                     :mode "local"
                     :processId (string-to-number pid)))))

(defun +go/debug-remote ()
  "连接到远程调试服务器"
  (interactive)
  (let ((host (read-string "主机地址: " "localhost"))
        (port (read-string "端口: " "2345")))
    (dap-debug (list :type "go"
                     :request "attach"
                     :name "Remote Debug"
                     :mode "remote"
                     :remotePath ""
                     :host host
                     :port (string-to-number port)))))

(defun +go/debug-eval-region ()
  "求值选中区域"
  (interactive)
  (dap-eval-region (region-beginning) (region-end)))

(defun +go/start-dlv-server ()
  "启动 dlv 调试服务器"
  (interactive)
  (let ((port (read-string "端口: " "2345")))
    (compile (format "dlv debug --headless --listen=:%s --api-version=2 --accept-multiclient" port))))

(defun +go/dlv-test-server ()
  "启动 dlv 测试调试服务器"
  (interactive)
  (let ((port (read-string "端口: " "2345"))
        (test-name (read-string "测试函数 (可选): ")))
    (if (string-empty-p test-name)
        (compile (format "dlv test --headless --listen=:%s --api-version=2" port))
      (compile (format "dlv test --headless --listen=:%s --api-version=2 -- -test.run %s" port test-name)))))

(use-package! go-mode
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (setq go-fontify-function-calls nil)
  (setq +go-lsp-clients nil)

  (add-hook 'go-mode-hook #'+go/setup-buffer))

(use-package! go-tag
  :after go-mode
  :config
  (setq go-tag-args (list "-transform" "camelcase")))

(use-package! go-gen-test
  :after go-mode)

(use-package! go-impl
  :after go-mode)

(use-package! go-fill-struct
  :after go-mode)

(use-package! gorepl-mode
  :after go-mode)

(use-package! go-playground
  :after go-mode
  :config
  (setq go-playground-basedir "~/go/playground"))

(use-package! go-projectile
  :after (go-mode projectile))

;; go-eldoc 已移除：依赖废弃的 gocode，eldoc 由 lsp-bridge + gopls 提供

(use-package! go-guru
  :after go-mode
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  (add-hook 'go-ts-mode-hook #'go-guru-hl-identifier-mode))

(use-package! go-rename
  :after go-mode)

(use-package! dap-mode
  :after go-mode
  :init
  ;; 禁用默认的 dap-ui 控制窗口，使用自定义布局
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip))
  
  :config
  ;; 启用 dap-mode
  (dap-mode 1)
  (dap-ui-mode 1)
  (when (display-graphic-p)
    (dap-tooltip-mode 1)
    (tooltip-mode 1))
  
  ;; 注册 Go 调试适配器
  (require 'dap-dlv-go)
  
  ;; 自定义 DAP UI 缓冲区布局配置
  (setq dap-ui-buffer-configurations
        `(("*dap-ui-locals*" . ((side . right) (slot . 0) (window-width . 60)))
          ("*dap-ui-breakpoints*" . ((side . right) (slot . 1) (window-width . 60)))
          ("*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 60)))
          ("*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 60)))
          ("*dap-ui-repl*" . ((side . bottom) (slot . 0) (window-height . 12)))))
  
  ;; 设置 Go 调试模板
  (dap-register-debug-template
   "Go Debug Main"
   (list :type "go"
         :request "launch"
         :name "Debug Main"
         :mode "debug"
         :program nil
         :buildFlags nil
         :args nil
         :env nil
         :envFile nil))
  
  (dap-register-debug-template
   "Go Debug Test"
   (list :type "go"
         :request "launch"
         :name "Debug Test"
         :mode "test"
         :program nil
         :buildFlags nil
         :args nil
         :env nil
         :envFile nil))
  
  (dap-register-debug-template
   "Go Attach"
   (list :type "go"
         :request "attach"
         :name "Attach to Process"
         :mode "local"
         :processId nil))
  
  (dap-register-debug-template
   "Go Remote"
   (list :type "go"
         :request "attach"
         :name "Remote Debug"
         :mode "remote"
         :remotePath ""
         :host "localhost"
         :port 2345))
  )

;; 自定义 DAP 调试布局
(defvar +go/dap-debug-window-config nil
  "保存调试前的窗口配置")

(defvar +go/dap-ui-windows nil
  "保存 DAP UI 窗口引用")

(defun +go/dap-setup-debug-layout (&optional session)
  "设置 Go DAP 调试布局，位置由 dap-ui-buffer-configurations 控制。"
  (interactive)
  (setq +go/dap-debug-window-config (current-window-configuration))
  (dap-ui-locals)
  (dap-ui-breakpoints)
  (dap-ui-expressions)
  (dap-ui-repl))

(defun +go/dap-show-debug-log (session)
  (when-let ((buf (dap--debug-session-server-log-buffer session)))
    (display-buffer
     buf
     '((display-buffer-in-side-window)
       (side . bottom)
       (slot . 1)
       (window-height . 12)))))

(defun +go/dap-cleanup-debug-layout (&optional session)
  "清理调试布局，恢复之前的窗口配置"
  (interactive)
  ;; 关闭所有 side windows
  (when (window-with-parameter 'window-side)
    (window-toggle-side-windows))
  
  ;; 恢复窗口配置
  (when +go/dap-debug-window-config
    (set-window-configuration +go/dap-debug-window-config)
    (setq +go/dap-debug-window-config nil))
  
  ;; 关闭调试相关缓冲区
  (dolist (buf (buffer-list))
    (let ((buf-name (buffer-name buf)))
      (when (string-match-p "^\\*dap-ui-" buf-name)
        (kill-buffer buf))))
  
  ;; 关闭当前 session 的 log buffer
  (when session
    (let* ((session-name (dap--debug-session-name session))
           (log-buf-name (format " * %s log*" session-name)))
      (when (get-buffer log-buf-name)
        (kill-buffer log-buf-name)))))

;; 在调试会话启动时设置布局
(after! dap-mode
  (add-hook 'dap-session-created-hook #'+go/dap-show-debug-log)
  (add-hook 'dap-session-created-hook #'+go/dap-setup-debug-layout)
  
  ;; 在调试停止时显示 hydra
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  
  ;; 在调试会话终止时清理布局
  (add-hook 'dap-terminated-hook #'+go/dap-cleanup-debug-layout)
  
  ;; 在调试会话断开连接时也清理布局
  (add-hook 'dap-disconnected-hook #'+go/dap-cleanup-debug-layout))

;; 设置 Go 相关的键绑定（go-mode 和 go-ts-mode 共用）
(map! :after go-mode
      :localleader
      :map go-mode-map
      ;; 基础操作
      (:prefix ("g" . "go")
               "a" #'go-tag-add
               "r" #'go-tag-remove
               "i" #'go-impl
               "f" #'go-fill-struct
               "d" #'+go/doc-at-point
               "R" #'go-rename)

      ;; 测试相关
      (:prefix ("t" . "test")
               "t" #'go-gen-test-dwim
               "T" #'go-gen-test-all
               "r" #'+go/test-current-file
               "p" #'+go/test-project
               "b" #'+go/benchmark-project
               "c" #'+go/coverage)

      ;; 运行相关
      (:prefix ("r" . "run")
               "r" #'+go/run-current-file
               "m" #'+go/run-main
               "p" #'go-playground
               "R" #'gorepl-run
               "l" #'gorepl-run-load-current-file)

      ;; 构建相关
      (:prefix ("b" . "build")
               "b" #'+go/build-project
               "v" #'+go/vet-project
               "g" #'+go/generate
               "c" #'+go/clean
               "t" #'+go/add-build-tags)

      ;; 模块管理
      (:prefix ("m" . "module")
               "i" #'+go/init-module
               "t" #'+go/mod-tidy
               "d" #'+go/mod-download
               "g" #'+go/get-package
               "l" #'+go/list-packages
               "D" #'+go/show-deps
               "w" #'+go/why-package)

      ;; 性能分析
      (:prefix ("p" . "profile")
               "c" #'+go/profile-cpu
               "m" #'+go/profile-mem)

      ;; 工具相关
      (:prefix ("x" . "tools")
               "i" #'+go/install-tools)


      
      ;; Guru 相关
      (:prefix ("u" . "guru")
               "d" #'go-guru-describe
               "f" #'go-guru-freevars
               "i" #'go-guru-implements
               "c" #'go-guru-peers
               "r" #'go-guru-referrers
               "s" #'go-guru-callstack
               "e" #'go-guru-whicherrs
               "p" #'go-guru-pointsto
               "<" #'go-guru-callers
               ">" #'go-guru-callees)

      ;; 调试相关
      (:prefix ("d" . "debug")
               "d" #'+go/debug-main
               "t" #'+go/debug-test
               "f" #'+go/debug-current-file
               "a" #'+go/debug-attach
               "r" #'+go/debug-remote
               "b" #'dap-breakpoint-toggle
               "B" #'dap-breakpoint-delete-all
               "c" #'dap-continue
               "n" #'dap-next
               "i" #'dap-step-in
               "o" #'dap-step-out
               "R" #'dap-debug-restart
               "s" #'dap-disconnect
               "e" #'dap-eval
               "E" #'+go/debug-eval-region
               "l" #'dap-ui-locals
               "S" #'dap-ui-sessions
               "L" #'dap-ui-breakpoints
               "w" #'+go/dap-setup-debug-layout
               "W" #'+go/dap-cleanup-debug-layout)

      ;; dlv 服务器
      (:prefix ("D" . "dlv-server")
               "s" #'+go/start-dlv-server
               "t" #'+go/dlv-test-server)

      ;; 新建项目模板
      (:prefix ("n" . "new")
               "c" #'+go/new-cli-project
               "w" #'+go/new-web-project))

;; 设置 Go 工具路径
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("GOPATH" "GOROOT" "GO111MODULE" "GOPROXY" "GOSUMDB")))

;; 项目模板和代码片段
(defun +go/new-cli-project ()
  "创建新的 CLI 项目结构"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((full-path (expand-file-name project-name project-dir)))
      (make-directory full-path t)
      (let ((default-directory full-path))
        (+go/init-module project-name)
        (make-directory "cmd" t)
        (make-directory "internal" t)
        (make-directory "pkg" t)
        (with-temp-file "main.go"
          (insert "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n}\n"))
        (with-temp-file "README.md"
          (insert (format "# %s\n\n## 描述\n\n## 安装\n\n```bash\ngo install\n```\n\n## 使用\n\n```bash\n%s\n```\n" project-name project-name)))
        (with-temp-file ".gitignore"
          (insert "# Binaries\n*.exe\n*.exe~\n*.dll\n*.so\n*.dylib\n\n# Test binary\n*.test\n\n# Output\n*.out\n\n# Go workspace file\ngo.work\n"))))))

(defun +go/new-web-project ()
  "创建新的 Web 项目结构"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((full-path (expand-file-name project-name project-dir)))
      (make-directory full-path t)
      (let ((default-directory full-path))
        (+go/init-module project-name)
        (make-directory "cmd/server" t)
        (make-directory "internal/handler" t)
        (make-directory "internal/service" t)
        (make-directory "internal/repository" t)
        (make-directory "pkg/middleware" t)
        (make-directory "web/static" t)
        (make-directory "web/templates" t)
        (with-temp-file "cmd/server/main.go"
          (insert "package main\n\nimport (\n\t\"log\"\n\t\"net/http\"\n)\n\nfunc main() {\n\thttp.HandleFunc(\"/\", func(w http.ResponseWriter, r *http.Request) {\n\t\tw.Write([]byte(\"Hello, World!\"))\n\t})\n\n\tlog.Println(\"Server starting on :8080\")\n\tlog.Fatal(http.ListenAndServe(\":8080\", nil))\n}\n"))))))

;; Transient 菜单定义
(use-package! transient
  :after go-mode
  :config
  (transient-define-prefix +go/transient-menu ()
    "Go 开发菜单"
    [["基础操作"
      ("f"  "填充结构体      " go-fill-struct)
      ("i"  "实现接口        " go-impl)
      ("R"  "重命名          " go-rename)
      ("h"  "查看文档        " +go/doc-at-point)
      ("ga" "添加标签        " go-tag-add)
      ("gr" "移除标签        " go-tag-remove)]
     ["运行 & 构建"
      ("rr" "运行当前文件    " +go/run-current-file)
      ("rm" "运行 main.go    " +go/run-main)
      ("rp" "Go Playground   " go-playground)
      ("bb" "构建项目        " +go/build-project)
      ("bv" "go vet          " +go/vet-project)
      ("bg" "go generate     " +go/generate)]
     ["测试 & 分析"
      ("tt" "生成测试        " go-gen-test-dwim)
      ("tf" "测试当前文件    " +go/test-current-file)
      ("tp" "测试项目        " +go/test-project)
      ("tb" "基准测试        " +go/benchmark-project)
      ("tc" "测试覆盖率      " +go/coverage)
      ("pc" "CPU 性能分析    " +go/profile-cpu)]
     ["模块 & 工具"
      ("Mi" "初始化模块      " +go/init-module)
      ("Mt" "go mod tidy     " +go/mod-tidy)
      ("Mg" "获取包          " +go/get-package)
      ("Ml" "列出包          " +go/list-packages)
      ("xi" "安装开发工具    " +go/install-tools)
      ("nc" "新建 CLI 项目   " +go/new-cli-project)]
     ["调试控制"
      ("dd" "调试 main       " +go/debug-main)
      ("dt" "调试测试        " +go/debug-test)
      ("df" "调试当前文件    " +go/debug-current-file)
      ("db" "切换断点        " dap-breakpoint-toggle)
      ("dc" "继续执行        " dap-continue)
      ("ds" "停止调试        " dap-disconnect)
      ("dw" "设置调试布局    " +go/dap-setup-debug-layout)
      ("dW" "清理调试布局    " +go/dap-cleanup-debug-layout)]
     ["调试步进 & 其他"
      ("dn" "单步跳过        " dap-next)
      ("di" "单步进入        " dap-step-in)
      ("do" "单步跳出        " dap-step-out)
      ("de" "表达式求值      " dap-eval)
      ("dl" "查看局部变量    " dap-ui-locals)
      ("dS" "调试会话        " dap-ui-sessions)
      ("bc" "清理构建        " +go/clean)
      ("pm" "内存分析        " +go/profile-mem)
      ("nw" "新建 Web 项目   " +go/new-web-project)
      ("Md" "下载依赖        " +go/mod-download)
      ("q"  "退出菜单        " transient-quit-one)]])

  ;; 添加 transient 菜单键绑定
  (map! :after go-mode
        :localleader
        :map go-mode-map
        "m" #'+go/transient-menu))
