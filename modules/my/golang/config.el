;;; my/golang/config.el -*- lexical-binding: t; -*-

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
  (compile (format "go run %s" (buffer-file-name))))

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
  (compile (format "go test -v %s" (file-name-directory (buffer-file-name)))))

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
    (compile (format "go get %s" package))))

(defun +go/install-tools ()
  "安装常用的 Go 开发工具"
  (interactive)
  (let ((tools '("golang.org/x/tools/cmd/goimports@latest"
                 "golang.org/x/tools/gopls@latest"
                 "github.com/go-delve/delve/cmd/dlv@latest"
                 "honnef.co/go/tools/cmd/staticcheck@latest"
                 "github.com/golangci/golangci-lint/cmd/golangci-lint@latest")))
    (dolist (tool tools)
      (compile (format "go install %s" tool)))))

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
    (when symbol
      (compile (format "go doc %s" symbol)))))

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
    (compile (format "go mod why %s" package))))

(defun +go/coverage ()
  "运行测试覆盖率分析"
  (interactive)
  (compile "go test -coverprofile=coverage.out ./... && go tool cover -html=coverage.out"))

(defun +go/profile-cpu ()
  "CPU 性能分析"
  (interactive)
  (compile "go test -cpuprofile=cpu.prof -bench=. && go tool pprof cpu.prof"))

(defun +go/profile-mem ()
  "内存性能分析"
  (interactive)
  (compile "go test -memprofile=mem.prof -bench=. && go tool pprof mem.prof"))

(defun +go/init-module ()
  "初始化 Go 模块"
  (interactive)
  (let ((module-name (read-string "模块名: ")))
    (compile (format "go mod init %s" module-name))))

(defun +go/add-build-tags ()
  "添加构建标签到当前文件"
  (interactive)
  (let ((tags (read-string "构建标签 (用空格分隔): ")))
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
  (let ((test-name (save-excursion
                     (when (re-search-backward "^func \\(Test[A-Za-z0-9_]*\\)" nil t)
                       (match-string 1)))))
    (if test-name
        (dap-debug (list :type "go"
                        :request "launch"
                        :name "Debug Test"
                        :mode "test"
                        :program (file-name-directory (buffer-file-name))
                        :args (list "-test.run" (format "^%s$" test-name))))
      (message "未找到测试函数"))))

(defun +go/debug-main ()
  "调试 main 函数"
  (interactive)
  (let ((main-file (or (locate-dominating-file default-directory "main.go")
                       (when (string-match "main\\.go$" (buffer-file-name))
                         (file-name-directory (buffer-file-name))))))
    (if main-file
        (dap-debug (list :type "go"
                        :request "launch"
                        :name "Debug Main"
                        :mode "debug"
                        :program main-file))
      (message "未找到 main.go 文件"))))

(defun +go/debug-current-file ()
  "调试当前文件"
  (interactive)
  (dap-debug (list :type "go"
                  :request "launch"
                  :name "Debug Current File"
                  :mode "debug"
                  :program (file-name-directory (buffer-file-name)))))

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

(defun +go/toggle-breakpoint ()
  "切换断点"
  (interactive)
  (dap-breakpoint-toggle))

(defun +go/clear-all-breakpoints ()
  "清除所有断点"
  (interactive)
  (dap-breakpoint-delete-all))

(defun +go/debug-continue ()
  "继续执行"
  (interactive)
  (dap-continue))

(defun +go/debug-step-over ()
  "单步跳过"
  (interactive)
  (dap-next))

(defun +go/debug-step-into ()
  "单步进入"
  (interactive)
  (dap-step-in))

(defun +go/debug-step-out ()
  "单步跳出"
  (interactive)
  (dap-step-out))

(defun +go/debug-restart ()
  "重启调试"
  (interactive)
  (dap-debug-restart))

(defun +go/debug-stop ()
  "停止调试"
  (interactive)
  (dap-disconnect))

(defun +go/debug-eval ()
  "求值表达式"
  (interactive)
  (call-interactively 'dap-eval))

(defun +go/debug-eval-region ()
  "求值选中区域"
  (interactive)
  (dap-eval-region (region-beginning) (region-end)))

(defun +go/debug-locals ()
  "显示局部变量"
  (interactive)
  (dap-ui-locals))

(defun +go/debug-sessions ()
  "显示调试会话"
  (interactive)
  (dap-ui-sessions))

(defun +go/debug-breakpoints ()
  "显示断点列表"
  (interactive)
  (dap-ui-breakpoints))

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
  ;; 设置 Go 相关的环境变量和路径
  (setq gofmt-command "goimports")
  (setq go-fontify-function-calls nil)
  
  ;; 在保存时自动格式化
  (add-hook 'before-save-hook 'gofmt-before-save)
  
  ;; 设置缩进
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +go-lsp-clients nil)
  
  ;; Go 模式钩子
  (add-hook 'go-mode-hook
            (lambda ()
              ;; 设置编译命令
              (setq compile-command "go build -v && go test -v && go vet")
              ;; 启用 lsp-bridge（如果全局未启用）
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              ;; 启用 eldoc
              (go-eldoc-setup)
              ;; 设置 GOPATH 和 GOROOT（如果需要）
              (when (getenv "GOPATH")
                (setq exec-path (cons (concat (getenv "GOPATH") "/bin") exec-path)))
              (when (getenv "GOROOT")
                (setq exec-path (cons (concat (getenv "GOROOT") "/bin") exec-path))))))

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

(use-package! go-eldoc
  :after go-mode)

(use-package! go-guru
  :after go-mode
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package! go-rename
  :after go-mode)

(use-package! dap-mode
  :after go-mode
  :config
  ;; 启用 dap-mode
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  
  ;; 启用鼠标悬停支持
  (tooltip-mode 1)
  
  ;; 注册 Go 调试适配器
  (require 'dap-dlv-go)
  
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
  
  ;; 设置断点样式
  (setq dap-breakpoint-condition-face 'dap-breakpoint-condition-face)
  
  ;; 自动显示调试窗口
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;; 设置 Go 相关的键绑定
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
       "b" #'+go/toggle-breakpoint
       "B" #'+go/clear-all-breakpoints
       "c" #'+go/debug-continue
       "n" #'+go/debug-step-over
       "i" #'+go/debug-step-into
       "o" #'+go/debug-step-out
       "R" #'+go/debug-restart
       "s" #'+go/debug-stop
       "e" #'+go/debug-eval
       "E" #'+go/debug-eval-region
       "l" #'+go/debug-locals
       "S" #'+go/debug-sessions
       "L" #'+go/debug-breakpoints)
      
      ;; dlv 服务器
      (:prefix ("D" . "dlv-server")
       "s" #'+go/start-dlv-server
       "t" #'+go/dlv-test-server))

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

;; 添加项目模板到键绑定
(map! :after go-mode
      :localleader
      :map go-mode-map
      (:prefix ("n" . "new")
       "c" #'+go/new-cli-project
       "w" #'+go/new-web-project))

;; Transient 菜单定义
(use-package! transient
  :after go-mode
  :config
  (transient-define-prefix +go/transient-menu ()
    "Go 开发菜单"
    ["基础操作"
     ("f" "填充结构体" go-fill-struct)
     ("i" "实现接口" go-impl)
     ("r" "重命名" go-rename)
     ("d" "文档" +go/doc-at-point)]
    ["标签管理"
     ("ta" "添加标签" go-tag-add)
     ("tr" "移除标签" go-tag-remove)]
    ["运行"
     ("rr" "运行当前文件" +go/run-current-file)
     ("rm" "运行 main.go" +go/run-main)
     ("rp" "Go Playground" go-playground)]
    ["构建"
     ("bb" "构建项目" +go/build-project)
     ("bv" "go vet" +go/vet-project)
     ("bg" "go generate" +go/generate)
     ("bc" "清理" +go/clean)]
    ["测试"
     ("tt" "生成测试" go-gen-test-dwim)
     ("tT" "生成所有测试" go-gen-test-all)
     ("tr" "测试当前文件" +go/test-current-file)
     ("tp" "测试项目" +go/test-project)
     ("tb" "基准测试" +go/benchmark-project)
     ("tc" "覆盖率" +go/coverage)]
    ["模块管理"
     ("mi" "初始化模块" +go/init-module)
     ("mt" "go mod tidy" +go/mod-tidy)
     ("md" "go mod download" +go/mod-download)
     ("mg" "获取包" +go/get-package)
     ("ml" "列出包" +go/list-packages)]
    ["性能分析"
     ("pc" "CPU 分析" +go/profile-cpu)
     ("pm" "内存分析" +go/profile-mem)]
    ["调试"
     ("dd" "调试 main" +go/debug-main)
     ("dt" "调试测试" +go/debug-test)
     ("df" "调试当前文件" +go/debug-current-file)
     ("db" "切换断点" +go/toggle-breakpoint)
     ("dc" "继续执行" +go/debug-continue)
     ("dn" "单步跳过" +go/debug-step-over)
     ("di" "单步进入" +go/debug-step-into)
     ("ds" "停止调试" +go/debug-stop)]
    ["项目"
     ("nc" "新建 CLI 项目" +go/new-cli-project)
     ("nw" "新建 Web 项目" +go/new-web-project)]
    ["工具"
     ("xi" "安装工具" +go/install-tools)
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after go-mode
        :localleader
        :map go-mode-map
        "m" #'+go/transient-menu))