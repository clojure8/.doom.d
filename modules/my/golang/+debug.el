;;; my/golang/+debug.el -*- lexical-binding: t; -*-

;; DAP 调试相关函数

(defun +go/debug-test ()
  "调试当前测试函数"
  (interactive)
  (if (buffer-file-name)
      (let ((test-name (save-excursion
                         (when (re-search-backward "^func \\(Test[A-Za-z0-9_]*\\)" nil t)
                           (match-string 1))))
            (program-path (expand-file-name (file-name-directory (buffer-file-name)))))
        (if test-name
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

;; ── DAP 模式配置 ──────────────────────────────────────────────────────

;; ⚠️ 性能：原来是 `:after go-mode`，意味着一加载 go-mode 就立刻 `(dap-mode 1)`
;; 全局启用 dap-mode —— 而 dap-mode 依赖 lsp-mode / lsp-treemacs / treemacs，会把
;; 这一整套重包（约 3.6s）在「打开 go 文件 / 启动」时全拖起来，而你平时用 lsp-bridge
;; 根本不碰 dap。改成懒加载：只有真正按调试命令（+go/debug-* 会调 `dap-debug'，
;; 它是 dap-mode 的 autoload）时才加载 dap-mode，下面的 :config 也才执行。
(use-package! dap-mode
  :defer t
  :commands (dap-debug dap-debug-edit-template dap-hydra)
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
         :port 2345)))

;; ── DAP 布局管理 ──────────────────────────────────────────────────────

(defvar +go/dap-debug-window-config nil
  "保存调试前的窗口配置")

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
