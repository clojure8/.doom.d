;;; my/zig/config.el -*- lexical-binding: t; -*-

;; Zig 项目管理函数
(defun +zig/init-project ()
  "初始化 Zig 项目"
  (interactive)
  (let ((project-type (completing-read "项目类型: " '("exe" "lib"))))
    (compile (format "zig init-%s" project-type))))

(defun +zig/create-project ()
  "创建新的 Zig 项目"
  (interactive)
  (let* ((project-name (read-string "项目名: "))
         (project-type (completing-read "项目类型: " '("exe" "lib")))
         (project-dir (read-directory-name "项目目录: "))
         (full-path (expand-file-name project-name project-dir)))
    
    (make-directory full-path t)
    (let ((default-directory full-path))
      (compile (format "zig init-%s" project-type))
      (message "已创建 Zig 项目: %s" full-path))))

(defun +zig/create-simple-project ()
  "创建简单的 Zig 项目结构"
  (interactive)
  (let* ((project-name (read-string "项目名: "))
         (project-dir (read-directory-name "项目目录: "))
         (full-path (expand-file-name project-name project-dir)))
    
    (make-directory full-path t)
    (let ((default-directory full-path))
      ;; 创建 build.zig
      (with-temp-file "build.zig"
        (insert "const std = @import(\"std\");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = \"" project-name "\",
        .root_source_file = .{ .path = \"src/main.zig\" },
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step(\"run\", \"Run the app\");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = \"src/main.zig\" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step(\"test\", \"Run unit tests\");
    test_step.dependOn(&run_unit_tests.step);
}
"))
      
      ;; 创建目录结构
      (make-directory "src" t)
      
      ;; 创建 main.zig
      (with-temp-file "src/main.zig"
        (insert "const std = @import(\"std\");

pub fn main() !void {
    std.debug.print(\"Hello, {s}!\\n\", .{\"" project-name "\"});
}

test \"simple test\" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit();
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
"))
      
      ;; 创建 .gitignore
      (with-temp-file ".gitignore"
        (insert "zig-out/\nzig-cache/\n"))
      
      ;; 创建 README.md
      (with-temp-file "README.md"
        (insert (format "# %s

A Zig project.

## Build

```bash
zig build
```

## Run

```bash
zig build run
```

## Test

```bash
zig build test
```
" project-name)))
      
      (message "已创建 Zig 项目: %s" full-path))))

;; 构建和运行
(defun +zig/build ()
  "构建 Zig 项目"
  (interactive)
  (compile "zig build"))

(defun +zig/build-release ()
  "发布构建 Zig 项目"
  (interactive)
  (compile "zig build -Doptimize=ReleaseFast"))

(defun +zig/run ()
  "运行 Zig 项目"
  (interactive)
  (compile "zig build run"))

(defun +zig/run-with-args ()
  "带参数运行 Zig 项目"
  (interactive)
  (let ((args (read-string "运行参数: ")))
    (compile (format "zig build run -- %s" args))))

(defun +zig/run-current-file ()
  "运行当前 Zig 文件"
  (interactive)
  (compile (format "zig run %s" (buffer-file-name))))

(defun +zig/check-current-file ()
  "检查当前 Zig 文件语法"
  (interactive)
  (compile (format "zig ast-check %s" (buffer-file-name))))

;; 测试相关
(defun +zig/test ()
  "运行 Zig 测试"
  (interactive)
  (compile "zig build test"))

(defun +zig/test-current-file ()
  "测试当前文件"
  (interactive)
  (compile (format "zig test %s" (buffer-file-name))))

(defun +zig/test-filter ()
  "运行过滤的测试"
  (interactive)
  (let ((filter (read-string "测试过滤器: ")))
    (compile (format "zig test %s --test-filter \"%s\"" (buffer-file-name) filter))))

;; 格式化
(defun +zig/format-buffer ()
  "格式化当前缓冲区"
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (shell-command (format "zig fmt %s" file))
      (revert-buffer t t))))

(defun +zig/format-project ()
  "格式化整个项目"
  (interactive)
  (compile "find . -name '*.zig' -exec zig fmt {} \\;"))

;; 文档和信息
(defun +zig/docs ()
  "打开 Zig 文档"
  (interactive)
  (browse-url "https://ziglang.org/documentation/master/"))

(defun +zig/std-docs ()
  "打开 Zig 标准库文档"
  (interactive)
  (browse-url "https://ziglang.org/documentation/master/std/"))

(defun +zig/version ()
  "显示 Zig 版本"
  (interactive)
  (compile "zig version"))

(defun +zig/env ()
  "显示 Zig 环境信息"
  (interactive)
  (compile "zig env"))

;; 交叉编译
(defun +zig/build-target ()
  "为特定目标构建"
  (interactive)
  (let ((target (read-string "目标平台 (如 x86_64-linux): ")))
    (compile (format "zig build -Dtarget=%s" target))))

(defun +zig/list-targets ()
  "列出支持的目标平台"
  (interactive)
  (compile "zig targets"))

;; 调试相关
(defun +zig/debug-build ()
  "调试构建"
  (interactive)
  (compile "zig build -Doptimize=Debug"))

(defun +zig/debug-executable ()
  "调试可执行文件"
  (interactive)
  (let ((executable (read-file-name "可执行文件: " "zig-out/bin/")))
    (dap-debug (list :type "lldb"
                    :request "launch"
                    :name "Debug Zig"
                    :program executable
                    :cwd default-directory))))

(defun +zig/toggle-breakpoint ()
  "切换断点"
  (interactive)
  (dap-breakpoint-toggle))

;; 包管理 (Zig 0.11+)
(defun +zig/fetch-package ()
  "获取包"
  (interactive)
  (let ((url (read-string "包 URL: ")))
    (compile (format "zig fetch %s" url))))

;; 工具和实用函数
(defun +zig/translate-c ()
  "将 C 代码转换为 Zig"
  (interactive)
  (let ((c-file (read-file-name "C 文件: ")))
    (compile (format "zig translate-c %s" c-file))))

(defun +zig/cc ()
  "使用 Zig 作为 C 编译器"
  (interactive)
  (let ((c-file (read-file-name "C 文件: "))
        (output (read-string "输出文件名: ")))
    (compile (format "zig cc %s -o %s" c-file output))))

(defun +zig/objdump ()
  "反汇编 Zig 可执行文件"
  (interactive)
  (let ((file (read-file-name "可执行文件: " "zig-out/bin/")))
    (compile (format "zig objdump %s" file))))

;; 性能分析
(defun +zig/build-profile ()
  "性能分析构建"
  (interactive)
  (compile "zig build -Doptimize=ReleaseFast -Dcpu=baseline"))

(defun +zig/benchmark ()
  "运行基准测试"
  (interactive)
  (compile "zig build test -Doptimize=ReleaseFast"))

;; 工具安装
(defun +zig/install-zig ()
  "安装 Zig"
  (interactive)
  (browse-url "https://ziglang.org/download/"))

(defun +zig/install-zls ()
  "安装 Zig Language Server"
  (interactive)
  (browse-url "https://github.com/zigtools/zls"))

(use-package! zig-mode
  :mode "\\.zig\\'"
  :config
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +zig-lsp-clients nil)
  
  ;; Zig 模式钩子
  (add-hook 'zig-mode-hook
            (lambda ()
              ;; 启用 lsp-bridge（如果全局未启用）
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              ;; 设置缩进
              (setq tab-width 4)
              (setq indent-tabs-mode nil)
              ;; 启用自动格式化
              (when (executable-find "zig")
                (add-hook 'before-save-hook '+zig/format-buffer nil t)))))

(use-package! dap-mode
  :after zig-mode
  :config
  ;; 注册 Zig 调试配置
  (dap-register-debug-template
   "Zig Debug"
   (list :type "lldb"
         :request "launch"
         :name "Debug Zig"
         :program nil
         :cwd nil)))

;; 设置 Zig 相关的键绑定
(map! :after zig-mode
      :localleader
      :map zig-mode-map
      ;; 项目管理
      (:prefix ("p" . "project")
       "i" #'+zig/init-project
       "c" #'+zig/create-project
       "s" #'+zig/create-simple-project)
      
      ;; 构建和运行
      (:prefix ("b" . "build")
       "b" #'+zig/build
       "r" #'+zig/build-release
       "d" #'+zig/debug-build
       "t" #'+zig/build-target
       "p" #'+zig/build-profile)
      
      ;; 运行
      (:prefix ("r" . "run")
       "r" #'+zig/run
       "a" #'+zig/run-with-args
       "f" #'+zig/run-current-file)
      
      ;; 测试
      (:prefix ("t" . "test")
       "t" #'+zig/test
       "f" #'+zig/test-current-file
       "F" #'+zig/test-filter
       "b" #'+zig/benchmark)
      
      ;; 格式化和检查
      (:prefix ("f" . "format")
       "f" #'+zig/format-buffer
       "p" #'+zig/format-project
       "c" #'+zig/check-current-file)
      
      ;; 调试
      (:prefix ("d" . "debug")
       "d" #'+zig/debug-executable
       "b" #'+zig/toggle-breakpoint)
      
      ;; 文档和信息
      (:prefix ("h" . "help")
       "d" #'+zig/docs
       "s" #'+zig/std-docs
       "v" #'+zig/version
       "e" #'+zig/env
       "t" #'+zig/list-targets)
      
      ;; 工具
      (:prefix ("x" . "tools")
       "c" #'+zig/translate-c
       "C" #'+zig/cc
       "o" #'+zig/objdump
       "f" #'+zig/fetch-package)
      
      ;; 安装
      (:prefix ("i" . "install")
       "z" #'+zig/install-zig
       "l" #'+zig/install-zls))

;; 设置环境变量
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("ZIG_GLOBAL_CACHE_DIR" "ZIG_LOCAL_CACHE_DIR")))

;; Transient 菜单定义
(use-package! transient
  :after zig-mode
  :config
  (transient-define-prefix +zig/transient-menu ()
    "Zig 开发菜单"
    ["项目管理"
     ("pi" "初始化项目" +zig/init-project)
     ("pc" "创建项目" +zig/create-project)
     ("ps" "创建简单项目" +zig/create-simple-project)]
    ["构建"
     ("bb" "构建项目" +zig/build)
     ("br" "发布构建" +zig/build-release)
     ("bd" "调试构建" +zig/debug-build)
     ("bt" "目标构建" +zig/build-target)
     ("bp" "性能构建" +zig/build-profile)]
    ["运行"
     ("rr" "运行项目" +zig/run)
     ("ra" "带参数运行" +zig/run-with-args)
     ("rf" "运行当前文件" +zig/run-current-file)]
    ["测试"
     ("tt" "运行测试" +zig/test)
     ("tf" "测试当前文件" +zig/test-current-file)
     ("tF" "过滤测试" +zig/test-filter)
     ("tb" "基准测试" +zig/benchmark)]
    ["格式化"
     ("ff" "格式化文件" +zig/format-buffer)
     ("fp" "格式化项目" +zig/format-project)
     ("fc" "检查语法" +zig/check-current-file)]
    ["调试"
     ("dd" "调试程序" +zig/debug-executable)
     ("db" "切换断点" +zig/toggle-breakpoint)]
    ["文档"
     ("hd" "Zig 文档" +zig/docs)
     ("hs" "标准库文档" +zig/std-docs)
     ("hv" "版本信息" +zig/version)
     ("ht" "目标平台" +zig/list-targets)]
    ["工具"
     ("xc" "转换 C 代码" +zig/translate-c)
     ("xC" "Zig CC" +zig/cc)
     ("xo" "反汇编" +zig/objdump)
     ("xf" "获取包" +zig/fetch-package)]
    ["退出"
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after zig-mode
        :localleader
        :map zig-mode-map
        "m" #'+zig/transient-menu))