;;; my/c/config.el -*- lexical-binding: t; -*-

;; C/C++ 项目管理函数
(defun +c/create-cmake-project ()
  "创建 CMake 项目"
  (interactive)
  (let* ((project-name (read-string "项目名: "))
         (project-type (completing-read "项目类型: " '("executable" "library" "header-only")))
         (use-cpp (y-or-n-p "使用 C++? "))
         (cpp-standard (if use-cpp (completing-read "C++ 标准: " '("11" "14" "17" "20" "23")) ""))
         (project-dir (read-directory-name "项目目录: "))
         (full-path (expand-file-name project-name project-dir)))
    
    (make-directory full-path t)
    (let ((default-directory full-path))
      ;; 创建 CMakeLists.txt
      (with-temp-file "CMakeLists.txt"
        (insert (format "cmake_minimum_required(VERSION 3.10)
project(%s%s)

%s

# 设置输出目录
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# 包含目录
include_directories(include)

# 源文件
%s

# 创建目标
%s

# 编译选项
target_compile_options(%s PRIVATE
    -Wall -Wextra -Wpedantic
    $<$<CONFIG:Debug>:-g -O0>
    $<$<CONFIG:Release>:-O3 -DNDEBUG>
)

# 链接库 (如果需要)
# target_link_libraries(%s pthread)
"
                        project-name
                        (if use-cpp " LANGUAGES CXX" " LANGUAGES C")
                        (if use-cpp (format "set(CMAKE_CXX_STANDARD %s)\nset(CMAKE_CXX_STANDARD_REQUIRED ON)" cpp-standard) "set(CMAKE_C_STANDARD 99)")
                        (cond
                         ((string= project-type "executable") "file(GLOB_RECURSE SOURCES \"src/*.c*\")")
                         ((string= project-type "library") "file(GLOB_RECURSE SOURCES \"src/*.c*\")")
                         (t "# Header-only library"))
                        (cond
                         ((string= project-type "executable") (format "add_executable(%s ${SOURCES})" project-name))
                         ((string= project-type "library") (format "add_library(%s ${SOURCES})" project-name))
                         (t (format "add_library(%s INTERFACE)" project-name)))
                        project-name
                        project-name)))
      
      ;; 创建目录结构
      (make-directory "src" t)
      (make-directory "include" t)
      (make-directory "tests" t)
      (make-directory "build" t)
      
      ;; 创建主文件
      (let ((main-file (if use-cpp "src/main.cpp" "src/main.c"))
            (header-file (format "include/%s.h" project-name)))
        (when (not (string= project-type "header-only"))
          (with-temp-file main-file
            (insert (if use-cpp
                        (format "#include <iostream>
#include \"%s.h\"

int main() {
    std::cout << \"Hello from %s!\" << std::endl;
    return 0;
}
" project-name project-name)
                      (format "#include <stdio.h>
#include \"%s.h\"

int main() {
    printf(\"Hello from %s!\\n\");
    return 0;
}
" project-name project-name)))))
        
        ;; 创建头文件
        (with-temp-file header-file
          (insert (format "#ifndef %s_H
#define %s_H

%s

#endif // %s_H
"
                          (upcase project-name)
                          (upcase project-name)
                          (if use-cpp
                              "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n// 函数声明\n\n#ifdef __cplusplus\n}\n#endif"
                            "// 函数声明")
                          (upcase project-name)))))
      
      ;; 创建 .gitignore
      (with-temp-file ".gitignore"
        (insert "build/\n*.o\n*.so\n*.a\n*.exe\n.vscode/\n.idea/\n"))
      
      ;; 创建 README.md
      (with-temp-file "README.md"
        (insert (format "# %s

## 构建

```bash
mkdir build && cd build
cmake ..
make
```

## 运行

```bash
./bin/%s
```
" project-name project-name)))
      
      (message "已创建 CMake 项目: %s" full-path))))

(defun +c/create-makefile-project ()
  "创建 Makefile 项目"
  (interactive)
  (let* ((project-name (read-string "项目名: "))
         (use-cpp (y-or-n-p "使用 C++? "))
         (project-dir (read-directory-name "项目目录: "))
         (full-path (expand-file-name project-name project-dir)))
    
    (make-directory full-path t)
    (let ((default-directory full-path))
      ;; 创建 Makefile
      (with-temp-file "Makefile"
        (insert (format "%s := %s
%s := %s
CFLAGS := -Wall -Wextra -Wpedantic -std=%s
LDFLAGS := 
SRCDIR := src
INCDIR := include
OBJDIR := obj
BINDIR := bin

SOURCES := $(wildcard $(SRCDIR)/*.%s)
OBJECTS := $(SOURCES:$(SRCDIR)/%%.%s=$(OBJDIR)/%%.o)
TARGET := $(BINDIR)/%s

.PHONY: all clean debug release

all: $(TARGET)

$(TARGET): $(OBJECTS) | $(BINDIR)
\t$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

$(OBJDIR)/%%.o: $(SRCDIR)/%%.%s | $(OBJDIR)
\t$(CC) $(CFLAGS) -I$(INCDIR) -c $< -o $@

$(OBJDIR):
\tmkdir -p $(OBJDIR)

$(BINDIR):
\tmkdir -p $(BINDIR)

debug: CFLAGS += -g -O0 -DDEBUG
debug: $(TARGET)

release: CFLAGS += -O3 -DNDEBUG
release: $(TARGET)

clean:
\trm -rf $(OBJDIR) $(BINDIR)

install: $(TARGET)
\tcp $(TARGET) /usr/local/bin/

.PHONY: test
test: $(TARGET)
\t./$(TARGET)
"
                        (if use-cpp "CXX" "CC")
                        (if use-cpp "g++" "gcc")
                        (if use-cpp "CXXFLAGS" "CFLAGS")
                        (if use-cpp "-Wall -Wextra -Wpedantic -std=c++17" "-Wall -Wextra -Wpedantic -std=c99")
                        (if use-cpp "c99" "c99")
                        (if use-cpp "cpp" "c")
                        (if use-cpp "cpp" "c")
                        project-name
                        (if use-cpp "cpp" "c"))))
      
      ;; 创建目录结构
      (make-directory "src" t)
      (make-directory "include" t)
      
      ;; 创建主文件
      (let ((main-file (if use-cpp "src/main.cpp" "src/main.c")))
        (with-temp-file main-file
          (insert (if use-cpp
                      "#include <iostream>\n\nint main() {\n    std::cout << \"Hello, World!\" << std::endl;\n    return 0;\n}\n"
                    "#include <stdio.h>\n\nint main() {\n    printf(\"Hello, World!\\n\");\n    return 0;\n}\n"))))
      
      (message "已创建 Makefile 项目: %s" full-path))))

;; 构建和运行
(defun +c/cmake-configure ()
  "配置 CMake 项目"
  (interactive)
  (let ((build-type (completing-read "构建类型: " '("Debug" "Release" "RelWithDebInfo" "MinSizeRel"))))
    (compile (format "cmake -B build -DCMAKE_BUILD_TYPE=%s" build-type))))

(defun +c/cmake-build ()
  "构建 CMake 项目"
  (interactive)
  (compile "cmake --build build"))

(defun +c/cmake-clean ()
  "清理 CMake 构建"
  (interactive)
  (compile "cmake --build build --target clean"))

(defun +c/make-build ()
  "使用 Make 构建"
  (interactive)
  (compile "make"))

(defun +c/make-debug ()
  "调试构建"
  (interactive)
  (compile "make debug"))

(defun +c/make-release ()
  "发布构建"
  (interactive)
  (compile "make release"))

(defun +c/make-clean ()
  "清理构建"
  (interactive)
  (compile "make clean"))

(defun +c/run-executable ()
  "运行可执行文件"
  (interactive)
  (let ((executable (cond
                     ((file-exists-p "build/bin") 
                      (completing-read "选择可执行文件: " 
                                       (directory-files "build/bin" nil "^[^.]")))
                     ((file-exists-p "bin")
                      (completing-read "选择可执行文件: " 
                                       (directory-files "bin" nil "^[^.]")))
                     (t (read-string "可执行文件路径: ")))))
    (compile (format "./%s" executable))))

;; 调试功能
(defun +c/debug-executable ()
  "调试可执行文件"
  (interactive)
  (let ((executable (read-file-name "可执行文件: ")))
    (dap-debug (list :type "cppdbg"
                    :request "launch"
                    :name "Debug C/C++"
                    :program executable
                    :cwd (file-name-directory executable)
                    :stopAtEntry t))))

(defun +c/debug-attach ()
  "附加到进程调试"
  (interactive)
  (let ((pid (read-string "进程 PID: ")))
    (dap-debug (list :type "cppdbg"
                    :request "attach"
                    :name "Attach to Process"
                    :processId (string-to-number pid)))))

(defun +c/toggle-breakpoint ()
  "切换断点"
  (interactive)
  (dap-breakpoint-toggle))

(defun +c/debug-continue ()
  "继续执行"
  (interactive)
  (dap-continue))

(defun +c/debug-step-over ()
  "单步跳过"
  (interactive)
  (dap-next))

(defun +c/debug-step-into ()
  "单步进入"
  (interactive)
  (dap-step-in))

(defun +c/debug-step-out ()
  "单步跳出"
  (interactive)
  (dap-step-out))

;; 代码分析和工具
(defun +c/format-buffer ()
  "格式化当前缓冲区"
  (interactive)
  (clang-format-buffer))

(defun +c/format-region ()
  "格式化选中区域"
  (interactive)
  (clang-format-region (region-beginning) (region-end)))

(defun +c/disassemble ()
  "反汇编当前函数"
  (interactive)
  (disaster))

(defun +c/compile-single-file ()
  "编译单个文件"
  (interactive)
  (let* ((file (buffer-file-name))
         (output (file-name-sans-extension file))
         (is-cpp (string-match "\\.cpp\\|\\.cxx\\|\\.cc\\'" file))
         (compiler (if is-cpp "g++" "gcc"))
         (std (if is-cpp "-std=c++17" "-std=c99")))
    (compile (format "%s %s -Wall -Wextra -g -o %s %s" compiler std output file))))

(defun +c/run-current-file ()
  "编译并运行当前文件"
  (interactive)
  (let* ((file (buffer-file-name))
         (output (file-name-sans-extension file)))
    (+c/compile-single-file)
    (run-with-timer 2 nil (lambda () (compile (format "./%s" (file-name-nondirectory output)))))))

;; 测试相关
(defun +c/run-tests ()
  "运行测试"
  (interactive)
  (cond
   ((file-exists-p "build/tests") (compile "cd build && ctest"))
   ((file-exists-p "test") (compile "make test"))
   (t (message "未找到测试"))))

(defun +c/valgrind-check ()
  "使用 Valgrind 检查内存"
  (interactive)
  (let ((executable (read-file-name "可执行文件: ")))
    (compile (format "valgrind --leak-check=full --show-leak-kinds=all %s" executable))))

(defun +c/static-analysis ()
  "静态代码分析"
  (interactive)
  (let ((tool (completing-read "分析工具: " '("cppcheck" "clang-tidy" "scan-build"))))
    (cond
     ((string= tool "cppcheck")
      (compile "cppcheck --enable=all --std=c++17 src/"))
     ((string= tool "clang-tidy")
      (compile "clang-tidy src/*.cpp -- -std=c++17"))
     ((string= tool "scan-build")
      (compile "scan-build make")))))

;; 工具安装
(defun +c/install-tools ()
  "安装 C/C++ 开发工具"
  (interactive)
  (message "请手动安装以下工具: gcc/clang, cmake, gdb/lldb, valgrind, cppcheck, clang-format"))

(use-package! cc-mode
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode))
  :config
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +c-lsp-clients nil)
  
  ;; C/C++ 模式钩子
  (add-hook 'c-mode-common-hook
            (lambda ()
              ;; 启用 lsp-bridge（如果全局未启用）
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              ;; 设置缩进风格
              (setq c-default-style "linux"
                    c-basic-offset 4)
              ;; 启用自动格式化
              (when (executable-find "clang-format")
                (add-hook 'before-save-hook 'clang-format-buffer nil t)))))

(use-package! cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package! cmake-ide
  :after cmake-mode
  :config
  (cmake-ide-setup))

(use-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package! clang-format
  :after cc-mode
  :config
  (setq clang-format-style "file"))

(use-package! disaster
  :after cc-mode)

(use-package! dap-mode
  :after cc-mode
  :config
  ;; 启用 dap-mode
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  
  ;; 注册 C/C++ 调试适配器
  (require 'dap-gdb-lldb)
  
  ;; 设置调试模板
  (dap-register-debug-template
   "C/C++ Debug"
   (list :type "cppdbg"
         :request "launch"
         :name "Debug C/C++"
         :program nil
         :cwd nil
         :stopAtEntry t))
  
  (dap-register-debug-template
   "C/C++ Attach"
   (list :type "cppdbg"
         :request "attach"
         :name "Attach to Process"
         :processId nil)))

;; 设置 C/C++ 相关的键绑定
(map! :after cc-mode
      :localleader
      :map (c-mode-map c++-mode-map)
      ;; 项目管理
      (:prefix ("p" . "project")
       "c" #'+c/create-cmake-project
       "m" #'+c/create-makefile-project)
      
      ;; 构建
      (:prefix ("b" . "build")
       "c" #'+c/cmake-configure
       "b" #'+c/cmake-build
       "C" #'+c/cmake-clean
       "m" #'+c/make-build
       "d" #'+c/make-debug
       "r" #'+c/make-release
       "M" #'+c/make-clean
       "f" #'+c/compile-single-file
       "R" #'+c/run-executable)
      
      ;; 调试
      (:prefix ("d" . "debug")
       "d" #'+c/debug-executable
       "a" #'+c/debug-attach
       "b" #'+c/toggle-breakpoint
       "c" #'+c/debug-continue
       "n" #'+c/debug-step-over
       "i" #'+c/debug-step-into
       "o" #'+c/debug-step-out)
      
      ;; 代码质量
      (:prefix ("l" . "lint")
       "f" #'+c/format-buffer
       "r" #'+c/format-region
       "s" #'+c/static-analysis
       "v" #'+c/valgrind-check)
      
      ;; 测试
      (:prefix ("t" . "test")
       "t" #'+c/run-tests
       "v" #'+c/valgrind-check)
      
      ;; 运行
      (:prefix ("r" . "run")
       "r" #'+c/run-current-file
       "e" #'+c/run-executable)
      
      ;; 工具
      (:prefix ("x" . "tools")
       "d" #'+c/disassemble
       "i" #'+c/install-tools))

;; 设置环境变量
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("CC" "CXX" "CFLAGS" "CXXFLAGS" "LDFLAGS")))

;; Transient 菜单定义
(use-package! transient
  :after cc-mode
  :config
  (transient-define-prefix +c/transient-menu ()
    "C/C++ 开发菜单"
    ["项目创建"
     ("pc" "CMake 项目" +c/create-cmake-project)
     ("pm" "Makefile 项目" +c/create-makefile-project)]
    ["构建"
     ("bc" "CMake 配置" +c/cmake-configure)
     ("bb" "CMake 构建" +c/cmake-build)
     ("bC" "CMake 清理" +c/cmake-clean)
     ("bm" "Make 构建" +c/make-build)
     ("bd" "调试构建" +c/make-debug)
     ("br" "发布构建" +c/make-release)]
    ["调试"
     ("dd" "调试程序" +c/debug-executable)
     ("da" "附加调试" +c/debug-attach)
     ("db" "切换断点" +c/toggle-breakpoint)
     ("dc" "继续执行" +c/debug-continue)
     ("dn" "单步跳过" +c/debug-step-over)
     ("di" "单步进入" +c/debug-step-into)]
    ["代码质量"
     ("lf" "格式化缓冲区" +c/format-buffer)
     ("lr" "格式化区域" +c/format-region)
     ("ls" "静态分析" +c/static-analysis)
     ("lv" "Valgrind 检查" +c/valgrind-check)]
    ["运行"
     ("rr" "运行当前文件" +c/run-current-file)
     ("re" "运行可执行文件" +c/run-executable)
     ("tt" "运行测试" +c/run-tests)]
    ["工具"
     ("xd" "反汇编" +c/disassemble)
     ("xi" "安装工具" +c/install-tools)]
    ["退出"
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after cc-mode
        :localleader
        :map (c-mode-map c++-mode-map)
        "m" #'+c/transient-menu))