;;; my/clojure/config.el -*- lexical-binding: t; -*-

;; Clojure 项目管理函数
(defun +clojure/create-lein-project ()
  "创建 Leiningen 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (template (completing-read "模板: " '("default" "app" "lib" "template" "plugin")))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (if (string= template "default")
          (compile (format "lein new %s" project-name))
        (compile (format "lein new %s %s" template project-name))))))

(defun +clojure/create-deps-project ()
  "创建 deps.edn 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((full-path (expand-file-name project-name project-dir)))
      (make-directory full-path t)
      (let ((default-directory full-path))
        (with-temp-file "deps.edn"
          (insert "{:paths [\"src\" \"resources\"]
 :deps {org.clojure/clojure {:mvn/version \"1.11.1\"}}
 :aliases
 {:test {:extra-paths [\"test\"]
         :extra-deps {org.clojure/test.check {:mvn/version \"1.1.1\"}}}
  :runner
  {:extra-deps {com.cognitect/test-runner
                {:git/url \"https://github.com/cognitect-labs/test-runner\"
                 :sha \"b6b3193fcc42659d7e46ecd1884a228993441182\"}}
   :main-opts [\"-m\" \"cognitect.test-runner\"
               \"-d\" \"test\"]}
  :uberjar {:replace-deps {com.github.seancorfield/depstar
                          {:mvn/version \"2.1.303\"}}
            :exec-fn hf.depstar/uberjar
            :exec-args {:aot true
                        :jar \"target/uberjar.jar\"
                        :main-class \"core\"
                        :sync-pom true}}}}
"))
        (make-directory "src" t)
        (make-directory "test" t)
        (make-directory "resources" t)
        (with-temp-file "src/core.clj"
          (insert (format "(ns core)

(defn -main
  \"Main entry point\"
  [& args]
  (println \"Hello, %s!\"))
" project-name)))
        (with-temp-file "test/core_test.clj"
          (insert "(ns core-test
  (:require [clojure.test :refer :all]
            [core :refer :all]))

(deftest a-test
  (testing \"FIXME, I fail.\"
    (is (= 0 1))))
"))
        (message "已创建 deps.edn 项目: %s" full-path)))))

(defun +clojure/create-shadow-cljs-project ()
  "创建 Shadow CLJS 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "npx create-cljs-project %s" project-name)))))

(defun +clojure/create-babashka-project ()
  "创建 Babashka 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((full-path (expand-file-name project-name project-dir)))
      (make-directory full-path t)
      (let ((default-directory full-path))
        (with-temp-file "bb.edn"
          (insert "{:deps {org.clojure/clojure {:mvn/version \"1.11.1\"}}
 :tasks
 {:requires ([babashka.fs :as fs])
  test {:doc \"Run tests\"
        :task (shell \"bb test\")}
  build {:doc \"Build project\"
         :task (println \"Building...\")}}}
"))
        (with-temp-file "src/main.clj"
          (insert (format "(ns main)

(defn -main [& args]
  (println \"Hello from %s!\"))

(when (= *file* (System/getProperty \"babashka.file\"))
  (-main))
" project-name)))
        (make-directory "test" t)
        (message "已创建 Babashka 项目: %s" full-path)))))

;; REPL 管理
(defun +clojure/start-repl ()
  "启动 CIDER REPL"
  (interactive)
  (cider-jack-in))

(defun +clojure/start-cljs-repl ()
  "启动 ClojureScript REPL"
  (interactive)
  (cider-jack-in-cljs))

(defun +clojure/connect-repl ()
  "连接到现有 REPL"
  (interactive)
  (cider-connect))

(defun +clojure/restart-repl ()
  "重启 REPL"
  (interactive)
  (cider-restart))

(defun +clojure/quit-repl ()
  "退出 REPL"
  (interactive)
  (cider-quit))

(defun +clojure/switch-to-repl ()
  "切换到 REPL 缓冲区"
  (interactive)
  (cider-switch-to-repl-buffer))

;; 代码求值
(defun +clojure/eval-buffer ()
  "求值整个缓冲区"
  (interactive)
  (cider-eval-buffer))

(defun +clojure/eval-defun ()
  "求值当前函数"
  (interactive)
  (cider-eval-defun-at-point))

(defun +clojure/eval-last-sexp ()
  "求值上一个表达式"
  (interactive)
  (cider-eval-last-sexp))

(defun +clojure/eval-region ()
  "求值选中区域"
  (interactive)
  (cider-eval-region (region-beginning) (region-end)))

(defun +clojure/eval-ns-form ()
  "求值命名空间表单"
  (interactive)
  (cider-eval-ns-form))

;; 测试相关
(defun +clojure/test-run-all ()
  "运行所有测试"
  (interactive)
  (cider-test-run-project-tests))

(defun +clojure/test-run-ns ()
  "运行当前命名空间的测试"
  (interactive)
  (cider-test-run-ns-tests))

(defun +clojure/test-run-test ()
  "运行当前测试"
  (interactive)
  (cider-test-run-test))

(defun +clojure/test-rerun-failed ()
  "重新运行失败的测试"
  (interactive)
  (cider-test-rerun-failed-tests))

(defun +clojure/test-show-report ()
  "显示测试报告"
  (interactive)
  (cider-test-show-report))

;; 文档和检查
(defun +clojure/doc-lookup ()
  "查看文档"
  (interactive)
  (cider-doc))

(defun +clojure/apropos ()
  "搜索符号"
  (interactive)
  (cider-apropos))

(defun +clojure/inspect-last-result ()
  "检查上次求值结果"
  (interactive)
  (cider-inspect-last-result))

(defun +clojure/macroexpand-1 ()
  "宏展开一层"
  (interactive)
  (cider-macroexpand-1))

(defun +clojure/macroexpand-all ()
  "完全宏展开"
  (interactive)
  (cider-macroexpand-all))

;; 重构
(defun +clojure/add-missing-libspec ()
  "添加缺失的库规范"
  (interactive)
  (cljr-add-missing-libspec))

(defun +clojure/clean-ns ()
  "清理命名空间"
  (interactive)
  (cljr-clean-ns))

(defun +clojure/extract-function ()
  "提取函数"
  (interactive)
  (cljr-extract-function))

(defun +clojure/inline-symbol ()
  "内联符号"
  (interactive)
  (cljr-inline-symbol))

(defun +clojure/thread-first-all ()
  "线程优先宏重构"
  (interactive)
  (cljr-thread-first-all))

(defun +clojure/thread-last-all ()
  "线程后置宏重构"
  (interactive)
  (cljr-thread-last-all))

;; 项目管理
(defun +clojure/lein-test ()
  "运行 Leiningen 测试"
  (interactive)
  (compile "lein test"))

(defun +clojure/lein-uberjar ()
  "构建 uberjar"
  (interactive)
  (compile "lein uberjar"))

(defun +clojure/lein-run ()
  "运行项目"
  (interactive)
  (compile "lein run"))

(defun +clojure/deps-test ()
  "运行 deps.edn 测试"
  (interactive)
  (compile "clojure -X:test"))

(defun +clojure/deps-uberjar ()
  "构建 deps.edn uberjar"
  (interactive)
  (compile "clojure -X:uberjar"))

(defun +clojure/bb-test ()
  "运行 Babashka 测试"
  (interactive)
  (compile "bb test"))

(defun +clojure/bb-run ()
  "运行 Babashka 脚本"
  (interactive)
  (compile "bb src/main.clj"))

;; 格式化
(defun +clojure/format-buffer ()
  "格式化当前缓冲区"
  (interactive)
  (cider-format-buffer))

(defun +clojure/format-region ()
  "格式化选中区域"
  (interactive)
  (cider-format-region (region-beginning) (region-end)))

;; 工具安装
(defun +clojure/install-tools ()
  "安装 Clojure 开发工具"
  (interactive)
  (let ((tools '("leiningen" "clojure" "babashka" "clj-kondo")))
    (message "请手动安装以下工具: %s" (mapconcat 'identity tools ", "))))

(use-package! clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'" "\\.edn\\'")
  :config
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +clojure-lsp-clients nil)
  
  ;; Clojure 模式钩子
  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; 启用 lsp-bridge（如果全局未启用）
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              ;; 启用结构化编辑
              (paredit-mode 1)
              ;; 启用自动缩进
              (aggressive-indent-mode 1)
              ;; 启用彩虹括号
              (rainbow-delimiters-mode 1)
              ;; 设置缩进
              (setq tab-width 2)
              (setq lisp-indent-offset 2))))

(use-package! cider
  :after clojure-mode
  :config
  ;; CIDER 配置
  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t)
  
  ;; 启用 eldoc
  (add-hook 'cider-mode-hook 'eldoc-mode)
  
  ;; REPL 配置
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-interactive-eval-result-prefix ";; => ")
  
  ;; 测试配置
  (setq cider-test-show-report-on-success t))

(use-package! clj-refactor
  :after clojure-mode
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1) ; for adding require/use/import statements
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package! flycheck-clj-kondo
  :after clojure-mode)

(use-package! rainbow-delimiters
  :hook (clojure-mode . rainbow-delimiters-mode))

(use-package! paredit
  :hook (clojure-mode . paredit-mode)
  :config
  ;; 使 paredit 与 evil 兼容
  (when (featurep 'evil)
    (add-hook 'paredit-mode-hook #'evil-cleverparens-mode)))

(use-package! aggressive-indent
  :hook (clojure-mode . aggressive-indent-mode))

;; 设置 Clojure 相关的键绑定
(map! :after clojure-mode
      :localleader
      :map clojure-mode-map
      ;; 项目管理
      (:prefix ("p" . "project")
       "l" #'+clojure/create-lein-project
       "d" #'+clojure/create-deps-project
       "s" #'+clojure/create-shadow-cljs-project
       "b" #'+clojure/create-babashka-project)
      
      ;; REPL 管理
      (:prefix ("r" . "repl")
       "j" #'+clojure/start-repl
       "J" #'+clojure/start-cljs-repl
       "c" #'+clojure/connect-repl
       "r" #'+clojure/restart-repl
       "q" #'+clojure/quit-repl
       "s" #'+clojure/switch-to-repl)
      
      ;; 代码求值
      (:prefix ("e" . "eval")
       "b" #'+clojure/eval-buffer
       "f" #'+clojure/eval-defun
       "e" #'+clojure/eval-last-sexp
       "r" #'+clojure/eval-region
       "n" #'+clojure/eval-ns-form)
      
      ;; 测试
      (:prefix ("t" . "test")
       "a" #'+clojure/test-run-all
       "n" #'+clojure/test-run-ns
       "t" #'+clojure/test-run-test
       "f" #'+clojure/test-rerun-failed
       "r" #'+clojure/test-show-report)
      
      ;; 文档和检查
      (:prefix ("d" . "doc")
       "d" #'+clojure/doc-lookup
       "a" #'+clojure/apropos
       "i" #'+clojure/inspect-last-result
       "m" #'+clojure/macroexpand-1
       "M" #'+clojure/macroexpand-all)
      
      ;; 重构
      (:prefix ("R" . "refactor")
       "a" #'+clojure/add-missing-libspec
       "c" #'+clojure/clean-ns
       "e" #'+clojure/extract-function
       "i" #'+clojure/inline-symbol
       "f" #'+clojure/thread-first-all
       "l" #'+clojure/thread-last-all)
      
      ;; 构建
      (:prefix ("b" . "build")
       "t" #'+clojure/lein-test
       "u" #'+clojure/lein-uberjar
       "r" #'+clojure/lein-run
       "T" #'+clojure/deps-test
       "U" #'+clojure/deps-uberjar
       "B" #'+clojure/bb-test
       "R" #'+clojure/bb-run)
      
      ;; 格式化
      (:prefix ("f" . "format")
       "b" #'+clojure/format-buffer
       "r" #'+clojure/format-region)
      
      ;; 工具
      (:prefix ("x" . "tools")
       "i" #'+clojure/install-tools))

;; 设置环境变量
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("LEIN_HOME" "CLOJURE_HOME" "JAVA_HOME")))

;; Transient 菜单定义
(use-package! transient
  :after clojure-mode
  :config
  (transient-define-prefix +clojure/transient-menu ()
    "Clojure 开发菜单"
    ["项目创建"
     ("pl" "Leiningen 项目" +clojure/create-lein-project)
     ("pd" "deps.edn 项目" +clojure/create-deps-project)
     ("ps" "Shadow CLJS 项目" +clojure/create-shadow-cljs-project)
     ("pb" "Babashka 项目" +clojure/create-babashka-project)]
    ["REPL"
     ("rj" "启动 REPL" +clojure/start-repl)
     ("rJ" "启动 CLJS REPL" +clojure/start-cljs-repl)
     ("rc" "连接 REPL" +clojure/connect-repl)
     ("rr" "重启 REPL" +clojure/restart-repl)
     ("rs" "切换到 REPL" +clojure/switch-to-repl)]
    ["求值"
     ("eb" "求值缓冲区" +clojure/eval-buffer)
     ("ef" "求值函数" +clojure/eval-defun)
     ("ee" "求值表达式" +clojure/eval-last-sexp)
     ("er" "求值区域" +clojure/eval-region)
     ("en" "求值命名空间" +clojure/eval-ns-form)]
    ["测试"
     ("ta" "运行所有测试" +clojure/test-run-all)
     ("tn" "测试命名空间" +clojure/test-run-ns)
     ("tt" "运行当前测试" +clojure/test-run-test)
     ("tf" "重跑失败测试" +clojure/test-rerun-failed)]
    ["文档"
     ("dd" "查看文档" +clojure/doc-lookup)
     ("da" "搜索符号" +clojure/apropos)
     ("di" "检查结果" +clojure/inspect-last-result)
     ("dm" "宏展开" +clojure/macroexpand-1)]
    ["重构"
     ("Ra" "添加库规范" +clojure/add-missing-libspec)
     ("Rc" "清理命名空间" +clojure/clean-ns)
     ("Re" "提取函数" +clojure/extract-function)
     ("Rf" "线程优先" +clojure/thread-first-all)]
    ["构建"
     ("bt" "Lein 测试" +clojure/lein-test)
     ("bu" "Lein Uberjar" +clojure/lein-uberjar)
     ("br" "Lein 运行" +clojure/lein-run)
     ("bT" "Deps 测试" +clojure/deps-test)]
    ["退出"
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after clojure-mode
        :localleader
        :map clojure-mode-map
        "m" #'+clojure/transient-menu))