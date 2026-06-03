;;; my/golang/+templates.el -*- lexical-binding: t; -*-

;; 项目模板

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
          (insert "package main\n\nimport (\n\t\"log\"\n\t\"net/http\"\n)\n\nfunc main() {\n\thttp.HandleFunc(\"/\", func(w http.ResponseWriter, r *http.Request) {\n\t\tw.Write([]byte(\"Hello, World\"))\n\t})\n\n\tlog.Println(\"Server starting on :8080\")\n\tlog.Fatal(http.ListenAndServe(\":8080\", nil))\n}\n"))))))
