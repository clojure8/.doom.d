;;; my/react/config.el -*- lexical-binding: t; -*-

;; React 项目管理函数
(defun +react/create-react-app ()
  "创建 Create React App 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (template (completing-read "模板: " '("default" "typescript" "redux" "redux-typescript")))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (if (string= template "default")
          (compile (format "npx create-react-app %s" project-name))
        (compile (format "npx create-react-app %s --template %s" project-name template))))))

(defun +react/create-vite-react ()
  "创建 Vite + React 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (template (completing-read "模板: " '("react" "react-ts" "react-swc" "react-swc-ts")))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "npm create vite@latest %s -- --template %s" project-name template)))))

(defun +react/create-nextjs-app ()
  "创建 Next.js 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (use-typescript (y-or-n-p "使用 TypeScript? "))
        (use-eslint (y-or-n-p "使用 ESLint? "))
        (use-tailwind (y-or-n-p "使用 Tailwind CSS? "))
        (use-app-router (y-or-n-p "使用 App Router? "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir)
          (flags (concat
                  (if use-typescript " --typescript" " --javascript")
                  (if use-eslint " --eslint" " --no-eslint")
                  (if use-tailwind " --tailwind" " --no-tailwind")
                  (if use-app-router " --app" " --src-dir"))))
      (compile (format "npx create-next-app@latest %s%s" project-name flags)))))

(defun +react/create-expo-app ()
  "创建 React Native Expo 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (template (completing-read "模板: " '("blank" "blank-typescript" "tabs" "bare-minimum")))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "npx create-expo-app %s --template %s" project-name template)))))

(defun +react/create-gatsby-site ()
  "创建 Gatsby 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (starter (completing-read "Starter: " '("default" "blog" "hello-world" "minimal")))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (if (string= starter "default")
          (compile (format "npx gatsby new %s" project-name))
        (compile (format "npx gatsby new %s https://github.com/gatsbyjs/gatsby-starter-%s" project-name starter))))))

;; 开发服务器管理
(defun +react/dev-server ()
  "启动开发服务器"
  (interactive)
  (let ((command (cond
                  ((file-exists-p "next.config.js") "npm run dev")
                  ((file-exists-p "gatsby-config.js") "npm run develop")
                  ((file-exists-p "expo.json") "npx expo start")
                  ((file-exists-p "vite.config.js") "npm run dev")
                  (t "npm start"))))
    (compile command)))

(defun +react/build-project ()
  "构建项目"
  (interactive)
  (compile "npm run build"))

(defun +react/build-storybook ()
  "构建 Storybook"
  (interactive)
  (compile "npm run build-storybook"))

(defun +react/start-storybook ()
  "启动 Storybook"
  (interactive)
  (compile "npm run storybook"))

(defun +react/export-static ()
  "导出静态文件 (Next.js)"
  (interactive)
  (compile "npm run export"))

;; 测试相关
(defun +react/test-run ()
  "运行测试"
  (interactive)
  (compile "npm test"))

(defun +react/test-watch ()
  "监视模式运行测试"
  (interactive)
  (compile "npm test -- --watch"))

(defun +react/test-coverage ()
  "运行测试覆盖率"
  (interactive)
  (compile "npm test -- --coverage"))

(defun +react/test-current-file ()
  "测试当前文件"
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (compile (format "npm test -- %s" file-name))))

(defun +react/jest-run ()
  "运行 Jest 测试"
  (interactive)
  (jest-run))

;; 代码质量
(defun +react/lint-project ()
  "代码检查"
  (interactive)
  (compile "npm run lint"))

(defun +react/lint-fix ()
  "自动修复代码问题"
  (interactive)
  (compile "npm run lint -- --fix"))

(defun +react/format-buffer ()
  "格式化当前缓冲区"
  (interactive)
  (cond
   ((executable-find "prettier") (prettier-js))
   (t (message "未找到 prettier"))))

(defun +react/format-project ()
  "格式化整个项目"
  (interactive)
  (compile "npx prettier --write ."))

;; 包管理
(defun +react/install-dependencies ()
  "安装依赖"
  (interactive)
  (compile "npm install"))

(defun +react/add-dependency ()
  "添加依赖"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "npm install %s" package))))

(defun +react/add-dev-dependency ()
  "添加开发依赖"
  (interactive)
  (let ((package (read-string "开发依赖包名: ")))
    (compile (format "npm install --save-dev %s" package))))

(defun +react/remove-dependency ()
  "移除依赖"
  (interactive)
  (let ((package (read-string "要移除的包名: ")))
    (compile (format "npm uninstall %s" package))))

(defun +react/update-dependencies ()
  "更新依赖"
  (interactive)
  (compile "npm update"))

(defun +react/audit-dependencies ()
  "安全审计"
  (interactive)
  (compile "npm audit"))

(defun +react/audit-fix ()
  "自动修复安全问题"
  (interactive)
  (compile "npm audit fix"))

;; React 组件生成
(defun +react/create-component ()
  "创建 React 组件"
  (interactive)
  (let* ((component-name (read-string "组件名: "))
         (component-dir (read-directory-name "组件目录: " "src/components/"))
         (use-typescript (y-or-n-p "使用 TypeScript? "))
         (component-type (completing-read "组件类型: " '("functional" "class")))
         (use-hooks (and (string= component-type "functional") (y-or-n-p "使用 Hooks? ")))
         (file-extension (if use-typescript ".tsx" ".jsx"))
         (component-path (expand-file-name (concat component-name file-extension) component-dir)))
    
    (make-directory component-dir t)
    
    (with-temp-file component-path
      (insert (if (string= component-type "functional")
                  (if use-typescript
                      (format "import React%s from 'react'

interface %sProps {
  // 定义 props 类型
}

const %s: React.FC<%sProps> = () => {
%s
  return (
    <div className=\"%s\">
      <h1>%s Component</h1>
    </div>
  )
}

export default %s
"
                              (if use-hooks ", { useState, useEffect }" "")
                              component-name
                              component-name
                              component-name
                              (if use-hooks "  // const [state, setState] = useState()\n\n  // useEffect(() => {\n  //   // 副作用逻辑\n  // }, [])\n" "")
                              (downcase component-name)
                              component-name
                              component-name)
                    (format "import React%s from 'react'

const %s = () => {
%s
  return (
    <div className=\"%s\">
      <h1>%s Component</h1>
    </div>
  )
}

export default %s
"
                            (if use-hooks ", { useState, useEffect }" "")
                            component-name
                            (if use-hooks "  // const [state, setState] = useState()\n\n  // useEffect(() => {\n  //   // 副作用逻辑\n  // }, [])\n" "")
                            (downcase component-name)
                            component-name
                            component-name))
                (if use-typescript
                    (format "import React, { Component } from 'react'

interface %sProps {
  // 定义 props 类型
}

interface %sState {
  // 定义 state 类型
}

class %s extends Component<%sProps, %sState> {
  constructor(props: %sProps) {
    super(props)
    this.state = {
      // 初始状态
    }
  }

  render() {
    return (
      <div className=\"%s\">
        <h1>%s Component</h1>
      </div>
    )
  }
}

export default %s
"
                            component-name
                            component-name
                            component-name
                            component-name
                            component-name
                            component-name
                            (downcase component-name)
                            component-name
                            component-name)
                  (format "import React, { Component } from 'react'

class %s extends Component {
  constructor(props) {
    super(props)
    this.state = {
      // 初始状态
    }
  }

  render() {
    return (
      <div className=\"%s\">
        <h1>%s Component</h1>
      </div>
    )
  }
}

export default %s
"
                          component-name
                          (downcase component-name)
                          component-name
                          component-name))))
    
    ;; 创建对应的 CSS 文件
    (let ((css-path (expand-file-name (concat component-name ".module.css") component-dir)))
      (with-temp-file css-path
        (insert (format ".%s {
  /* 组件样式 */
}
" (downcase component-name)))))
    
    (find-file component-path)
    (message "已创建组件: %s" component-path)))

(defun +react/create-hook ()
  "创建自定义 Hook"
  (interactive)
  (let* ((hook-name (read-string "Hook 名 (use开头): "))
         (hook-dir (read-directory-name "Hook 目录: " "src/hooks/"))
         (use-typescript (y-or-n-p "使用 TypeScript? "))
         (file-extension (if use-typescript ".ts" ".js"))
         (hook-path (expand-file-name (concat hook-name file-extension) hook-dir)))
    
    (make-directory hook-dir t)
    
    (with-temp-file hook-path
      (insert (if use-typescript
                  (format "import { useState, useEffect } from 'react'

interface %sReturn {
  // 定义返回值类型
}

const %s = (): %sReturn => {
  const [state, setState] = useState()

  useEffect(() => {
    // Hook 逻辑
  }, [])

  return {
    // 返回值
  }
}

export default %s
"
                          (capitalize (substring hook-name 3))
                          hook-name
                          (capitalize (substring hook-name 3))
                          hook-name)
                (format "import { useState, useEffect } from 'react'

const %s = () => {
  const [state, setState] = useState()

  useEffect(() => {
    // Hook 逻辑
  }, [])

  return {
    // 返回值
  }
}

export default %s
"
                        hook-name
                        hook-name))))
    
    (find-file hook-path)
    (message "已创建 Hook: %s" hook-path)))

(defun +react/create-context ()
  "创建 React Context"
  (interactive)
  (let* ((context-name (read-string "Context 名: "))
         (context-dir (read-directory-name "Context 目录: " "src/contexts/"))
         (use-typescript (y-or-n-p "使用 TypeScript? "))
         (file-extension (if use-typescript ".tsx" ".jsx"))
         (context-path (expand-file-name (concat context-name "Context" file-extension) context-dir)))
    
    (make-directory context-dir t)
    
    (with-temp-file context-path
      (insert (if use-typescript
                  (format "import React, { createContext, useContext, useState, ReactNode } from 'react'

interface %sContextType {
  // 定义 context 类型
}

interface %sProviderProps {
  children: ReactNode
}

const %sContext = createContext<%sContextType | undefined>(undefined)

export const %sProvider: React.FC<%sProviderProps> = ({ children }) => {
  const [state, setState] = useState()

  const value: %sContextType = {
    // context 值
  }

  return (
    <%sContext.Provider value={value}>
      {children}
    </%sContext.Provider>
  )
}

export const use%s = (): %sContextType => {
  const context = useContext(%sContext)
  if (context === undefined) {
    throw new Error('use%s must be used within a %sProvider')
  }
  return context
}
"
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name
                          context-name)
                (format "import React, { createContext, useContext, useState } from 'react'

const %sContext = createContext()

export const %sProvider = ({ children }) => {
  const [state, setState] = useState()

  const value = {
    // context 值
  }

  return (
    <%sContext.Provider value={value}>
      {children}
    </%sContext.Provider>
  )
}

export const use%s = () => {
  const context = useContext(%sContext)
  if (context === undefined) {
    throw new Error('use%s must be used within a %sProvider')
  }
  return context
}
"
                        context-name
                        context-name
                        context-name
                        context-name
                        context-name
                        context-name
                        context-name
                        context-name))))
    
    (find-file context-path)
    (message "已创建 Context: %s" context-path)))

;; 工具安装
(defun +react/install-create-react-app ()
  "安装 Create React App"
  (interactive)
  (compile "npm install -g create-react-app"))

(defun +react/install-nextjs ()
  "安装 Next.js CLI"
  (interactive)
  (compile "npm install -g create-next-app"))

(defun +react/install-expo ()
  "安装 Expo CLI"
  (interactive)
  (compile "npm install -g @expo/cli"))

(defun +react/install-dev-tools ()
  "安装开发工具"
  (interactive)
  (let ((tools '("create-react-app" "create-next-app" "@expo/cli" "gatsby-cli" 
                "prettier" "eslint" "@typescript-eslint/parser" "jest")))
    (compile (format "npm install -g %s" (mapconcat 'identity tools " ")))))

(use-package! rjsx-mode
  :mode ("\\.jsx\\'" "\\.js\\'")
  :config
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +react-lsp-clients nil)
  
  ;; React 模式钩子
  (add-hook 'rjsx-mode-hook
            (lambda ()
              ;; 启用 lsp-bridge（如果全局未启用）
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              ;; 启用 emmet
              (emmet-mode 1)
              ;; 添加 node_modules 到路径
              (add-node-modules-path)
              ;; 设置缩进
              (setq tab-width 2)
              (setq js-indent-level 2)
              (setq sgml-basic-offset 2)
              ;; 启用自动格式化
              (when (executable-find "prettier")
                (add-hook 'before-save-hook 'prettier-js nil t)))))

(use-package! typescript-mode
  :mode ("\\.tsx\\'" "\\.ts\\'")
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              (emmet-mode 1)
              (add-node-modules-path)
              (setq typescript-indent-level 2)
              (when (executable-find "prettier")
                (add-hook 'before-save-hook 'prettier-js nil t)))))

(use-package! web-mode
  :mode ("\\.html\\'" "\\.jsx\\'" "\\.tsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package! emmet-mode
  :hook (rjsx-mode typescript-mode web-mode html-mode css-mode))

(use-package! prettier-js
  :after rjsx-mode
  :config
  (setq prettier-js-args '("--single-quote" "--trailing-comma" "es5" "--semi" "false")))

(use-package! add-node-modules-path
  :hook (rjsx-mode typescript-mode js-mode))

(use-package! npm-mode
  :hook (rjsx-mode typescript-mode js-mode))

(use-package! jest
  :after rjsx-mode)

(use-package! js2-refactor
  :after rjsx-mode
  :config
  (add-hook 'rjsx-mode-hook #'js2-refactor-mode))

;; 设置 React 相关的键绑定
(map! :after rjsx-mode
      :localleader
      :map rjsx-mode-map
      ;; 项目管理
      (:prefix ("p" . "project")
       "r" #'+react/create-react-app
       "v" #'+react/create-vite-react
       "n" #'+react/create-nextjs-app
       "e" #'+react/create-expo-app
       "g" #'+react/create-gatsby-site)
      
      ;; 开发服务器
      (:prefix ("s" . "server")
       "s" #'+react/dev-server
       "b" #'+react/build-project
       "S" #'+react/start-storybook
       "B" #'+react/build-storybook
       "e" #'+react/export-static)
      
      ;; 代码质量
      (:prefix ("l" . "lint")
       "l" #'+react/lint-project
       "f" #'+react/lint-fix
       "F" #'+react/format-buffer
       "P" #'+react/format-project)
      
      ;; 测试
      (:prefix ("t" . "test")
       "t" #'+react/test-run
       "w" #'+react/test-watch
       "c" #'+react/test-coverage
       "f" #'+react/test-current-file
       "j" #'+react/jest-run)
      
      ;; 包管理
      (:prefix ("n" . "npm")
       "i" #'+react/install-dependencies
       "a" #'+react/add-dependency
       "d" #'+react/add-dev-dependency
       "r" #'+react/remove-dependency
       "u" #'+react/update-dependencies
       "A" #'+react/audit-dependencies
       "f" #'+react/audit-fix)
      
      ;; 代码生成
      (:prefix ("g" . "generate")
       "c" #'+react/create-component
       "h" #'+react/create-hook
       "x" #'+react/create-context)
      
      ;; 工具
      (:prefix ("x" . "tools")
       "r" #'+react/install-create-react-app
       "n" #'+react/install-nextjs
       "e" #'+react/install-expo
       "i" #'+react/install-dev-tools))

;; TypeScript 模式也使用相同的键绑定
(map! :after typescript-mode
      :localleader
      :map typescript-mode-map
      ;; 项目管理
      (:prefix ("p" . "project")
       "r" #'+react/create-react-app
       "v" #'+react/create-vite-react
       "n" #'+react/create-nextjs-app
       "e" #'+react/create-expo-app
       "g" #'+react/create-gatsby-site)
      
      ;; 开发服务器
      (:prefix ("s" . "server")
       "s" #'+react/dev-server
       "b" #'+react/build-project
       "S" #'+react/start-storybook
       "B" #'+react/build-storybook
       "e" #'+react/export-static)
      
      ;; 代码质量
      (:prefix ("l" . "lint")
       "l" #'+react/lint-project
       "f" #'+react/lint-fix
       "F" #'+react/format-buffer
       "P" #'+react/format-project)
      
      ;; 测试
      (:prefix ("t" . "test")
       "t" #'+react/test-run
       "w" #'+react/test-watch
       "c" #'+react/test-coverage
       "f" #'+react/test-current-file
       "j" #'+react/jest-run)
      
      ;; 包管理
      (:prefix ("n" . "npm")
       "i" #'+react/install-dependencies
       "a" #'+react/add-dependency
       "d" #'+react/add-dev-dependency
       "r" #'+react/remove-dependency
       "u" #'+react/update-dependencies
       "A" #'+react/audit-dependencies
       "f" #'+react/audit-fix)
      
      ;; 代码生成
      (:prefix ("g" . "generate")
       "c" #'+react/create-component
       "h" #'+react/create-hook
       "x" #'+react/create-context)
      
      ;; 工具
      (:prefix ("x" . "tools")
       "r" #'+react/install-create-react-app
       "n" #'+react/install-nextjs
       "e" #'+react/install-expo
       "i" #'+react/install-dev-tools))

;; 设置环境变量
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("NODE_ENV" "REACT_APP_API_URL" "NEXT_PUBLIC_API_URL")))

;; Transient 菜单定义
(use-package! transient
  :after rjsx-mode
  :config
  (transient-define-prefix +react/transient-menu ()
    "React 开发菜单"
    ["项目创建"
     ("pr" "Create React App" +react/create-react-app)
     ("pv" "Vite + React" +react/create-vite-react)
     ("pn" "Next.js" +react/create-nextjs-app)
     ("pe" "Expo (React Native)" +react/create-expo-app)
     ("pg" "Gatsby" +react/create-gatsby-site)]
    ["开发服务器"
     ("ss" "启动开发服务器" +react/dev-server)
     ("sb" "构建项目" +react/build-project)
     ("sS" "启动 Storybook" +react/start-storybook)
     ("sB" "构建 Storybook" +react/build-storybook)
     ("se" "导出静态文件" +react/export-static)]
    ["代码质量"
     ("ll" "代码检查" +react/lint-project)
     ("lf" "自动修复" +react/lint-fix)
     ("lF" "格式化文件" +react/format-buffer)
     ("lP" "格式化项目" +react/format-project)]
    ["测试"
     ("tt" "运行测试" +react/test-run)
     ("tw" "监视测试" +react/test-watch)
     ("tc" "测试覆盖率" +react/test-coverage)
     ("tf" "测试当前文件" +react/test-current-file)]
    ["包管理"
     ("ni" "安装依赖" +react/install-dependencies)
     ("na" "添加依赖" +react/add-dependency)
     ("nd" "添加开发依赖" +react/add-dev-dependency)
     ("nr" "移除依赖" +react/remove-dependency)
     ("nu" "更新依赖" +react/update-dependencies)]
    ["代码生成"
     ("gc" "创建组件" +react/create-component)
     ("gh" "创建 Hook" +react/create-hook)
     ("gx" "创建 Context" +react/create-context)]
    ["工具"
     ("xi" "安装开发工具" +react/install-dev-tools)
     ("xr" "安装 CRA" +react/install-create-react-app)
     ("xn" "安装 Next.js" +react/install-nextjs)]
    ["退出"
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after rjsx-mode
        :localleader
        :map rjsx-mode-map
        "m" #'+react/transient-menu)
  
  (map! :after typescript-mode
        :localleader
        :map typescript-mode-map
        "m" #'+react/transient-menu))