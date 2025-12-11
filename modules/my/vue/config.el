;;; my/vue/config.el -*- lexical-binding: t; -*-

;; Vue 项目管理函数
(defun +vue/create-vue2-project ()
  "创建 Vue 2 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "vue create %s" project-name)))))

(defun +vue/create-vue3-project ()
  "创建 Vue 3 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "npm create vue@latest %s" project-name)))))

(defun +vue/create-nuxt-project ()
  "创建 Nuxt.js 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "npx nuxi@latest init %s" project-name)))))

(defun +vue/create-vite-project ()
  "创建 Vite + Vue 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (template (completing-read "模板: " '("vue" "vue-ts" "vue-pwa" "vue-pwa-ts")))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "npm create vite@latest %s -- --template %s" project-name template)))))

(defun +vue/create-quasar-project ()
  "创建 Quasar 项目"
  (interactive)
  (let ((project-name (read-string "项目名: "))
        (project-dir (read-directory-name "项目目录: ")))
    (let ((default-directory project-dir))
      (compile (format "npm create quasar %s" project-name)))))

;; 开发服务器管理
(defun +vue/dev-server ()
  "启动开发服务器"
  (interactive)
  (let ((command (cond
                  ((file-exists-p "package.json")
                   (let ((scripts (with-temp-buffer
                                   (insert-file-contents "package.json")
                                   (goto-char (point-min))
                                   (when (search-forward "\"scripts\"" nil t)
                                     (buffer-substring-no-properties (point) (line-end-position))))))
                     (cond
                      ((string-match "\"dev\"" scripts) "npm run dev")
                      ((string-match "\"serve\"" scripts) "npm run serve")
                      (t "npm start"))))
                  (t "npm run dev"))))
    (compile command)))

(defun +vue/build-project ()
  "构建项目"
  (interactive)
  (compile "npm run build"))

(defun +vue/build-preview ()
  "构建并预览"
  (interactive)
  (compile "npm run build && npm run preview"))

(defun +vue/lint-project ()
  "代码检查"
  (interactive)
  (compile "npm run lint"))

(defun +vue/lint-fix ()
  "自动修复代码问题"
  (interactive)
  (compile "npm run lint -- --fix"))

(defun +vue/test-unit ()
  "运行单元测试"
  (interactive)
  (compile "npm run test:unit"))

(defun +vue/test-e2e ()
  "运行端到端测试"
  (interactive)
  (compile "npm run test:e2e"))

(defun +vue/test-watch ()
  "监视模式运行测试"
  (interactive)
  (compile "npm run test -- --watch"))

;; 包管理
(defun +vue/install-dependencies ()
  "安装依赖"
  (interactive)
  (compile "npm install"))

(defun +vue/add-dependency ()
  "添加依赖"
  (interactive)
  (let ((package (read-string "包名: ")))
    (compile (format "npm install %s" package))))

(defun +vue/add-dev-dependency ()
  "添加开发依赖"
  (interactive)
  (let ((package (read-string "开发依赖包名: ")))
    (compile (format "npm install --save-dev %s" package))))

(defun +vue/remove-dependency ()
  "移除依赖"
  (interactive)
  (let ((package (read-string "要移除的包名: ")))
    (compile (format "npm uninstall %s" package))))

(defun +vue/update-dependencies ()
  "更新依赖"
  (interactive)
  (compile "npm update"))

(defun +vue/audit-dependencies ()
  "安全审计"
  (interactive)
  (compile "npm audit"))

(defun +vue/audit-fix ()
  "自动修复安全问题"
  (interactive)
  (compile "npm audit fix"))

;; Vue 组件生成
(defun +vue/create-component ()
  "创建 Vue 组件"
  (interactive)
  (let* ((component-name (read-string "组件名: "))
         (component-dir (read-directory-name "组件目录: " "src/components/"))
         (use-typescript (y-or-n-p "使用 TypeScript? "))
         (use-composition-api (y-or-n-p "使用 Composition API? "))
         (file-extension (if use-typescript ".vue" ".vue"))
         (component-path (expand-file-name (concat component-name file-extension) component-dir)))
    
    (make-directory component-dir t)
    
    (with-temp-file component-path
      (insert (format "<template>
  <div class=\"%s\">
    <h1>{{ title }}</h1>
  </div>
</template>

<script%s>
%s
</script>

<style scoped>
.%s {
  /* 组件样式 */
}
</style>
"
                      (downcase component-name)
                      (if use-typescript " lang=\"ts\"" "")
                      (if use-composition-api
                          (if use-typescript
                              (format "import { defineComponent, ref } from 'vue'

export default defineComponent({
  name: '%s',
  setup() {
    const title = ref('%s Component')
    
    return {
      title
    }
  }
})" component-name component-name)
                            (format "import { ref } from 'vue'

export default {
  name: '%s',
  setup() {
    const title = ref('%s Component')
    
    return {
      title
    }
  }
}" component-name component-name))
                        (if use-typescript
                            (format "import { defineComponent } from 'vue'

export default defineComponent({
  name: '%s',
  data() {
    return {
      title: '%s Component'
    }
  }
})" component-name component-name)
                          (format "export default {
  name: '%s',
  data() {
    return {
      title: '%s Component'
    }
  }
}" component-name component-name)))
                      (downcase component-name))))
    
    (find-file component-path)
    (message "已创建组件: %s" component-path)))

(defun +vue/create-page ()
  "创建 Vue 页面"
  (interactive)
  (let* ((page-name (read-string "页面名: "))
         (page-dir (read-directory-name "页面目录: " "src/views/"))
         (use-typescript (y-or-n-p "使用 TypeScript? "))
         (file-extension ".vue")
         (page-path (expand-file-name (concat page-name file-extension) page-dir)))
    
    (make-directory page-dir t)
    
    (with-temp-file page-path
      (insert (format "<template>
  <div class=\"%s-page\">
    <h1>%s Page</h1>
    <p>Welcome to the %s page!</p>
  </div>
</template>

<script%s>
export default {
  name: '%sPage',
  data() {
    return {
      // 页面数据
    }
  },
  mounted() {
    // 页面挂载后的逻辑
  }
}
</script>

<style scoped>
.%s-page {
  padding: 20px;
}
</style>
"
                      (downcase page-name)
                      page-name
                      page-name
                      (if use-typescript " lang=\"ts\"" "")
                      page-name
                      (downcase page-name))))
    
    (find-file page-path)
    (message "已创建页面: %s" page-path)))

(defun +vue/create-store ()
  "创建 Vuex/Pinia store"
  (interactive)
  (let* ((store-name (read-string "Store 名: "))
         (store-type (completing-read "Store 类型: " '("vuex" "pinia")))
         (use-typescript (y-or-n-p "使用 TypeScript? "))
         (store-dir (read-directory-name "Store 目录: " "src/store/"))
         (file-extension (if use-typescript ".ts" ".js"))
         (store-path (expand-file-name (concat store-name file-extension) store-dir)))
    
    (make-directory store-dir t)
    
    (with-temp-file store-path
      (insert (if (string= store-type "pinia")
                  (if use-typescript
                      (format "import { defineStore } from 'pinia'

export const use%sStore = defineStore('%s', {
  state: () => ({
    // state 定义
  }),
  
  getters: {
    // getters 定义
  },
  
  actions: {
    // actions 定义
  }
})
" (capitalize store-name) store-name)
                    (format "import { defineStore } from 'pinia'

export const use%sStore = defineStore('%s', {
  state: () => ({
    // state 定义
  }),
  
  getters: {
    // getters 定义
  },
  
  actions: {
    // actions 定义
  }
})
" (capitalize store-name) store-name))
                (if use-typescript
                    (format "export interface %sState {
  // state 类型定义
}

const state: %sState = {
  // 初始状态
}

const mutations = {
  // mutations 定义
}

const actions = {
  // actions 定义
}

const getters = {
  // getters 定义
}

export default {
  namespaced: true,
  state,
  mutations,
  actions,
  getters
}
" (capitalize store-name) (capitalize store-name))
                  (format "const state = {
  // 初始状态
}

const mutations = {
  // mutations 定义
}

const actions = {
  // actions 定义
}

const getters = {
  // getters 定义
}

export default {
  namespaced: true,
  state,
  mutations,
  actions,
  getters
}
")))))
    
    (find-file store-path)
    (message "已创建 %s store: %s" store-type store-path)))

;; 代码格式化
(defun +vue/format-buffer ()
  "格式化当前缓冲区"
  (interactive)
  (cond
   ((executable-find "prettier") (prettier-js))
   (t (message "未找到 prettier"))))

(defun +vue/format-project ()
  "格式化整个项目"
  (interactive)
  (compile "npx prettier --write ."))

;; 工具安装
(defun +vue/install-vue-cli ()
  "安装 Vue CLI"
  (interactive)
  (compile "npm install -g @vue/cli"))

(defun +vue/install-vite ()
  "安装 Vite"
  (interactive)
  (compile "npm install -g create-vite"))

(defun +vue/install-nuxt ()
  "安装 Nuxt CLI"
  (interactive)
  (compile "npm install -g nuxi"))

(defun +vue/install-dev-tools ()
  "安装开发工具"
  (interactive)
  (let ((tools '("@vue/cli" "vite" "nuxi" "prettier" "eslint" "@typescript-eslint/parser")))
    (compile (format "npm install -g %s" (mapconcat 'identity tools " ")))))

(use-package! vue-mode
  :mode "\\.vue\\'"
  :config
  ;; 禁用 Doom 默认的 LSP 配置，使用 lsp-bridge
  (setq +vue-lsp-clients nil)
  
  ;; Vue 模式钩子
  (add-hook 'vue-mode-hook
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
              (setq css-indent-offset 2)
              ;; 启用自动格式化
              (when (executable-find "prettier")
                (add-hook 'before-save-hook 'prettier-js nil t)))))

(use-package! vue-html-mode
  :after vue-mode)

(use-package! typescript-mode
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-mode-hook
            (lambda ()
              (unless (bound-and-true-p lsp-bridge-mode)
                (lsp-bridge-mode 1))
              (setq typescript-indent-level 2))))

(use-package! web-mode
  :mode ("\\.html\\'" "\\.jsx\\'" "\\.tsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package! emmet-mode
  :hook (vue-mode web-mode html-mode css-mode))

(use-package! prettier-js
  :after vue-mode
  :config
  (setq prettier-js-args '("--single-quote" "--trailing-comma" "es5")))

(use-package! add-node-modules-path
  :hook (vue-mode js-mode typescript-mode))

(use-package! npm-mode
  :hook (vue-mode js-mode typescript-mode))

;; 设置 Vue 相关的键绑定
(map! :after vue-mode
      :localleader
      :map vue-mode-map
      ;; 项目管理
      (:prefix ("p" . "project")
       "2" #'+vue/create-vue2-project
       "3" #'+vue/create-vue3-project
       "n" #'+vue/create-nuxt-project
       "v" #'+vue/create-vite-project
       "q" #'+vue/create-quasar-project)
      
      ;; 开发服务器
      (:prefix ("s" . "server")
       "s" #'+vue/dev-server
       "b" #'+vue/build-project
       "p" #'+vue/build-preview)
      
      ;; 代码质量
      (:prefix ("l" . "lint")
       "l" #'+vue/lint-project
       "f" #'+vue/lint-fix
       "F" #'+vue/format-buffer
       "P" #'+vue/format-project)
      
      ;; 测试
      (:prefix ("t" . "test")
       "u" #'+vue/test-unit
       "e" #'+vue/test-e2e
       "w" #'+vue/test-watch)
      
      ;; 包管理
      (:prefix ("n" . "npm")
       "i" #'+vue/install-dependencies
       "a" #'+vue/add-dependency
       "d" #'+vue/add-dev-dependency
       "r" #'+vue/remove-dependency
       "u" #'+vue/update-dependencies
       "A" #'+vue/audit-dependencies
       "f" #'+vue/audit-fix)
      
      ;; 代码生成
      (:prefix ("g" . "generate")
       "c" #'+vue/create-component
       "p" #'+vue/create-page
       "s" #'+vue/create-store)
      
      ;; 工具
      (:prefix ("x" . "tools")
       "c" #'+vue/install-vue-cli
       "v" #'+vue/install-vite
       "n" #'+vue/install-nuxt
       "i" #'+vue/install-dev-tools))

;; 设置环境变量
(after! exec-path-from-shell
  (exec-path-from-shell-copy-envs '("NODE_ENV" "VUE_APP_API_URL")))

;; Transient 菜单定义
(use-package! transient
  :after vue-mode
  :config
  (transient-define-prefix +vue/transient-menu ()
    "Vue 开发菜单"
    ["项目创建"
     ("p2" "Vue 2 项目" +vue/create-vue2-project)
     ("p3" "Vue 3 项目" +vue/create-vue3-project)
     ("pn" "Nuxt 项目" +vue/create-nuxt-project)
     ("pv" "Vite 项目" +vue/create-vite-project)
     ("pq" "Quasar 项目" +vue/create-quasar-project)]
    ["开发服务器"
     ("ss" "启动开发服务器" +vue/dev-server)
     ("sb" "构建项目" +vue/build-project)
     ("sp" "构建预览" +vue/build-preview)]
    ["代码质量"
     ("ll" "代码检查" +vue/lint-project)
     ("lf" "自动修复" +vue/lint-fix)
     ("lF" "格式化文件" +vue/format-buffer)
     ("lP" "格式化项目" +vue/format-project)]
    ["测试"
     ("tu" "单元测试" +vue/test-unit)
     ("te" "端到端测试" +vue/test-e2e)
     ("tw" "监视测试" +vue/test-watch)]
    ["包管理"
     ("ni" "安装依赖" +vue/install-dependencies)
     ("na" "添加依赖" +vue/add-dependency)
     ("nd" "添加开发依赖" +vue/add-dev-dependency)
     ("nr" "移除依赖" +vue/remove-dependency)
     ("nu" "更新依赖" +vue/update-dependencies)]
    ["代码生成"
     ("gc" "创建组件" +vue/create-component)
     ("gp" "创建页面" +vue/create-page)
     ("gs" "创建 Store" +vue/create-store)]
    ["工具"
     ("xi" "安装开发工具" +vue/install-dev-tools)
     ("xc" "安装 Vue CLI" +vue/install-vue-cli)
     ("xv" "安装 Vite" +vue/install-vite)]
    ["退出"
     ("q" "退出" transient-quit-one)])

  ;; 添加 transient 菜单键绑定
  (map! :after vue-mode
        :localleader
        :map vue-mode-map
        "m" #'+vue/transient-menu))