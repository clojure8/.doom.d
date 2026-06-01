;;; my/web/config.el -*- lexical-binding: t; -*-

;; 自定义 Web 开发模块。
;;
;; 补全后端统一走 lsp-bridge（见 my/lsp-bridge），本模块只负责把各类前端文件
;; 正确地映射到对应的 language server，并补齐 lsp-bridge 默认配置里缺失的部分：
;;
;;   文件类型        major-mode              language server (lsp-bridge)
;;   --------------  ----------------------  ----------------------------------
;;   .html .htm      web-mode                html_emmet（html LS + emmet-ls）
;;   .css            css-mode/css-ts-mode    vscode-css-language-server（内置）
;;   .scss .sass     scss-mode/sass-mode     vscode-css-language-server
;;   .less           less-css-mode           vscode-css-language-server
;;   .js .mjs .cjs   js-mode/js-ts-mode      javascript（内置）
;;   .jsx            rjsx-mode/js-jsx-mode   javascriptreact（React）
;;   .ts .mts        typescript-ts-mode      typescript_eslint（内置）
;;   .tsx            tsx-ts-mode             typescriptreact_eslint（React，内置）
;;   .vue            web-mode                volar_vtsls_emmet（volar+vtsls+emmet）
;;
;; 依赖的 language server（需在 PATH 中可用）：
;;   vscode-html-language-server / vscode-css-language-server
;;   typescript-language-server / vue-language-server (volar) / vtsls
;;   vscode-eslint-language-server / emmet-ls
;; 缺失 emmet-ls 时本模块会自动回退到不带 emmet 的纯 LS 配置。

;; ---------------------------------------------------------------------------
;; Vue（volar 3.x）正确接线
;; ---------------------------------------------------------------------------
;; volar 3.x 只负责 .vue 的「模板/样式」；脚本(TS/JS)区域的补全、跳转、诊断已
;; 全部移交给「带 @vue/typescript-plugin 的 tsserver」。lsp-bridge 自带的
;; .vue -> volar_emmet 里只有 volar，没有 tsserver，所以脚本区毫无补全（这正是
;; “Vue 不正常”的根因）；而它自带的 vtsls.json 又把插件路径写死成
;; /opt/homebrew/...（本机经 mise 安装，路径不符）。
;;
;; 这里据「实际可执行文件」解析出真实路径，动态生成正确的 vtsls.json / volar.json
;; 和组合 multiserver(volar+vtsls+emmet)，写入缓存目录并让 lsp-bridge 优先采用，
;; 再把 .vue 指过去。好处：随 node 版本/安装位置自动更新，升级 lsp-bridge 不丢。

(defun +web--node-pkg-dir (exe pkg)
  "由可执行文件 EXE 解析出其所属 npm 包 PKG 的目录（解析软链后按子串截取）。"
  (when-let* ((bin (executable-find exe))
              (real (file-truename bin))
              (idx (string-search pkg real)))
    (substring real 0 (+ idx (length pkg)))))

(defun +web--tsdk-path ()
  "TypeScript lib 目录（tsserver 的 tsdk）；找不到 tsc 返回 nil。"
  (when-let* ((tsc (executable-find "tsc"))
              (dir (expand-file-name "../lib" (file-name-directory (file-truename tsc))))
              ((file-directory-p dir)))
    (directory-file-name dir)))

(defun +web/setup-vue-lsp ()
  "动态生成并启用 volar+vtsls(+emmet) 的 Vue LSP 配置。
缺 vtsls 或 vue-language-server 时静默跳过（保留 lsp-bridge 内置 volar_emmet 兜底）。"
  (let ((vue-loc (+web--node-pkg-dir "vue-language-server" "@vue/language-server"))
        (tsdk (or (+web--tsdk-path) ""))
        (emmet (executable-find "emmet-ls")))
    (when (and vue-loc (executable-find "vtsls"))
      (let* ((gen   (expand-file-name "lsp-bridge-web/" doom-cache-dir))
             (lsdir (expand-file-name "langserver/" gen))
             (msdir (expand-file-name "multiserver/" gen))
             (srvs  (if emmet "[\"volar\", \"vtsls\", \"emmet-ls\"]"
                      "[\"volar\", \"vtsls\"]")))
        (make-directory lsdir t)
        (make-directory msdir t)
        ;; vtsls.json：把 @vue/typescript-plugin 指到本机真实路径 + 正确 tsdk
        (with-temp-file (expand-file-name "vtsls.json" lsdir)
          (insert (format "{
  \"name\": \"vtsls\",
  \"languageId\": \"\",
  \"command\": [\"vtsls\", \"--stdio\"],
  \"settings\": {
    \"vtsls\": {
      \"tsserver\": {
        \"globalPlugins\": [
          {\"name\": \"@vue/typescript-plugin\", \"location\": \"%s\", \"languages\": [\"vue\"], \"configNamespace\": \"typescript\"}
        ]
      }
    }
  },
  \"initializationOptions\": {\"typescript\": {\"tsdk\": \"%s\"}}
}" vue-loc tsdk)))
        ;; volar.json：现代格式，模板/样式由 volar 负责，TS 交给插件（hybrid 默认）
        (with-temp-file (expand-file-name "volar.json" lsdir)
          (insert (format "{
  \"name\": \"volar\",
  \"languageId\": \"vue\",
  \"command\": [\"vue-language-server\", \"--stdio\"],
  \"projectFiles\": [\"package.json\"],
  \"settings\": {},
  \"initializationOptions\": {\"typescript\": {\"tsdk\": \"%s\"}}
}" tsdk)))
        ;; 组合 multiserver：volar(模板) + vtsls(脚本) + emmet(缩写)
        (with-temp-file (expand-file-name "volar_vtsls_emmet.json" msdir)
          (insert (format "{
  \"default\": \"vtsls\",
  \"servers\": %s,
  \"completion\": %s,
  \"completion_item_resolve\": %s,
  \"diagnostics\": [\"volar\", \"vtsls\"],
  \"code_action\": [\"volar\", \"vtsls\"],
  \"hover\": \"volar\"
}" srvs srvs srvs)))
        (setq lsp-bridge-user-langserver-dir lsdir
              lsp-bridge-user-multiserver-dir msdir)
        ;; .vue -> 组合 multiserver（先移除内置 volar_emmet 映射，再前插覆盖）
        (setq lsp-bridge-multi-lang-server-extension-list
              (cl-remove-if (lambda (rule) (member "vue" (car rule)))
                            lsp-bridge-multi-lang-server-extension-list))
        (add-to-list 'lsp-bridge-multi-lang-server-extension-list
                     '(("vue") . "volar_vtsls_emmet"))))))

(after! lsp-bridge
  ;; lsp-bridge 默认只为 `lsp-bridge-default-mode-hooks' 里列出的 major-mode 打开
  ;; lsp-bridge-mode。前端这几个 mode 不在默认列表里，补上，否则打开对应文件不会
  ;; 自动补全。
  (dolist (hook '(mhtml-mode-hook
                  html-mode-hook
                  html-ts-mode-hook
                  scss-mode-hook
                  sass-mode-hook
                  less-css-mode-hook
                  js-jsx-mode-hook
                  vue-mode-hook
                  vue-ts-mode-hook))
    (add-to-list 'lsp-bridge-default-mode-hooks hook))

  ;; 按扩展名补全/覆盖 language server 选择。
  ;; lsp-bridge 解析优先级：扩展名 > major-mode，且列表中靠前者优先，
  ;; 因此用 add-to-list 前插即可覆盖内置默认值。
  (let ((emmet (executable-find "emmet-ls")))
    ;; HTML：装了 emmet-ls 就用 html_emmet（html LS + emmet 缩写补全），
    ;; 否则退回纯 html LS。
    (if emmet
        (add-to-list 'lsp-bridge-multi-lang-server-extension-list
                     '(("html" "htm") . "html_emmet"))
      (add-to-list 'lsp-bridge-single-lang-server-extension-list
                   '(("html" "htm") . "vscode-html-language-server"))))

  ;; Vue：见下方 `+web/setup-vue-lsp'。volar 3.x 只做模板/样式，脚本(TS)智能
  ;; 必须交给带 @vue/typescript-plugin 的 tsserver(vtsls)，因此单独处理。
  (+web/setup-vue-lsp)

  ;; React：.jsx 用 javascriptreact（带 JSX 语义），而非默认按 mode 推出的 javascript。
  (add-to-list 'lsp-bridge-single-lang-server-extension-list
               '(("jsx") . "javascriptreact"))

  ;; SCSS / Sass / Less 复用 css LS。
  (add-to-list 'lsp-bridge-single-lang-server-extension-list
               '(("scss" "sass" "less") . "vscode-css-language-server"))

  ;; ESM/CJS 扩展名归到 javascript。
  (add-to-list 'lsp-bridge-single-lang-server-extension-list
               '(("mjs" "cjs") . "javascript")))

;; web-mode 体验优化（.html/.vue 等都走 web-mode）。
(after! web-mode
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting nil      ; 关掉 ="" 自动补全，避免和 lsp-bridge 打架
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        web-mode-auto-close-style 2))

;; ---------------------------------------------------------------------------
;; web 缩进 = 2 空格（实际偏移在 $DOOMDIR/config.el 的 setq-default 里设为 2）
;; ---------------------------------------------------------------------------
;; Doom 的 `doom-set-indent' 会按 major-mode 的「缩进变量」把 tab-width /
;; standard-indent / evil-shift-width(>> 缩进量) 同步成该变量的值。但它的自动
;; 猜测对 web-mode、tsx-ts-mode、json-ts-mode 会失败（mode 名被过度截断），
;; 导致这些 mode 的 >> 仍按全局 4。这里显式登记其缩进变量，让三者也同步为 2。
;;
;; 时序：`doom-set-indent' 跑在 change-major-mode-after-body-hook(-100)，早于
;; editorconfig（挂在 prog-mode-hook 末尾）。因此存在 .editorconfig 时仍由
;; editorconfig 最终覆盖 —— “有 editorconfig 就按它”的语义不变。
(set-indent-vars! 'web-mode     'web-mode-code-indent-offset)
(set-indent-vars! 'tsx-ts-mode  'tsx-ts-mode-indent-offset)
(set-indent-vars! 'json-ts-mode 'json-ts-mode-indent-offset)

;; ---------------------------------------------------------------------------
;; 实用命令
;; ---------------------------------------------------------------------------

(defun +web/browse-current-file ()
  "在默认浏览器中打开当前 HTML 文件。"
  (interactive)
  (if buffer-file-name
      (browse-url (concat "file://" buffer-file-name))
    (user-error "当前缓冲区没有关联文件")))

(defun +web/npm-run (script)
  "在最近的 package.json 所在目录运行 npm run SCRIPT。"
  (interactive
   (let* ((root (locate-dominating-file default-directory "package.json"))
          (scripts (when root
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "package.json" root))
                       (let ((data (ignore-errors (json-parse-buffer :object-type 'alist))))
                         (when data
                           (mapcar (lambda (s) (symbol-name (car s)))
                                   (alist-get 'scripts data))))))))
     (unless root (user-error "未找到 package.json"))
     (list (completing-read "npm run: " scripts))))
  (let ((default-directory (locate-dominating-file default-directory "package.json")))
    (compile (format "npm run %s" script))))

(defun +web/npm-install ()
  "在最近的 package.json 所在目录运行 npm install。"
  (interactive)
  (let ((root (locate-dominating-file default-directory "package.json")))
    (unless root (user-error "未找到 package.json"))
    (let ((default-directory root))
      (compile "npm install"))))

(defun +web/prettier-format ()
  "用 prettier 格式化当前文件（需项目内或全局可用 npx prettier）。"
  (interactive)
  (if buffer-file-name
      (progn
        (when (buffer-modified-p) (save-buffer))
        (compile (format "npx prettier --write %s"
                         (shell-quote-argument buffer-file-name))))
    (user-error "当前缓冲区没有关联文件")))

;; localleader 键绑定（web/js/ts/css 等前端 major-mode）。
(map! :localleader
      :map (web-mode-map
            js-mode-map
            js-ts-mode-map
            typescript-ts-mode-map
            tsx-ts-mode-map
            css-mode-map
            css-ts-mode-map)
      (:prefix ("w" . "web")
       :desc "浏览器打开当前文件" "o" #'+web/browse-current-file
       :desc "npm run <script>"   "r" #'+web/npm-run
       :desc "npm install"        "i" #'+web/npm-install
       :desc "prettier 格式化"    "f" #'+web/prettier-format))
