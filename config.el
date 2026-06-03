;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; ── GUI 外观：放「顶层」而非 server-after-make-frame-hook ───────────────────
;; 关键：default-frame-alist 和这些全局变量必须在「配置加载时」就绪，frame 创建时
;; 才会被读取——这样 daemon 的第一个 emacsclient frame 也带上。
;; 若塞进 server-after-make-frame-hook，hook 在 frame 建好「之后」才跑，等于马后炮：
;;   · 首帧的 default-frame-alist 类参数（尤其 fullscreen）根本来不及生效；
;;   · NS 透明标题栏这类即便事后 set-frame-parameter 补设也刷不彻底（要重建 frame）。
;; 这些 NS 变量/参数在 tty 下被忽略，无副作用，所以无需 display-graphic-p 守卫。
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
      frame-title-format ""              ; 标题栏不显示标题文字
      icon-title-format ""
      ns-use-srgb-colorspace nil         ; 避免颜色失真（macOS Cocoa）
      ns-use-proxy-icon nil)             ; 去掉标题栏左侧的文件代理小图标

;; frame 参数：开机即进 default-frame-alist，首帧创建时直接读取（idempotent）
;;   fullscreen=maximized        —— 启动即最大化
;;   ns-transparent-titlebar=t   —— 标题栏与背景融合（仍可拖动、交通灯还在）
;;   ns-appearance=dark          —— 配 doom-one 深色主题；换浅色主题改 light
(dolist (p '((fullscreen . maximized)
             (ns-transparent-titlebar . t)
             (ns-appearance . dark)))
  (add-to-list 'default-frame-alist p))

;; 已存在的 GUI frame 也补设一遍：
;;   · 直接启动 Emacs.app（非 daemon）时，初始 frame 在 config 加载「之前」就建好了，
;;     default-frame-alist 对它来不及，这里直接 modify-frame-parameters 补上。
;;   · daemon 模式下此刻没有 GUI frame，循环为空，由 default-frame-alist 接管首帧。
;; （ns-transparent-titlebar 对已存在 frame 可能要新建 frame 才彻底刷新，属 macOS 限制。）
(dolist (f (frame-list))
  (when (display-graphic-p f)
    (modify-frame-parameters
     f '((fullscreen . maximized)
         (ns-transparent-titlebar . t)
         (ns-appearance . dark)))))

(custom-set-faces!
  '(aw-leading-char-face :foreground "red" :weight bold :height 400))

;; 像素级平滑滚动（全局开启；tty 下无副作用）
(pixel-scroll-precision-mode 1)

;; ── tty menu-bar 兜底：这个才真正需要 per-frame hook ────────────────────────
;; emacsclient -nw 偶尔出现 menu-bar；Doom 在 macOS 用 'tty 哨兵延迟初始化，
;; after-make-frame-functions 有竞态，server-after-make-frame-hook 在 frame 就绪后更可靠。
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (unless (display-graphic-p)
              (set-frame-parameter nil 'menu-bar-lines 0))))

;; fix mode line rendering artifacts
(setq gc-cons-threshold (* 20 1024 1024))
(setq gc-cons-percentage 0.1)
;; 渲染优化
(setq redisplay-skip-fontification-on-input t)
(setq auto-window-vscroll nil)


;; 文件大小限制优化
(setq large-file-warning-threshold (* 500 1000 1000))  ; 500MB

;; 全局缩进：tab = 4 空格
;; tab-width 只影响真实 \t 字符的显示宽度；按 TAB 键的实际缩进量由各 mode 自己的
;; *-offset / *-indent-level 变量决定，所以这里一并设默认值。
(setq-default tab-width 4
              indent-tabs-mode nil
              evil-shift-width 4
              standard-indent 4
              ;; C / C++ / Java / Objective-C / Awk 等 cc-mode 家族
              c-basic-offset 4
              ;; Shell
              sh-basic-offset 4
              sh-indentation 4
              ;; JavaScript / TypeScript / JSON（web：2 空格）
              js-indent-level 2
              js-switch-indent-offset 2
              js2-basic-offset 2
              typescript-indent-level 2
              json-reformat:indent-width 2
              ;; Python
              python-indent-offset 4
              ;; CSS / SCSS / Less（web：2 空格）
              css-indent-offset 2
              ;; web-mode（html/vue/jsx：2 空格）
              web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-attr-indent-offset 2
              ;; Lua / Rust / Ruby
              lua-indent-level 4
              rust-indent-offset 4
              ruby-indent-level 4
              ;; XML（非 web，保持 4）
              nxml-child-indent 4
              nxml-attribute-indent 4
              ;; SGML / HTML（html-mode、js-jsx-mode：web 2 空格）
              sgml-basic-offset 2
              ;; tree-sitter *-ts-mode 各自独立的 offset
              c-ts-mode-indent-offset 4
              c-ts-common-indent-offset 4
              ;; TypeScript / TSX / JSON（web：2 空格）
              typescript-ts-mode-indent-offset 2
              tsx-ts-mode-indent-offset 2
              rust-ts-mode-indent-offset 4
              ruby-ts-mode-indent-offset 4
              lua-ts-mode-indent-offset 4
              toml-ts-mode-indent-offset 4
              json-ts-mode-indent-offset 2
              yaml-ts-mode-indent-offset 4
              dockerfile-ts-mode-indent-offset 4
              cmake-ts-mode-indent-offset 4
              ;; go-ts-mode-indent-offset 由 golang 模块的 hook 另设
              ;; （Go 用真实 tab，indent-tabs-mode t）。
              ;; bash-ts-mode / js-ts-mode / python-ts-mode 复用上面的
              ;; sh-basic-offset / js-indent-level / python-indent-offset
              )


;; 快速滚动
(setq fast-but-imprecise-scrolling t)
;; 减少滚动跳动
(setq scroll-conservatively 101)
;; 鼠标滚轮优化
(setq mouse-wheel-scroll-amount '(1 ((control) . 5)))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; 修改默认快捷键
;; 故意让 SPC 直接执行 M-x，而非打开 which-key 菜单。
;; 如需 which-key 前缀菜单，改用 SPC : 或恢复默认 SPC SPC。
(map! :leader
      :desc "Execute command" "SPC" #'execute-extended-command)
(map! "s-p" #'switch-to-buffer)


;; doom-one 给 `magit-header-line' 脸设了 3px 蓝色 :box，magit-log 等缓冲区顶部
;; 的 header-line 会因此显示为"四周一圈高亮边框"。这里去掉边框。
(after! magit
  (custom-set-faces!
    '(magit-header-line :box nil)))

;; 放大 window-select 的提示字体（GUI 下使用像素倍数，TUI 下使用固定绝对高度）
(after! window-select
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'doom-window-select-face nil :height 3.0)
        (set-face-attribute 'doom-window-select-number-face nil :height 3))
    (set-face-attribute 'doom-window-select-face nil :height 200)
    (set-face-attribute 'doom-window-select-number-face nil :height 200)))



;; 范围高亮
(setq show-paren-style 'expression
      show-paren-delay 0
      show-paren-when-point-inside-paren t)

;; prose 软换行：Doom 的 word-wrap 提供「保留缩进、语言感知」的视觉折行，
;; 默认不全局开启。这里开启 +global-word-wrap-mode，覆盖 text/markdown/org/rst
;; 等 prose 模式（见 `+word-wrap-text-modes'）；prog-mode 默认不受影响。
(when (modulep! :editor word-wrap)
  (add-hook 'doom-after-init-hook #'+global-word-wrap-mode))

;; breadcrumb：header-line 显示「项目 › 文件 › 当前函数/命名空间（imenu 路径）」。
;; 默认不自动开启（不挂 hook），需要时手动 `M-x breadcrumb-local-mode'（当前 buffer）
;; 或 `M-x breadcrumb-mode'（全局）。
(use-package! breadcrumb
  :commands (breadcrumb-local-mode breadcrumb-mode))

;; Info 手册彩色化（变量/函数/键位等不同着色，更易读）
(use-package! info-colors
  :hook (Info-selection . info-colors-fontify-node))

;; 把分页符 ^L 渲染成一条横线（C-x [ / C-x ] 按页跳转更直观）
(use-package! page-break-lines
  :config (global-page-break-lines-mode))

