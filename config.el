;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; emacsclient -nw 偶尔出现 menu-bar 的兜底修复。
;; Doom 在 macOS 上用 'tty 哨兵延迟初始化 menu-bar，after-make-frame-functions
;; 存在竞态，server-after-make-frame-hook 在 frame 完全就绪后执行，更可靠。
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
              go-ts-mode-indent-offset 4
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
              heex-ts-mode-indent-offset 4
              elixir-ts-mode-indent-offset 4
              nix-ts-mode-indent-offset 4
              php-ts-mode-indent-offset 4
              ;; go-ts-mode-indent-offset 在下方 go-ts-mode-hook 里另设
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

(when (display-graphic-p)
  ;; 避免颜色失真（macOS Cocoa 专用）
  (setq ns-use-srgb-colorspace nil)

  (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))
  ;; (setq doom-font (font-spec :family "霞鹜文楷等宽" :size 14))
  (setq frame-title-format "")

  ;; 设置 ace-window 超大字体
  (custom-set-faces!
    '(aw-leading-char-face
      :foreground "red"
      :weight bold
      :height 400))

  ;; 像素级别平滑滚动
  (pixel-scroll-precision-mode 1)

  (add-to-list 'default-frame-alist '(fullscreen . maximized)))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

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
(map! :leader
      :desc "Execute command" "SPC" #'execute-extended-command)
(map! "s-p" #'switch-to-buffer)


;; doom-one 给 `magit-header-line' 脸设了 3px 蓝色 :box，magit-log 等缓冲区顶部
;; 的 header-line 会因此显示为“四周一圈高亮边框”。这里去掉边框。
(custom-set-faces!
  '(magit-header-line :box nil))

;; 放大 window-select 的提示字体（GUI 下使用像素倍数，TUI 下使用固定绝对高度）
(after! window-select
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'doom-window-select-face nil :height 3.0)
        (set-face-attribute 'doom-window-select-number-face nil :height 3))
    (set-face-attribute 'doom-window-select-face nil :height 200)
    (set-face-attribute 'doom-window-select-number-face nil :height 200)))




(use-package! dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (defun dwim-shell-commands-macos-open-with ()
    "Convert all marked images to jpg(s)."
    (interactive)
    (let* ((apps (seq-sort
                  #'string-lessp
                  (seq-mapcat (lambda (paths)
                                (directory-files-recursively
                                 paths "\\.app$" t (lambda (path)
                                                     (not (string-suffix-p ".app" path)))))
                              '("/Applications" "~/Applications" "/System/Applications"))))
           (selection (progn
                        (cl-assert apps nil "No apps found")
                        (completing-read "Open with: "
                                         (mapcar (lambda (path)
                                                   (propertize (file-name-base path) 'path path))
                                                 apps)))))
      (dwim-shell-command-on-marked-files
       "Open with"
       (format "open -a '%s' '<<*>>'" (get-text-property 0 'path selection))
       :silent-success t
       :no-progress t
       :utils "open"))))

;; treesit-auto: 自动切换到 tree-sitter 版本的 major-mode
;;
;; ⚠️ 性能坑（已修）：treesit-auto 给 `set-auto-mode-0' 挂了 :before advice，
;; 每次打开文件都会遍历所有 recipe 调 `treesit-ready-p' 重建 remap 表。grammar
;; 已装的语言探测很快，但**未装** grammar 的语言在 macOS 上每次 dlopen 探测约
;; 29ms 且不缓存；对 markdown 这类未装 grammar 的类型还会触发 `revert-buffer'
;; 递归，把整套扫描重复 ~3 遍 → 单次开文件阻塞 5~10s（本机未装的 grammar 有 50 个）。
;;
;; 修复：
;;   1. `treesit-auto-install nil' —— 本机网络/代理下 grammar 根本下载不下来，
;;      关掉无谓的安装尝试（缺失就静默回退普通 major-mode）。
;;   2. 启动时把 `treesit-auto-langs' 过滤成「grammar 已安装」的子集 —— 未装的
;;      语言反正用不了 ts-mode，移出 recipe 后开文件不再对它们反复探测。
;; 效果：打开 markdown 等文件从 ~9.8s 降到 ~0.27s。
(use-package! treesit-auto
  :config
  (setq treesit-auto-install nil)
  ;; markdown / markdown-inline：本机未装 grammar，且我们已用 major-mode-remap-alist
  ;; 强制 markdown → gfm-mode（markdown-ts-mode 无标题分级等特性，并不想要），显式
  ;; 排除，避免首次开 .md 时还去探测一次缺失的 markdown-inline grammar。
  (setq treesit-auto-langs
        (seq-filter (lambda (lang)
                      (and (not (memq lang '(markdown markdown-inline)))
                           (treesit-ready-p lang t)))
                    treesit-auto-langs))
  (global-treesit-auto-mode))

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

;; ── doom-modeline 瘦身：高度按字体行高比例（可配置）──────────────────────
;; 改 `+modeline-height-ratio' 调高度：1.0=与字体等高（最瘦），1.1~1.3 留点留白。
;; 改后 `M-x +modeline/apply-height' 即时生效（或重启）。
(defcustom +modeline-height-ratio 1.0
  "doom-modeline 高度相对字体行高 `frame-char-height' 的倍数。"
  :type 'number
  :group 'doom-modeline
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp '+modeline/apply-height) (+modeline/apply-height))))

(defun +modeline/apply-height (&rest _)
  "按 `+modeline-height-ratio' 把 doom-modeline 高度设成字体行高的比例。"
  (interactive)
  (when (and (display-graphic-p) (boundp 'doom-modeline-height))
    (setq doom-modeline-height
          (max 1 (round (* +modeline-height-ratio (frame-char-height)))))
    (when (fboundp 'doom-modeline-refresh-bars)
      (doom-modeline-refresh-bars))))

(after! doom-modeline
  (+modeline/apply-height))
;; 字体 / 主题 / 新建 frame（daemon 首帧）变化时重算高度
(add-hook 'after-setting-font-hook      #'+modeline/apply-height)
(add-hook 'doom-load-theme-hook         #'+modeline/apply-height)
(add-hook 'server-after-make-frame-hook #'+modeline/apply-height)

;; ── 小工具集成：大文件 / grep 批量编辑 / 跳转历史 / 可视化 undo / 快速切目录 ──
(use-package! vlf
  :commands (vlf vlf-mode)
  :init
  (map! :leader
        :desc "Open large file with VLF" "f V" #'vlf))

(use-package! wgrep
  :commands (wgrep-change-to-wgrep-mode)
  :init
  (map! :after grep
        :map grep-mode-map
        :n "e" #'wgrep-change-to-wgrep-mode))

(use-package! dogears
  :commands (dogears-back dogears-forward dogears-list dogears-go dogears-mode)
  :hook (doom-first-buffer . dogears-mode)
  :init
  (map! :leader
        (:prefix ("j" . "jump")
         :desc "Dogears back"    "b" #'dogears-back
         :desc "Dogears forward" "f" #'dogears-forward
         :desc "Dogears list"    "l" #'dogears-list)))

(use-package! vundo
  :commands vundo
  :init
  (map! :leader
        (:prefix ("o" . "open")
         :desc "Visual undo" "u" #'vundo)))

(use-package! consult-dir
  :commands (consult-dir consult-dir-jump-file)
  :init
  (map! :leader
        :desc "Switch directory" "f D" #'consult-dir
        :map minibuffer-local-completion-map
        "C-x C-d" #'consult-dir
        "C-x C-j" #'consult-dir-jump-file))

